#!/usr/bin/env bash
# Publish ticket packages to AUR
# Usage: ./scripts/publish-aur.sh <version> <sha256>
# Requires: AUR_SSH_KEY environment variable

set -euo pipefail

VERSION="${1#v}"
SHA256="$2"
REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"

# Setup SSH for AUR access
setup_ssh() {
    mkdir -p ~/.ssh
    echo "$AUR_SSH_KEY" > ~/.ssh/aur
    chmod 600 ~/.ssh/aur
    cat >> ~/.ssh/config << 'EOF'
Host aur.archlinux.org
  IdentityFile ~/.ssh/aur
  User aur
  StrictHostKeyChecking accept-new
EOF
    ssh-keyscan aur.archlinux.org >> ~/.ssh/known_hosts 2>/dev/null
}

# Parse plugin metadata from script file
# Returns: version|description
parse_plugin_metadata() {
    local plugin_file="$1"
    local version desc
    version=$(grep -m1 '^# tk-plugin-version:' "$plugin_file" 2>/dev/null | sed 's/^# tk-plugin-version: *//' || echo "")
    desc=$(grep -m1 '^# tk-plugin:' "$plugin_file" 2>/dev/null | sed 's/^# tk-plugin: *//' || echo "")
    echo "${version:-$VERSION}|${desc:-No description}"
}

# Generate .SRCINFO using Docker
generate_srcinfo() {
    local pkg_dir="$1"
    docker run --rm -v "$pkg_dir:/pkg" -w /pkg archlinux:latest bash -c "
        pacman -Sy --noconfirm pacman-contrib >/dev/null 2>&1
        useradd -m builder 2>/dev/null || true
        chown -R builder .
        su builder -c 'makepkg --printsrcinfo' > .SRCINFO
    "
}

# Push package to AUR (creates repo if it doesn't exist)
push_to_aur() {
    local pkgname="$1"
    local pkg_dir="$2"

    echo "Publishing $pkgname to AUR..."

    local aur_dir="/tmp/aur-$pkgname"
    rm -rf "$aur_dir"

    # Clone existing or initialize new
    if ! git clone "ssh://aur@aur.archlinux.org/$pkgname.git" "$aur_dir" 2>/dev/null; then
        echo "  Creating new AUR package: $pkgname"
        mkdir -p "$aur_dir"
        git -C "$aur_dir" init
        git -C "$aur_dir" remote add origin "ssh://aur@aur.archlinux.org/$pkgname.git"
    fi

    # Copy PKGBUILD and generate .SRCINFO
    cp "$pkg_dir/PKGBUILD" "$aur_dir/"
    generate_srcinfo "$aur_dir"

    # Commit and push
    git -C "$aur_dir" config user.name "github-actions[bot]"
    git -C "$aur_dir" config user.email "github-actions[bot]@users.noreply.github.com"
    git -C "$aur_dir" add PKGBUILD .SRCINFO

    if git -C "$aur_dir" diff --cached --quiet; then
        echo "  No changes for $pkgname"
        return 0
    fi

    git -C "$aur_dir" commit -m "Update to v$VERSION"
    git -C "$aur_dir" push -u origin master
    echo "  Published $pkgname"
}

# Update PKGBUILD with version and SHA
update_pkgbuild() {
    local pkgbuild="$1"
    local version="$2"
    local sha="$3"

    sed -i "s|^pkgver=.*|pkgver=$version|" "$pkgbuild"
    sed -i "s|^sha256sums=.*|sha256sums=('$sha')|" "$pkgbuild"
    sed -i "s|^pkgrel=.*|pkgrel=1|" "$pkgbuild"
}

# Generate PKGBUILD for a plugin
generate_plugin_pkgbuild() {
    local plugin_name="$1"  # e.g., "query"
    local plugin_file="$2"  # e.g., "plugins/ticket-query"
    local output_dir="$3"

    local metadata pkgver pkgdesc
    metadata=$(parse_plugin_metadata "$plugin_file")
    pkgver="${metadata%%|*}"
    pkgdesc="${metadata#*|}"

    # Determine dependencies
    local extra_deps=""
    case "$plugin_name" in
        query|migrate-beads) extra_deps="'jq'" ;;
    esac

    mkdir -p "$output_dir"
    cat > "$output_dir/PKGBUILD" << EOF
# Maintainer: wedow <wedow@users.noreply.github.com>
pkgname=ticket-$plugin_name
pkgver=$pkgver
pkgrel=1
pkgdesc="$pkgdesc"
arch=('any')
url="https://github.com/wedow/ticket"
license=('MIT')
depends=('ticket-core'${extra_deps:+ $extra_deps})
source=("ticket-\$pkgver.tar.gz::https://github.com/wedow/ticket/archive/refs/tags/v\$pkgver.tar.gz")
sha256sums=('$SHA256')

package() {
    cd "ticket-\$pkgver"
    install -Dm755 "plugins/ticket-$plugin_name" "\$pkgdir/usr/bin/ticket-$plugin_name"
}
EOF
}

# Main
main() {
    echo "Publishing ticket packages to AUR (v$VERSION)"

    setup_ssh

    local failed=()

    # 1. Publish ticket-core
    echo ""
    echo "=== ticket-core ==="
    update_pkgbuild "$REPO_ROOT/pkg/aur/ticket-core/PKGBUILD" "$VERSION" "$SHA256"
    push_to_aur "ticket-core" "$REPO_ROOT/pkg/aur/ticket-core" || failed+=("ticket-core")

    # 2. Publish individual plugins
    local plugin_deps=()
    if [[ -d "$REPO_ROOT/plugins" ]]; then
        for plugin_file in "$REPO_ROOT"/plugins/ticket-*; do
            [[ -f "$plugin_file" ]] || continue

            local plugin_name="${plugin_file##*/ticket-}"
            echo ""
            echo "=== ticket-$plugin_name ==="

            local plugin_pkg_dir="/tmp/pkg-ticket-$plugin_name"
            generate_plugin_pkgbuild "$plugin_name" "$plugin_file" "$plugin_pkg_dir"
            push_to_aur "ticket-$plugin_name" "$plugin_pkg_dir" || failed+=("ticket-$plugin_name")

            plugin_deps+=("'ticket-$plugin_name'")
        done
    fi

    # 3. Update and publish ticket-extras with plugin dependencies
    echo ""
    echo "=== ticket-extras ==="
    local extras_pkgbuild="$REPO_ROOT/pkg/aur/ticket-extras/PKGBUILD"
    update_pkgbuild "$extras_pkgbuild" "$VERSION" "SKIP"
    # Update depends line with discovered plugins
    if [[ ${#plugin_deps[@]} -gt 0 ]]; then
        local deps_str=$(IFS=' '; echo "${plugin_deps[*]}")
        sed -i "s|^depends=.*|depends=($deps_str)|" "$extras_pkgbuild"
    fi
    push_to_aur "ticket-extras" "$REPO_ROOT/pkg/aur/ticket-extras" || failed+=("ticket-extras")

    # 4. Publish ticket meta-package
    echo ""
    echo "=== ticket ==="
    update_pkgbuild "$REPO_ROOT/pkg/aur/ticket/PKGBUILD" "$VERSION" "SKIP"
    push_to_aur "ticket" "$REPO_ROOT/pkg/aur/ticket" || failed+=("ticket")

    # Summary
    echo ""
    if [[ ${#failed[@]} -gt 0 ]]; then
        echo "FAILED: ${failed[*]}"
        exit 1
    fi
    echo "All packages published successfully!"
}

main "$@"
