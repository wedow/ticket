#!/usr/bin/env bash
# Publish ticket packages to Homebrew tap
# Usage: ./scripts/publish-homebrew.sh <version> <sha256>
# Requires: TAP_GITHUB_TOKEN environment variable

set -euo pipefail

VERSION="${1#v}"
SHA256="$2"
REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
TAP_REPO="wedow/homebrew-tools"

# Parse plugin metadata from script file
parse_plugin_metadata() {
    local plugin_file="$1"
    local version desc
    version=$(grep -m1 '^# tk-plugin-version:' "$plugin_file" 2>/dev/null | sed 's/^# tk-plugin-version: *//' || echo "")
    desc=$(grep -m1 '^# tk-plugin:' "$plugin_file" 2>/dev/null | sed 's/^# tk-plugin: *//' || echo "")
    echo "${version:-$VERSION}|${desc:-No description}"
}

# Generate formula for a plugin
generate_plugin_formula() {
    local plugin_name="$1"  # e.g., "query"
    local plugin_file="$2"
    local formula_dir="$3"

    local metadata pkgver pkgdesc
    metadata=$(parse_plugin_metadata "$plugin_file")
    pkgver="${metadata%%|*}"
    pkgdesc="${metadata#*|}"

    # Convert plugin name to Ruby class name (ticket-query -> TicketQuery)
    local class_name="Ticket$(echo "$plugin_name" | sed -r 's/(^|-)(\w)/\U\2/g')"

    cat > "$formula_dir/ticket-$plugin_name.rb" << EOF
class $class_name < Formula
  desc "$pkgdesc"
  homepage "https://github.com/wedow/ticket"
  url "https://github.com/wedow/ticket/archive/refs/tags/v$pkgver.tar.gz"
  sha256 "$SHA256"
  license "MIT"

  depends_on "ticket-core"

  def install
    bin.install "plugins/ticket-$plugin_name"
  end

  test do
    assert_match "tk-plugin:", shell_output("head -5 #{bin}/ticket-$plugin_name")
  end
end
EOF
}

main() {
    echo "Publishing ticket packages to Homebrew tap (v$VERSION)"

    # Clone tap
    local tap_dir="/tmp/homebrew-tap"
    rm -rf "$tap_dir"
    git clone "https://x-access-token:${TAP_GITHUB_TOKEN}@github.com/${TAP_REPO}.git" "$tap_dir"

    local formula_dir="$tap_dir/Formula"
    mkdir -p "$formula_dir"

    # 1. Update ticket-core formula
    echo "Updating ticket-core..."
    cat > "$formula_dir/ticket-core.rb" << EOF
class TicketCore < Formula
  desc "Minimal ticket tracking in bash (core only)"
  homepage "https://github.com/wedow/ticket"
  url "https://github.com/wedow/ticket/archive/refs/tags/v$VERSION.tar.gz"
  sha256 "$SHA256"
  license "MIT"

  def install
    bin.install "ticket" => "tk"
  end

  test do
    system "#{bin}/tk", "help"
  end
end
EOF

    # 2. Generate plugin formulas
    local plugin_deps=()
    if [[ -d "$REPO_ROOT/plugins" ]]; then
        for plugin_file in "$REPO_ROOT"/plugins/ticket-*; do
            [[ -f "$plugin_file" ]] || continue
            local plugin_name="${plugin_file##*/ticket-}"
            echo "Generating formula for ticket-$plugin_name..."
            generate_plugin_formula "$plugin_name" "$plugin_file" "$formula_dir"
            plugin_deps+=("\"ticket-$plugin_name\"")
        done
    fi

    # 3. Update ticket-extras formula (meta-formula)
    echo "Updating ticket-extras..."
    local deps_ruby=""
    for dep in "${plugin_deps[@]:-}"; do
        deps_ruby+="  depends_on $dep"$'\n'
    done
    cat > "$formula_dir/ticket-extras.rb" << EOF
class TicketExtras < Formula
  desc "All official plugins for ticket"
  homepage "https://github.com/wedow/ticket"
  url "https://github.com/wedow/ticket/archive/refs/tags/v$VERSION.tar.gz"
  sha256 "$SHA256"
  license "MIT"

$deps_ruby
  def install
    # Meta-formula - plugins installed via dependencies
    (prefix/"README").write "This is a meta-formula that installs all ticket plugins."
  end
end
EOF

    # 4. Update ticket formula (full installation)
    echo "Updating ticket..."
    cat > "$formula_dir/ticket.rb" << EOF
class Ticket < Formula
  desc "Minimal ticket tracking in bash"
  homepage "https://github.com/wedow/ticket"
  url "https://github.com/wedow/ticket/archive/refs/tags/v$VERSION.tar.gz"
  sha256 "$SHA256"
  license "MIT"

  depends_on "ticket-core"
  depends_on "ticket-extras"

  def install
    # Meta-formula - everything installed via dependencies
    (prefix/"README").write "Full ticket installation with all plugins."
  end

  test do
    system "tk", "help"
  end
end
EOF

    # Commit and push
    cd "$tap_dir"
    git config user.name "github-actions[bot]"
    git config user.email "github-actions[bot]@users.noreply.github.com"
    git add Formula/

    if git diff --cached --quiet; then
        echo "No changes to publish"
        exit 0
    fi

    git commit -m "ticket v$VERSION"
    git push

    echo "All formulas published successfully!"
}

main "$@"
