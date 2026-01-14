#!/usr/bin/env bash
# Install SBCL locally for the ACL2 ticket-cli implementation
# Downloads pre-built binary - no root required

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INSTALL_DIR="$SCRIPT_DIR/.sbcl"
VERSION="2.4.0"

# Detect platform
case "$(uname -s)-$(uname -m)" in
    Linux-x86_64)  PLATFORM="x86-64-linux" ;;
    Linux-aarch64) PLATFORM="arm64-linux" ;;
    Darwin-x86_64) PLATFORM="x86-64-darwin" ;;
    Darwin-arm64)  PLATFORM="arm64-darwin" ;;
    *)
        echo "Unsupported platform: $(uname -s)-$(uname -m)" >&2
        echo "Please install SBCL manually: https://www.sbcl.org/getting.html" >&2
        exit 1
        ;;
esac

URL="https://downloads.sourceforge.net/project/sbcl/sbcl/${VERSION}/sbcl-${VERSION}-${PLATFORM}-binary.tar.bz2"
TARBALL="/tmp/sbcl-${VERSION}.tar.bz2"

echo "Installing SBCL ${VERSION} for ${PLATFORM}..."
echo "Download: $URL"

# Download
curl -L -o "$TARBALL" "$URL"

# Extract to install dir
rm -rf "$INSTALL_DIR"
mkdir -p "$INSTALL_DIR"
tar xjf "$TARBALL" -C "$INSTALL_DIR" --strip-components=1

# Cleanup
rm -f "$TARBALL"

echo ""
echo "SBCL installed to: $INSTALL_DIR"
echo "Testing..."
"$INSTALL_DIR/run-sbcl.sh" --version

echo ""
echo "Done! You can now run: ./acl2_ticket.sh"
