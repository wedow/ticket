#!/usr/bin/env bash
# Install ACL2 (includes SBCL) for the ticket-cli implementation
# Downloads and builds ACL2 - no root required

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SBCL_DIR="$SCRIPT_DIR/.sbcl"
ACL2_DIR="$SCRIPT_DIR/.acl2"
SBCL_VERSION="2.4.0"
ACL2_VERSION="8.5"

# Detect platform
case "$(uname -s)-$(uname -m)" in
    Linux-x86_64)  PLATFORM="x86-64-linux" ;;
    Linux-aarch64) PLATFORM="arm64-linux" ;;
    Darwin-x86_64) PLATFORM="x86-64-darwin" ;;
    Darwin-arm64)  PLATFORM="arm64-darwin" ;;
    *)
        echo "Unsupported platform: $(uname -s)-$(uname -m)" >&2
        exit 1
        ;;
esac

echo "=== Installing SBCL ${SBCL_VERSION} ==="
SBCL_URL="https://downloads.sourceforge.net/project/sbcl/sbcl/${SBCL_VERSION}/sbcl-${SBCL_VERSION}-${PLATFORM}-binary.tar.bz2"
curl -L -o /tmp/sbcl.tar.bz2 "$SBCL_URL"
rm -rf "$SBCL_DIR"
mkdir -p "$SBCL_DIR"
tar xjf /tmp/sbcl.tar.bz2 -C "$SBCL_DIR" --strip-components=1
rm /tmp/sbcl.tar.bz2
echo "SBCL installed: $("$SBCL_DIR/run-sbcl.sh" --version)"

echo ""
echo "=== Installing ACL2 ${ACL2_VERSION} ==="
ACL2_URL="https://github.com/acl2/acl2/archive/refs/tags/${ACL2_VERSION}.tar.gz"
curl -L -o /tmp/acl2.tar.gz "$ACL2_URL"
rm -rf "$ACL2_DIR"
mkdir -p "$ACL2_DIR"
tar xzf /tmp/acl2.tar.gz -C /tmp
rm /tmp/acl2.tar.gz

echo "Building ACL2 (this takes ~1 minute)..."
cd /tmp/acl2-${ACL2_VERSION}
make LISP="$SBCL_DIR/run-sbcl.sh" > /tmp/acl2-build.log 2>&1
mv /tmp/acl2-${ACL2_VERSION}/* "$ACL2_DIR/"
rm -rf /tmp/acl2-${ACL2_VERSION}

echo ""
echo "=== Done ==="
echo "ACL2 installed to: $ACL2_DIR"
"$ACL2_DIR/saved_acl2" --version 2>/dev/null | head -1 || echo "ACL2 ready"
echo ""
echo "Run: ./acl2_ticket.sh create 'My ticket'"
