#!/usr/bin/env bash
# Wrapper script to run the ACL2/Common Lisp ticket CLI implementation
# Requires SBCL (Steel Bank Common Lisp)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ACL2_DIR="$SCRIPT_DIR/acl2/ticket"
SBCL_LOCAL="$SCRIPT_DIR/acl2/.sbcl/run-sbcl.sh"

# Find SBCL - check local install first, then system
if [[ -x "$SBCL_LOCAL" ]]; then
    SBCL="$SBCL_LOCAL"
elif command -v sbcl &> /dev/null; then
    SBCL="sbcl"
else
    echo "Error: SBCL not found." >&2
    echo "" >&2
    echo "Install SBCL:" >&2
    echo "  macOS:        brew install sbcl" >&2
    echo "  Ubuntu/Debian: sudo apt-get install sbcl" >&2
    echo "  Fedora:       sudo dnf install sbcl" >&2
    echo "  Arch:         sudo pacman -S sbcl" >&2
    echo "" >&2
    echo "Or run: $SCRIPT_DIR/acl2/install-sbcl.sh" >&2
    exit 1
fi

# Build the argument list for Lisp
LISP_ARGS=""
for arg in "$@"; do
    escaped_arg="${arg//\\/\\\\}"
    escaped_arg="${escaped_arg//\"/\\\"}"
    LISP_ARGS="$LISP_ARGS \"$escaped_arg\""
done

exec "$SBCL" --noinform --non-interactive \
    --load "$ACL2_DIR/src/package.lisp" \
    --load "$ACL2_DIR/src/utils.lisp" \
    --load "$ACL2_DIR/src/create.lisp" \
    --load "$ACL2_DIR/src/cli.lisp" \
    --eval "(sb-ext:exit :code (ticket:main (list $LISP_ARGS)))"
