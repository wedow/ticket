#!/usr/bin/env bash
# Wrapper script to run the ACL2/Common Lisp ticket CLI implementation
#
# Requires SBCL (Steel Bank Common Lisp) to be installed.
# Install on Ubuntu/Debian: apt-get install sbcl
# Install on macOS: brew install sbcl

set -euo pipefail

# Get the directory containing this script
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ACL2_DIR="$SCRIPT_DIR/acl2/ticket"

# Check if SBCL is installed
if ! command -v sbcl &> /dev/null; then
    echo "Error: sbcl (Steel Bank Common Lisp) is required but not installed." >&2
    echo "Install with: apt-get install sbcl (Debian/Ubuntu) or brew install sbcl (macOS)" >&2
    exit 1
fi

# Build the argument list for Lisp
LISP_ARGS=""
for arg in "$@"; do
    # Escape double quotes in arguments
    escaped_arg="${arg//\"/\\\"}"
    LISP_ARGS="$LISP_ARGS \"$escaped_arg\""
done

# Run the Lisp CLI
exec sbcl --noinform --non-interactive \
    --load "$ACL2_DIR/src/package.lisp" \
    --load "$ACL2_DIR/src/utils.lisp" \
    --load "$ACL2_DIR/src/create.lisp" \
    --load "$ACL2_DIR/src/cli.lisp" \
    --eval "(sb-ext:exit :code (ticket:main (list $LISP_ARGS)))"
