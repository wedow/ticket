#!/usr/bin/env bash
# Wrapper script to run the ACL2 ticket CLI implementation
# Uses actual ACL2 theorem prover

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ACL2_DIR="$SCRIPT_DIR/acl2"
ACL2_LOCAL="$ACL2_DIR/.acl2/saved_acl2"

# Find ACL2
if [[ -x "$ACL2_LOCAL" ]]; then
    ACL2="$ACL2_LOCAL"
elif command -v acl2 &> /dev/null; then
    ACL2="acl2"
else
    echo "Error: ACL2 not found." >&2
    echo "Run: $SCRIPT_DIR/acl2/install-acl2.sh" >&2
    exit 1
fi

# Handle commands
CMD="${1:-help}"
shift || true

case "$CMD" in
    create)
        TITLE="${1:-Untitled}"
        TYPE="${2:-task}"
        PRIORITY="${3:-2}"
        # Get directory name for prefix
        DIR_NAME="$(basename "$PWD")"
        
        # Ensure .tickets directory exists
        mkdir -p .tickets
        
        # Run ACL2 to create ticket
        "$ACL2" <<EOF 2>&1 | grep -oE '"[a-z]+-[0-9]+"' | head -1 | tr -d '"'
(ld "$ACL2_DIR/ticket/ticket.lisp" :ld-verbose nil)
(mv-let (id state)
        (ticket-create "$DIR_NAME" "$TITLE" "$TYPE" $PRIORITY state)
        (prog2$ (cw "~x0~%" id)
                (mv id state)))
(quit)
EOF
        ;;
    help|--help|-h)
        echo "tk - minimal ticket system (ACL2 implementation)"
        echo ""
        echo "Usage: tk <command> [args]"
        echo ""
        echo "Commands:"
        echo "  create [title] [type] [priority]  Create ticket"
        echo "  help                              Show this help"
        echo ""
        echo "Code admitted by ACL2 theorem prover."
        ;;
    *)
        echo "Unknown command: $CMD" >&2
        exit 1
        ;;
esac
