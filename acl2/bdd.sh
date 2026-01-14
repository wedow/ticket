#!/usr/bin/env bash
# BDD test runner for ACL2/Common Lisp ticket-cli implementation
#
# Uses the shared Behave features with the ACL2 implementation.

set -euo pipefail

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
export TICKET_SCRIPT="${PROJECT_ROOT}/acl2_ticket.sh"

# Check if SBCL is available
if ! command -v sbcl &> /dev/null; then
    echo "Error: sbcl is required for ACL2/Common Lisp implementation" >&2
    echo "Install with: apt-get install sbcl (Debian/Ubuntu) or brew install sbcl (macOS)" >&2
    exit 1
fi

# Run the ticket_creation feature
echo "Running ticket_creation.feature..."
cd "$PROJECT_ROOT"
uv run --with behave behave features/ticket_creation.feature

echo ""
echo "All ACL2 BDD tests passed!"
