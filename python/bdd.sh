#!/usr/bin/env bash
# Run BDD tests with Python implementation

set -euo pipefail

# Get the project root directory (parent of python/)
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# Export the Python ticket script for the tests
export TICKET_SCRIPT="${PROJECT_ROOT}/py_ticket.sh"

# Run ticket_creation feature tests
uv run --with behave behave "${PROJECT_ROOT}/features/ticket_creation.feature"
