#!/usr/bin/env bash
# Run BDD tests with Zig implementation

set -euo pipefail

# Get the project root directory (parent of zig/)
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# Export the Zig ticket script for the tests
export TICKET_SCRIPT="${PROJECT_ROOT}/zig_ticket.sh"

# Run ticket_dependencies feature tests
uv run --with behave behave "${PROJECT_ROOT}/features/ticket_dependencies.feature"

# Run id_resolution feature tests
uv run --with behave behave "${PROJECT_ROOT}/features/id_resolution.feature"
