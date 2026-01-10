#!/usr/bin/env bash
# Run BDD tests with TypeScript implementation

set -euo pipefail

# Get the project root directory (parent of typescript/)
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# Export the TypeScript ticket script for the tests
export TICKET_SCRIPT="${PROJECT_ROOT}/ts_ticket.sh"

# Run id_resolution feature tests
uv run --with behave behave "${PROJECT_ROOT}/features/id_resolution.feature"

# Run ticket_links feature tests
uv run --with behave behave "${PROJECT_ROOT}/features/ticket_links.feature"

# Run ticket_query feature tests
uv run --with behave behave "${PROJECT_ROOT}/features/ticket_query.feature"
