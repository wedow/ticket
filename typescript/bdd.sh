#!/usr/bin/env bash
# Run BDD tests with TypeScript implementation

set -euo pipefail

# Get the project root directory (parent of typescript/)
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# Export the TypeScript ticket script for the tests
export TICKET_SCRIPT="${PROJECT_ROOT}/ts_ticket.sh"

# Run each feature test in logical order (basic to complex)
uv run --with behave behave "${PROJECT_ROOT}/features/ticket_creation.feature"
uv run --with behave behave "${PROJECT_ROOT}/features/ticket_show.feature"
uv run --with behave behave "${PROJECT_ROOT}/features/ticket_status.feature"
uv run --with behave behave "${PROJECT_ROOT}/features/ticket_listing.feature"
uv run --with behave behave "${PROJECT_ROOT}/features/ticket_notes.feature"
uv run --with behave behave "${PROJECT_ROOT}/features/ticket_edit.feature"
uv run --with behave behave "${PROJECT_ROOT}/features/ticket_dependencies.feature"
uv run --with behave behave "${PROJECT_ROOT}/features/ticket_links.feature"
uv run --with behave behave "${PROJECT_ROOT}/features/ticket_query.feature"
uv run --with behave behave "${PROJECT_ROOT}/features/id_resolution.feature"
