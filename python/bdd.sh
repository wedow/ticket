#!/usr/bin/env bash
# Run BDD tests with Python implementation

set -euo pipefail

# Get the project root directory (parent of python/)
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# Export the Python ticket script for the tests
export TICKET_SCRIPT="${PROJECT_ROOT}/py_ticket.sh"

# Run ticket_creation feature tests
uv run --with behave behave "${PROJECT_ROOT}/features/ticket_creation.feature"

# Run ticket_show feature tests
uv run --with behave behave "${PROJECT_ROOT}/features/ticket_show.feature"

# Run ticket_notes feature tests
uv run --with behave behave "${PROJECT_ROOT}/features/ticket_notes.feature"

# Run ticket_status feature tests
uv run --with behave behave "${PROJECT_ROOT}/features/ticket_status.feature"

# Run ticket_listing feature tests
uv run --with behave behave "${PROJECT_ROOT}/features/ticket_listing.feature"

# Run ticket_edit feature tests
uv run --with behave behave "${PROJECT_ROOT}/features/ticket_edit.feature"

# Run ticket_links feature tests
uv run --with behave behave "${PROJECT_ROOT}/features/ticket_links.feature"
