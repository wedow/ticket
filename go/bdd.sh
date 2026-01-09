#!/usr/bin/env bash
# Run BDD tests with Go implementation

set -euo pipefail

# Get the project root directory (parent of go/)
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# Export the Go ticket script for the tests
export TICKET_SCRIPT="${PROJECT_ROOT}/go_ticket.sh"

# Run ticket_creation feature tests
uv run --with behave behave "${PROJECT_ROOT}/features/ticket_creation.feature"

# Run ticket_show feature tests
uv run --with behave behave "${PROJECT_ROOT}/features/ticket_show.feature"

# Run ticket_query feature tests
uv run --with behave behave "${PROJECT_ROOT}/features/ticket_query.feature"

# Run ticket_notes feature tests
uv run --with behave behave "${PROJECT_ROOT}/features/ticket_notes.feature"
