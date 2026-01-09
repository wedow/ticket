#!/usr/bin/env bash
# Wrapper script to run the Python ticket CLI implementation

set -euo pipefail

# Get the directory containing this script
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Run the Python CLI using Python directly from the package source
export PYTHONPATH="$SCRIPT_DIR/python/ticket/src:${PYTHONPATH:-}"
exec python3 -m ticket.cli "$@"
