#!/usr/bin/env bash
# Wrapper script to run the TypeScript ticket CLI implementation

set -euo pipefail

# Get the directory containing this script
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Run the TypeScript CLI using Bun
exec bun run "$SCRIPT_DIR/typescript/ticket/src/cli.ts" "$@"
