---
id: tc-3d2f
status: open
deps: [tc-b909]
links: []
created: 2026-01-12T03:22:38Z
type: task
priority: 3
assignee: Ray Myers
---
# Add full BDD suite to C build

Integrate all BDD tests into the C build system.

Tasks:
- Create c/bdd.sh script that:
  - Exports TICKET_SCRIPT pointing to c_ticket.sh
  - Runs all 10 feature tests using behave
- Add c-bdd target to root Makefile
- Ensure all BDD tests pass
- Document any C-specific considerations

Example c/bdd.sh structure:
#!/usr/bin/env bash
set -euo pipefail

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/." && pwd)"
export TICKET_SCRIPT="${PROJECT_ROOT}/c_ticket.sh"

# Run all features
behave "${PROJECT_ROOT}/features/"

