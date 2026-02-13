---
id: t-bf8c
status: closed
deps: [t-69fa]
links: []
created: 2026-01-24T23:58:10Z
type: task
priority: 2
assignee: Steve Macbeth
parent: t-ff45
---
# Update tk ready with hierarchy gating

Only show tickets whose parent chain is in_progress (with opt-out flag)

## Design

Files: ticket
- Add --open flag to cmd_ready that skips hierarchy checks (old behavior)
- Modify cmd_ready awk logic:
  1. If --open flag set, skip parent checks entirely
  2. If ticket has no parent, include (current behavior)
  3. If ticket has parent, check parent status is in_progress
  4. Recursively check up the chain
- Update cmd_help to document --open flag

## Acceptance Criteria

- `tk ready` shows only tickets whose parent chain is in_progress
- `tk ready --open` shows all ready tickets regardless of parent status (old behavior)
- Tickets with no parent show in both modes

