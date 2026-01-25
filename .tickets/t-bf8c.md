---
id: t-bf8c
status: open
deps: [t-69fa]
links: []
created: 2026-01-24T23:58:10Z
type: task
priority: 2
assignee: Steve Macbeth
parent: t-ff45
---
# Update tk ready with hierarchy gating

Only show tickets whose parent chain is in_progress

## Design

Files: ticket
- Modify cmd_ready awk logic:
  1. If ticket has no parent, include (current behavior)
  2. If ticket has parent, check parent status is in_progress
  3. Recursively check up the chain
- May need helper to resolve parent statuses

## Acceptance Criteria

Tickets under open epic don't show in tk ready, tickets under in_progress epic do show

