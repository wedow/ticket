---
id: t-ef1f
status: open
deps: []
links: []
created: 2026-01-24T23:58:09Z
type: task
priority: 2
assignee: Steve Macbeth
parent: t-ff45
---
# Update status values

Replace current statuses with open, in_progress, needs_testing, complete

## Design

Files: ticket
- Update status validation to accept: open, in_progress, needs_testing, complete
- Update cmd_close to set 'complete' instead of 'closed'
- Add cmd_testing function to set needs_testing status
- Update cmd_help with new statuses and testing command

## Acceptance Criteria

All four statuses validate, tk close sets complete, new tk testing command works

