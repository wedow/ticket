---
id: t-ef1f
status: closed
deps: []
links: []
created: 2026-01-24T23:58:09Z
type: task
priority: 2
assignee: Steve Macbeth
parent: t-ff45
---
# Update status values

Add needs_testing status to existing workflow (backward compatible)

## Design

Files: ticket
- Update VALID_STATUSES to include needs_testing: "open in_progress needs_testing closed"
- Add cmd_testing function to set needs_testing status
- Update cmd_help with new status and testing command

## Acceptance Criteria

needs_testing status validates, new tk testing command works, existing closed tickets unaffected

