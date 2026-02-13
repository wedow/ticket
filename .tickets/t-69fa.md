---
id: t-69fa
status: closed
deps: []
links: []
created: 2026-01-24T23:58:09Z
type: task
priority: 2
assignee: Steve Macbeth
parent: t-ff45
---
# Add workstream type

Add workstream as a valid ticket type alongside epic

## Design

Files: ticket
- Add 'workstream' to VALID_TYPES array/validation
- Update cmd_help to document workstream type

## Acceptance Criteria

tk create --type workstream works, tk help shows workstream

