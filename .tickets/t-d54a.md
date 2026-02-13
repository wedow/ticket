---
id: t-d54a
status: closed
deps: []
links: []
created: 2026-01-24T23:58:10Z
type: task
priority: 2
assignee: Steve Macbeth
parent: t-ff45
---
# Add tk workflow command

New command that outputs workflow narrative for LLM context

## Design

Files: ticket
- Add cmd_workflow function
- Output the full workflow guide as heredoc (hierarchy, types, statuses, readiness rules, propagation, ticket structure, working conventions, commit format)
- Add to cmd_help

## Acceptance Criteria

tk workflow outputs complete guide, tk help shows workflow command

