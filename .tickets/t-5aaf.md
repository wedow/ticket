---
id: t-5aaf
status: open
deps: []
links: []
created: 2026-01-26T07:04:33Z
type: task
priority: 3
assignee: Steve Macbeth
---
# Figure out how to handle status="needs testing"

Need to decide if 'tk ready' should only return tasks that are ready to be worked on by the agent or also by the user, and thus return tickets that are  ready for testing. Or if we should add a new command 'tk test-ready' or the like that returns tickets ready for testing. Or just rely on 'tk ls --status="needs testing"'

