---
id: tc-b4dc
status: open
deps: [tc-9c9c, tc-69a8]
links: []
created: 2026-01-09T13:13:37Z
type: task
priority: 2
assignee: Ray Myers
---
# Port feature ticket_edit to Python


## Notes

**2026-01-09T13:14:05Z**

Implement Python version of ticket_edit feature.

Add this line to python/bdd.sh:

```sh
# export TICKET_SCRIPT=py_ticket.sh
# ...
uv run --with behave behave features/ticket_edit.feature
```
