---
id: tc-dd9d
status: open
deps: [tc-9c9c]
links: []
created: 2026-01-09T13:13:17Z
type: task
priority: 2
assignee: Ray Myers
---
# Port feature ticket_creation to Python


## Notes

**2026-01-09T13:13:23Z**

Implement Python version of ticket_creation feature.

Add this line to python/bdd.sh:

```sh
# export TICKET_SCRIPT=py_ticket.sh
# ...
uv run --with behave behave features/ticket_creation.feature
```
