---
id: tc-e42d
status: open
deps: [tc-9c9c, tc-8bfc]
links: []
created: 2026-01-09T13:13:43Z
type: task
priority: 2
assignee: Ray Myers
---
# Port feature ticket_query to Python


## Notes

**2026-01-09T13:14:19Z**

Implement Python version of ticket_query feature.

Add this line to python/bdd.sh:

```sh
# export TICKET_SCRIPT=py_ticket.sh
# ...
uv run --with behave behave features/ticket_query.feature
```
