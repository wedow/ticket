---
id: tc-8bfc
status: open
deps: [tc-9c9c, tc-98c7]
links: []
created: 2026-01-09T13:13:37Z
type: task
priority: 2
assignee: Ray Myers
---
# Port feature ticket_listing to Python


## Notes

**2026-01-09T13:13:55Z**

Implement Python version of ticket_listing feature.

Add this line to python/bdd.sh:

```sh
# export TICKET_SCRIPT=py_ticket.sh
# ...
uv run --with behave behave features/ticket_listing.feature
```
