---
id: tc-9c9c
status: closed
deps: []
links: []
created: 2026-01-09T13:03:20Z
type: task
priority: 2
assignee: Ray Myers
---
# Scope out BDD Python

Using `./ticket create "<TITLE>"` make tickets per feature in features/, like "Port feature X ... to Python..

Add to all ticket descriptions that they add a line like this to `python/bdd.sh`.

```sh
# export TICKET_SCRIPT=py_ticket.sh
# ...
uv run --with behave behave features/<...>.feature
```

Make the new tickets depend on this one with `./ticket dep [new-id] tc-9c9c`.

Make them depend on eachother in a logical implementation order.

## Notes

**2026-01-09T13:15:04Z**

Created 10 tickets for porting BDD features to Python:

1. tc-dd9d - Port feature ticket_creation to Python
2. tc-69a8 - Port feature ticket_show to Python
3. tc-98c7 - Port feature ticket_status to Python
4. tc-8bfc - Port feature ticket_listing to Python
5. tc-128e - Port feature ticket_notes to Python
6. tc-b4dc - Port feature ticket_edit to Python
7. tc-d29c - Port feature ticket_dependencies to Python
8. tc-c845 - Port feature ticket_links to Python
9. tc-e42d - Port feature ticket_query to Python
10. tc-c8f8 - Port feature id_resolution to Python

All tickets:
- Include instructions for adding behave test commands to python/bdd.sh
- Depend on tc-9c9c (this ticket)
- Are organized in a logical implementation order:
  * Creation -> Show -> Status -> Listing
  * Show -> (Notes, Edit, Dependencies, Links)
  * Listing -> Query -> ID Resolution
