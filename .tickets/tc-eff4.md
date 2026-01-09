---
id: tc-eff4
status: closed
deps: [tc-3cff]
links: []
created: 2026-01-09T17:20:20Z
type: task
priority: 2
assignee: Ray Myers
---
# Port feature ticket_links to Go

Implement the ticket_links functionality in Go. 

Add this line to go/bdd.sh:

```
# Run ticket_links feature tests
godog features/ticket_links.feature
```

## Notes

**2026-01-09T18:07:46Z**

Implemented cmdUnlink function in Go CLI. Added unlink case to switch statement in Run function. All ticket_links feature tests now passing.
