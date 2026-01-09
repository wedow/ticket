---
id: tc-fc98
status: closed
deps: [tc-ba73]
links: []
created: 2026-01-09T20:28:50Z
type: task
priority: 2
assignee: Ray Myers
---
# Port feature ticket_query to Zig

Implement the ticket_query functionality. Add this line to zig/bdd.sh:

```bash
# Run ticket_query feature tests
behave features/ticket_query.feature
```

## Notes

**2026-01-09T21:58:56Z**

Fixed handleQuery implementation by updating ArrayList API calls to match Zig 0.15.2. All methods like append(), appendSlice(), writer(), and toOwnedSlice() now require allocator as first argument. Tests passing.
