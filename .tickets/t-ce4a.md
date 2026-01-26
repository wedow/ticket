---
id: t-ce4a
status: closed
deps: []
links: []
created: 2026-01-26T06:43:41Z
type: bug
priority: 2
assignee: Steve Macbeth
---
# tk list should alias tk ls

Running 'tk list' doesn't work - should be an alias for 'tk ls' like users would expect


## Notes

**2026-01-26T06:45:59Z**

## Root Cause

Missing alias in command dispatch case statement (ticket:1897).

## Fix

Added `list` as alternate pattern: `ls|list) shift; cmd_ls ...`

Updated help text and README to document the alias.
