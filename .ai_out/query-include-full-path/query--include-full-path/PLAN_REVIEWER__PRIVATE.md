# Plan Reviewer Private Notes

## Verification Results

### Source Code Checks
- `cmd_query()` is at line 1321, confirmed still in core `ticket` script (not extracted to plugin)
- `cmd_help()` query line is at line 1505, confirmed
- README.md query line is at line 87, confirmed
- `find_tickets_dir()` returns absolute paths from `$PWD` walk (lines 8-27)
- TICKETS_DIR is always absolute for read commands (write-only fallback to ".tickets" at line 49 does not apply to query)
- AWK `FILENAME` receives the glob expansion of `"$TICKETS_DIR"/*.md` which is absolute
- The `cmd_ls()` while-loop pattern at line 653-664 is a good model for the flag parsing

### Step Definition Checks
- `JSONL output should have field "<field>"` exists at line 571 -- confirmed
- `the output should contain "<text>"` exists at line 359 -- confirmed
- `the output should not contain "<text>"` exists at line 366 -- confirmed
- `a ticket exists with ID "<id>" and title "<title>"` exists at line 95 -- confirmed
- `ticket "<id>" has status "<status>"` -- need to verify this step exists

### Potential Issues Found
1. The `JSONL output should have field` step only checks the FIRST non-empty line (line 581 has `break`). This is fine but worth noting -- if only the second ticket had the field, the test would still pass vacuously. In practice, `--include-full-path` applies to all records so this is not an issue.

2. JSON escaping: The plan correctly notes that existing code does NOT escape JSON values (line 1361). The plan is consistent with this existing behavior. Not a blocker.

3. The edge case where `TICKETS_DIR` is set via environment variable to a relative path -- in theory possible, but this is a pre-existing concern across the entire codebase, not specific to this feature. Not a blocker.

## Assessment
Plan is solid, surgical, well-researched. Minor refinements only.
