# PLANNER Private Notes

## Key Observations

1. `cmd_query()` is a built-in at line 1321 of `ticket` script
2. AWK's `FILENAME` variable already contains the full absolute path (since `$TICKETS_DIR` is absolute and the glob `$TICKETS_DIR/*.md` expands to absolute paths)
3. The `emit()` function inside AWK builds JSON from `field_keys`/`field_vals` arrays
4. Current signature: `cmd_query [jq-filter]` -- filter is positional `$1`
5. Flag parsing pattern in other commands (e.g., `cmd_ls`) uses `while [[ $# -gt 0 ]]` with `case` statement
6. The AWK already tracks `prev_file=FILENAME` so we have the path available in `emit()`

## Design Decision: Where to inject full_path

Two options:
- **Option A**: Pass a flag variable to AWK and have AWK inject `full_path` into the JSON output
- **Option B**: Post-process with jq to add the path

Option A is better: keeps it in one pass, no jq dependency for this feature, matches existing architecture.

The AWK variable `prev_file` holds `FILENAME` at emit time. We pass `include_path=1` to AWK and conditionally add `"full_path":"<prev_file>"` as the first or last field in the JSON object.

## Argument Parsing Change

Current: `local filter="${1:-}"` (single positional arg)
New: Parse `--include-full-path` flag from args, remaining positional arg is the filter.

This is a minimal change: add a while loop to extract the flag, treat leftover as the jq filter.

## Files to Change

1. `/home/nickolaykondratyev/git_repos/wedow_ticket/ticket` - `cmd_query()` function (lines 1321-1375) and `cmd_help()` (line 1505)
2. `/home/nickolaykondratyev/git_repos/wedow_ticket/features/ticket_query.feature` - Add 2 scenarios
3. `/home/nickolaykondratyev/git_repos/wedow_ticket/README.md` - Line 87
4. `/home/nickolaykondratyev/git_repos/wedow_ticket/CHANGELOG.md` - Unreleased section
