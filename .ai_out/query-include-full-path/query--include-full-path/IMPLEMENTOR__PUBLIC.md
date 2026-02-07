# Implementation Summary: `--include-full-path` flag for `tk query`

## Status: COMPLETE

## Changes Made

### 1. `/home/nickolaykondratyev/git_repos/wedow_ticket/ticket` (lines 1321-1377)

**`cmd_query()` function** -- Replaced simple `local filter="${1:-}"` with a while-loop flag parser that handles `--include-full-path` and passes `include_path` variable to AWK. In AWK's `emit()` function, added conditional `full_path` field appended after all other fields (inside the `if (field_count > 0)` block, before `printf "}\n"`).

**`cmd_help()` function** (line 1514-1515) -- Updated query line from `query [jq-filter]` to `query [options] [jq-filter]` and added `--include-full-path` sub-line.

### 2. `/home/nickolaykondratyev/git_repos/wedow_ticket/README.md` (line 87-88)

Mirrored the help text change in the usage block.

### 3. `/home/nickolaykondratyev/git_repos/wedow_ticket/features/ticket_query.feature`

Added 3 new BDD scenarios:
- **Query with --include-full-path includes file path** -- verifies JSONL validity, `full_path` field presence, and `.md` filename in output
- **Query with --include-full-path and jq filter** -- verifies flag works with jq filter, only matching tickets returned
- **Query without --include-full-path excludes file path** -- regression guard ensuring `full_path` is absent by default

### 4. `/home/nickolaykondratyev/git_repos/wedow_ticket/CHANGELOG.md`

Added entry under `[Unreleased] > Added`.

## Deviations from Plan

None. Followed the plan exactly, including the reviewer's minor correction (simplified comma logic without the redundant `if (field_count > 0)` guard).

## Test Results

- All 8 query scenarios pass (5 existing + 3 new)
- 117 scenarios pass, 9 fail (all 9 are pre-existing `ticket_plugins.feature` failures due to environment issues, exit code 126)
- No new step definitions were needed
