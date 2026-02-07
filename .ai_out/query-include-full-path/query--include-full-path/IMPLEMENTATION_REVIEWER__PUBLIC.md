# Implementation Review: `--include-full-path` flag for `tk query`

## Summary

The implementation adds a `--include-full-path` flag to `tk query` that includes the absolute file path of each ticket in the JSON output. The change is surgical and well-executed: 4 files modified, 40 lines added, 4 lines removed. All 8 query BDD scenarios pass (5 existing + 3 new). The 9 failing scenarios are pre-existing `ticket_plugins.feature` failures unrelated to this change.

**Overall assessment: APPROVE -- no blocking issues found.**

## Requirements Checklist

1. **Add `--include-full-path` flag to `tk query`** -- DONE. Flag parsed in a while-loop, passed to AWK as a variable, `full_path` field appended conditionally.
2. **Update help text in `cmd_help()`** -- DONE. Line 1514-1515 of `ticket`.
3. **Update `tk help` to reflect this change** -- Same as #2; DONE.
4. **Add BDD scenarios** -- DONE. Three scenarios covering: flag alone, flag with jq filter, and regression guard for absence without flag.
5. **Update README usage block** -- DONE. Lines 87-88 of `README.md`.

## CRITICAL Issues

None.

## IMPORTANT Issues

None.

## Suggestions

1. **JSON escaping of file paths (low risk, optional)**: The `full_path` value is injected into AWK's printf without JSON-escaping. If a path contained a double quote or backslash, the JSON output would be malformed. In practice, `.tickets/` paths are filesystem-controlled and extremely unlikely to contain such characters, and the existing code has the same non-escaping pattern for all field values. This is a pre-existing limitation, not something introduced by this change. Mentioning for awareness only -- not worth fixing here.

2. **BDD test could verify absolute path**: The test checks for `query-001.md` in the output (good) and checks the `full_path` field exists (good), but does not verify the path is absolute (starts with `/`). A step like `And the output should match "full_path.*/"` could strengthen this. However, this is minor since the path resolution logic in `find_tickets_dir()` guarantees absolute paths for read commands.

## Code Quality Assessment

- **Follows existing patterns**: The flag-parsing while-loop mirrors `cmd_ls` and other commands. The AWK variable passing via `-v` is the established pattern.
- **Placement of `full_path` field**: Appended after all other fields, which is clean -- it does not interleave with the existing field loop.
- **Comma handling**: Correct. Since `full_path` is only emitted inside the `if (field_count > 0)` block, there is always at least one prior field, so the leading comma is always valid.
- **Help text and README are consistent** with each other.
- **CHANGELOG** entry is appropriately placed under `[Unreleased] > Added`.
- **No existing tests were removed or modified** -- all changes are additive.

## Documentation Updates Needed

None. CLAUDE.md, README.md, and CHANGELOG.md are all already updated as part of this change.
