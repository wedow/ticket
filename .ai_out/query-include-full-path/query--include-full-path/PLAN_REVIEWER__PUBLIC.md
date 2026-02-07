# Plan Review

## Executive Summary

The plan is well-structured, surgical, and correctly aligned with the existing codebase patterns. All source code assumptions were verified against the actual implementation. I have two minor corrections and one small simplification opportunity, none of which are blocking.

## Critical Issues (BLOCKERS)

None.

## Major Concerns

None.

## Minor Issues (inline corrections -- PLAN_ITERATION can be skipped)

### 1. AWK `emit()` placement: field_count guard is incomplete

The plan says to add the `full_path` field inside `emit()` after the field loop (line 1363) but before `printf "}\n"`. The proposed code is:

```awk
if (include_path == 1) {
    if (field_count > 0) printf ","
    printf "\"full_path\":\"%s\"", prev_file
}
```

This is correct, but the guard `if (field_count > 0)` should always be true here because `emit()` is already inside `if (field_count > 0)` (line 1343). So the comma is always needed when `include_path == 1`. The guard is harmless but misleading -- it suggests `full_path` could be the first field. A cleaner implementation:

```awk
if (include_path == 1) {
    printf ",\"full_path\":\"%s\"", prev_file
}
```

This goes between line 1363 (`}` closing the for loop) and line 1364 (`printf "}\n"`), which is inside the `if (field_count > 0)` block. The comma is always correct because at least one field precedes it.

**Recommendation**: Use the simplified version above. The implementer should note the placement is inside the `if (field_count > 0)` block, so `full_path` is only emitted when there are actual fields.

### 2. BDD Scenario 2: jq filter quoting concern

The plan proposes:
```gherkin
When I run "ticket query --include-full-path '.status == \"open\"'"
```

This matches the existing pattern in `ticket_query.feature` line 22:
```gherkin
When I run "ticket query '.status == \"open\"'"
```

So this is correct. No issue here -- just confirming it follows the existing convention.

### 3. BDD Scenario 3 wording suggestion

The regression guard scenario:
```gherkin
Scenario: Query without --include-full-path excludes file path
```

The assertion `the output should not contain "full_path"` is good but note it checks the raw string `full_path` anywhere in stdout. If a ticket's title or description happened to contain the literal text "full_path", this test would be fragile. In practice, the test creates tickets with titles like "No path ticket" so this is fine. No change needed.

## Simplification Opportunities (PARETO)

None -- the plan is already minimal. Four files changed, all additive. This is as surgical as it gets.

## Strengths

- **Correct pattern matching**: The flag-parsing while-loop correctly mirrors the `cmd_ls()` pattern at line 653-664 of `/home/nickolaykondratyev/git_repos/wedow_ticket/ticket`.
- **Verified assumptions**: `TICKETS_DIR` is always absolute for read commands because `find_tickets_dir()` walks from `$PWD` (line 13). The write-command fallback to `".tickets"` (line 49) does not apply since `query` is not in `WRITE_COMMANDS`.
- **Existing step definitions confirmed**: All BDD steps used in the proposed scenarios exist in `/home/nickolaykondratyev/git_repos/wedow_ticket/features/steps/ticket_steps.py`:
  - `JSONL output should have field "<field>"` (line 571)
  - `the output should contain` (line 359)
  - `the output should not contain` (line 366)
  - `ticket "<id>" has status "<status>"` (line 102)
- **No new step definitions needed**: Correct assessment.
- **Three BDD scenarios cover the space well**: positive, combined with filter, and negative/regression.
- **JSON escaping**: Correctly identified as not worth solving -- consistent with existing behavior at line 1361 where field values are not escaped either.

## Requirements Coverage Check

| Requirement | Covered | Notes |
|---|---|---|
| Add `--include-full-path` flag to `tk query` | Yes | Phase 1 |
| Update help text in `cmd_help` | Yes | Phase 2 |
| Update `tk help` to reflect change | Yes | Same as Phase 2 (cmd_help IS tk help) |
| Add BDD scenarios | Yes | Phase 4 -- 3 scenarios |
| Update README usage block | Yes | Phase 3 |

All five requirements are addressed.

## Verdict

- [x] APPROVED WITH MINOR REVISIONS

The minor revision (simplify the comma guard in AWK) can be applied inline by the implementer. No plan iteration needed -- proceed directly to implementation.
