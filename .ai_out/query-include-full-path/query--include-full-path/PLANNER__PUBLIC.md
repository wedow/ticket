# Implementation Plan: `--include-full-path` flag for `tk query`

## 1. Problem Understanding

**Goal**: Add `--include-full-path` flag to the built-in `tk query` command so each JSONL object includes a `"full_path"` key with the absolute path to the ticket's `.md` file.

**Constraints**:
- Surgical changes only -- this is someone else's repo, must be easy to review/merge
- Follow existing patterns (flag parsing, AWK-based JSON generation, BDD test style)
- No new dependencies

**Assumptions**:
- `full_path` should be the absolute filesystem path (e.g., `/home/user/project/.tickets/foo-1234.md`)
- The flag works both standalone (`tk query --include-full-path`) and combined with a jq filter (`tk query --include-full-path '.status == "open"'`)

## 2. Architecture

No architectural changes. This is a single flag addition to an existing command. The AWK-based JSON generator already has access to the file path via its `FILENAME`/`prev_file` variable, so we pass a flag variable into AWK and conditionally append the field.

**Data flow**:
```
cmd_query() parses --include-full-path flag
  -> passes include_path=1/0 to AWK
  -> AWK emit() conditionally appends "full_path":"<prev_file>" to JSON
  -> optional jq filter applied (unchanged)
```

## 3. Implementation Phases

### Phase 1: Modify `cmd_query()` in `/home/nickolaykondratyev/git_repos/wedow_ticket/ticket`

**Goal**: Parse the `--include-full-path` flag and pass it to AWK.

**Lines affected**: 1321-1375

**Key steps**:

1. Replace the simple `local filter="${1:-}"` with a flag-parsing loop (matching the `cmd_ls` pattern):
   ```
   local filter="" include_full_path=0
   while [[ $# -gt 0 ]]; do
       case "$1" in
           --include-full-path) include_full_path=1; shift ;;
           *) filter="$1"; shift ;;
       esac
   done
   ```

2. Pass the `include_full_path` variable into AWK via `-v include_path="$include_full_path"`.

3. In the AWK `emit()` function, after the existing field loop closes (after line 1363's closing `}`), add a conditional block that appends `,"full_path":"<prev_file>"` when `include_path == 1`:
   ```awk
   if (include_path == 1) {
       if (field_count > 0) printf ","
       printf "\"full_path\":\"%s\"", prev_file
   }
   ```
   This goes right before the `printf "}\n"` line.

**Verification**: Run `tk query --include-full-path` and confirm each JSON line has `"full_path":"/absolute/path/to/ticket.md"`.

### Phase 2: Update help text in `cmd_help()`

**Goal**: Document the new flag.

**Lines affected**: Line 1505

**Key step**: Change line 1505 from:
```
  query [jq-filter]        Output tickets as JSON, optionally filtered
```
to:
```
  query [options] [jq-filter] Output tickets as JSON, optionally filtered
    --include-full-path      Include absolute file path in each JSON object
```

Follow the indentation pattern used by `create` flags (2 spaces + flag + description aligned).

### Phase 3: Update README.md usage block

**Goal**: Mirror the help text change.

**Lines affected**: Line 87 of `/home/nickolaykondratyev/git_repos/wedow_ticket/README.md`

**Key step**: Same change as Phase 2 -- update the `query` line and add the flag sub-line.

### Phase 4: Add BDD scenarios in `/home/nickolaykondratyev/git_repos/wedow_ticket/features/ticket_query.feature`

**Goal**: Test the new flag with and without jq filter.

**Key steps**: Add two scenarios at the end of the file:

**Scenario 1: Query with --include-full-path**
```gherkin
  Scenario: Query with --include-full-path includes file path
    Given a ticket exists with ID "query-001" and title "Path ticket"
    When I run "ticket query --include-full-path"
    Then the command should succeed
    And the output should be valid JSONL
    And the JSONL output should have field "full_path"
    And the output should contain "query-001.md"
```

**Scenario 2: Query with --include-full-path and jq filter**
```gherkin
  Scenario: Query with --include-full-path and jq filter
    Given a ticket exists with ID "query-001" and title "Open path ticket"
    And a ticket exists with ID "query-002" and title "Closed path ticket"
    And ticket "query-002" has status "closed"
    When I run "ticket query --include-full-path '.status == \"open\"'"
    Then the command should succeed
    And the JSONL output should have field "full_path"
    And the output should contain "query-001"
    And the output should not contain "query-002"
```

**Scenario 3: Query without --include-full-path does NOT include full_path** (regression guard)
```gherkin
  Scenario: Query without --include-full-path excludes file path
    Given a ticket exists with ID "query-001" and title "No path ticket"
    When I run "ticket query"
    Then the command should succeed
    And the output should not contain "full_path"
```

**Note**: The existing step `the JSONL output should have field "full_path"` already exists as a generic step in `ticket_steps.py` (line 571-581). The step `the output should contain "query-001.md"` uses the generic string-contains step. No new step definitions needed.

### Phase 5: Update CHANGELOG.md

**Goal**: Log the new feature under `[Unreleased]`.

**Lines affected**: After line 3 (under `### Added`)

**Key step**: Add:
```
- `query --include-full-path` flag to include absolute file path in JSON output
```

## 4. Technical Considerations

### AWK `FILENAME` is already absolute
Since `$TICKETS_DIR` is resolved to an absolute path by `find_tickets_dir()`, the glob `"$TICKETS_DIR"/*.md` expands to absolute paths. AWK's `FILENAME` captures these directly. No path manipulation needed.

### JSON escaping of file paths
File paths could theoretically contain characters that need JSON escaping (backslashes, quotes). In practice, `.tickets/` paths are controlled and safe. If paranoia is desired, AWK's `gsub(/\\/, "\\\\", prev_file); gsub(/"/, "\\\"", prev_file)` could be added before the printf -- but this is probably over-engineering for ticket file paths.

### Flag ordering
The while-loop parsing allows `--include-full-path` before or after the jq filter argument. Both `tk query --include-full-path '.status == "open"'` and `tk query '.status == "open"' --include-full-path` will work.

## 5. Testing Strategy

**Covered by BDD scenarios above**:
- `--include-full-path` produces JSONL with `full_path` field
- `full_path` value contains the actual `.md` filename
- Works in combination with jq filter
- Without the flag, `full_path` is absent (regression)

**Manual verification**:
- Run `make test` to execute all BDD tests
- Spot-check: `tk query --include-full-path | jq -r .full_path` should print absolute paths

## 6. Summary of Files Changed

| File | Change |
|------|--------|
| `/home/nickolaykondratyev/git_repos/wedow_ticket/ticket` | `cmd_query()`: add flag parsing + AWK variable; `cmd_help()`: add flag docs |
| `/home/nickolaykondratyev/git_repos/wedow_ticket/README.md` | Usage block: add flag docs |
| `/home/nickolaykondratyev/git_repos/wedow_ticket/features/ticket_query.feature` | 3 new scenarios |
| `/home/nickolaykondratyev/git_repos/wedow_ticket/CHANGELOG.md` | New entry under Unreleased |

Total: 4 files, all surgical additions, no rewrites.
