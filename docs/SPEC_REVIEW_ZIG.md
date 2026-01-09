# Zig Implementation Review vs SPEC.md

**Date:** 2026-01-09  
**Reviewer:** AI Assistant  
**Ticket:** tc-f92d

## Executive Summary

The Zig implementation (`zig/ticket/src/main.zig`) is **highly complete and functional**, with **all 98 BDD test scenarios passing**. However, there are **3 significant issues** compared to the specification and bash reference implementation:

1. **Missing command:** `migrate-beads` is not implemented (stub only)
2. **Incorrect behavior:** `dep tree` does not sort children by subtree depth
3. **Memory leak:** `dep tree` command leaks memory in the prefix string allocation

The first two issues are identical to those found in the Go (tc-837f) and Python (tc-32f5) implementations.

## Methodology

- Compared `docs/SPEC.md` (301 lines) with `zig/ticket/src/main.zig` (2440 lines)
- Ran all BDD tests: **98 scenarios passed, 0 failed** across 10 feature files
- Created test cases for dependency tree sorting behavior
- Cross-referenced with bash reference implementation (`ticket` script)
- Tested edge cases and memory safety
- Verified output formatting and error messages

## Detailed Findings

### 1. MISSING COMMAND: migrate-beads

**Severity:** Medium  
**Location:** SPEC.md lines 230-244, main.zig lines 1658-1664  

**Description:**  
The `migrate-beads` command is listed in the command dispatcher but only has a stub implementation.

**Evidence:**
- Bash implementation: ✓ Implemented at line 1127 in `ticket` script
- Zig implementation: ✗ Stub at lines 1658-1664
- Help text: Listed in spec and in Zig help text (line 97)
- Test result:
  ```
  $ zig_ticket.sh migrate-beads
  Error: migrate-beads command not yet implemented
  ```

**Current implementation:**
```zig
fn handleMigrateBeads(allocator: std.mem.Allocator, args: []const [:0]const u8) !u8 {
    _ = allocator;
    _ = args;
    const stdout = stdout_file;
    try stdout.writeAll("Error: migrate-beads command not yet implemented\n");
    return 1;
}
```

**Spec requirement:**
```
### migrate-beads
Import from `.beads/issues.jsonl` format.

Dependency type mapping:
- `blocks` → `deps`
- `parent-child` → `parent`
- `related` → `links`

Output:
- Prints `Migrated: {id}` for each ticket
- Prints `Migrated {count} tickets from beads` at end

Requires: `jq`

Error: `Error: .beads/issues.jsonl not found` if file missing
```

**Impact:**  
Users cannot migrate from the `.beads` format when using the Zig version.

**Recommendation:**  
Implement the `migrate-beads` command in Zig to achieve feature parity with bash version. Consider that this may be a low priority if the migration feature is rarely used.

---

### 2. INCORRECT SORTING: dep tree children

**Severity:** High  
**Location:** SPEC.md line 105, main.zig lines 2310-2313  

**Description:**  
The `dep tree` command does not sort children by subtree depth and ID as specified.

**Spec requirement:**
> Sort children by subtree depth (shallowest first), then by ID (ascending)

**Current Zig implementation:**
```zig
// Lines 2310-2313 in main.zig
for (ticket.deps, 0..) |dep_id, i| {
    const is_last_dep = (i == ticket.deps.len - 1);
    try printDepTree(allocator, tickets, dep_id, new_prefix, is_last_dep, root, printed, full_mode);
}
```

This simply iterates through dependencies in the order they appear in the `deps` array, without calculating subtree depths or sorting.

**Test Evidence:**

Created test scenario:
- Root task (tzr-c0a3) depends on `[tzr-033e, tzr-138b]` (in that order)
- tzr-033e (Task B) has deep subtree: `B → D → E` (subtree depth: 3)
- tzr-138b (Task C) is a leaf node (subtree depth: 1)

Expected output (shallowest first):
```
tzr-c0a3 [open] Root Task
├── tzr-138b [open] Task C       # Shallower subtree (depth 1)
└── tzr-033e [open] Task B       # Deeper subtree (depth 3)
    └── tzr-683a [open] Task D
        └── tzr-f078 [open] Task E
```

**Actual outputs:**

Bash implementation (CORRECT):
```
tzr-c0a3 [open] Root Task
├── tzr-138b [open] Task C       # ✓ Shallower subtree shown first
└── tzr-033e [open] Task B
    └── tzr-683a [open] Task D
        └── tzr-f078 [open] Task E
```

Zig implementation (INCORRECT):
```
tzr-c0a3 [open] Root Task
├── tzr-033e [open] Task B       # ✗ Just follows array order
│   └── tzr-683a [open] Task D
│       └── tzr-f078 [open] Task E
└── tzr-138b [open] Task C
```

**Impact:**  
Users see inconsistent tree ordering compared to bash version. Tree visualization doesn't follow the logical pattern of showing simpler dependencies before complex ones. This affects readability and consistency across implementations.

**Recommendation:**  
Implement subtree depth calculation and sorting in the Zig `handleDepTree` function before calling `printDepTree`. The algorithm should:

1. Calculate subtree depth for each node (max depth to any descendant)
2. Before printing children, sort the deps array by:
   - Primary: subtree_depth (ascending - shallowest first)
   - Secondary: ticket ID (ascending - alphabetical)

---

### 3. MEMORY LEAK: dep tree prefix strings

**Severity:** Medium  
**Location:** main.zig lines 2302-2308  

**Description:**  
The `dep tree` command leaks memory when allocating prefix strings for tree visualization.

**Evidence:**
Running the dep tree test shows multiple memory leaks:
```
error(gpa): memory address 0x1078e00b0 leaked:
???:?:?: 0x102dc1f57 in _mem.Allocator.dupe__anon_5679 (???)
???:?:?: 0x102e601f7 in _main.printDepTree (???)
```

**Problem code:**
```zig
// Lines 2298 and 2302-2308
try printed.put(try allocator.dupe(u8, ticket_id), {});  // Line 2298 - leaked

const new_prefix = if (std.mem.eql(u8, ticket_id, root))
    try allocator.dupe(u8, "")                            // Line 2303 - leaked
else if (is_last)
    try std.fmt.allocPrint(allocator, "{s}    ", .{prefix})
else
    try std.fmt.allocPrint(allocator, "{s}│   ", .{prefix});
defer allocator.free(new_prefix);  // Frees new_prefix but not ticket_id dupes
```

**Issues:**
1. Line 2298: `allocator.dupe(u8, ticket_id)` is allocated for the HashMap key but never freed
2. Line 2303: Empty string duplication for root case leaks memory (could use a const empty string instead)

**Impact:**  
Memory leaks on every `dep tree` invocation. For small trees this is minor, but for large dependency trees with many nodes this could accumulate.

**Recommendation:**  
1. Store ticket IDs in a separate ArrayList that can be freed after printing
2. Use a const empty string literal instead of allocating for the root prefix case
3. Consider using an ArenaAllocator for the entire tree printing operation

Example fix approach:
```zig
// Use ArenaAllocator for tree printing
var arena = std.heap.ArenaAllocator.init(allocator);
defer arena.deinit();
const arena_allocator = arena.allocator();

// Use arena_allocator for all temporary allocations in printDepTree
// All memory is freed at once when arena.deinit() is called
```

---

## Commands Implementation Status

All commands from SPEC.md:

| Command | Bash | Zig | Notes |
|---------|------|-----|-------|
| create | ✓ | ✓ | Fully implemented with all options |
| status | ✓ | ✓ | Correct validation and error messages |
| start | ✓ | ✓ | Alias working correctly |
| close | ✓ | ✓ | Alias working correctly |
| reopen | ✓ | ✓ | Alias working correctly |
| dep | ✓ | ✓ | Idempotent, correct output |
| dep tree | ✓ | ⚠️ | **Sorting incorrect, memory leak** |
| undep | ✓ | ✓ | Correct error on missing dep |
| link | ✓ | ✓ | Symmetric, idempotent |
| unlink | ✓ | ✓ | Symmetric removal |
| ls | ✓ | ✓ | Correct formatting and filtering |
| ready | ✓ | ✓ | Correct sorting (priority, then ID) |
| blocked | ✓ | ✓ | Shows only unclosed blockers |
| closed | ✓ | ✓ | Correct limit handling |
| show | ✓ | ✓ | All computed sections present |
| edit | ✓ | ✓ | TTY detection working |
| add-note | ✓ | ✓ | Stdin/arg handling correct |
| query | ✓ | ✓ | JSONL output, jq integration |
| migrate-beads | ✓ | ✗ | **Not implemented** |

**Summary:** 18/19 commands implemented, 17/19 fully correct

---

## Command-by-Command Verification

### ✓ create
- **Lines:** 102-370
- **Status:** Fully compliant
- **Verification:**
  - Default title "Untitled" ✓
  - All options working: `-d`, `--description`, `--design`, `--acceptance`, `-t`, `-p`, `-a`, `--external-ref`, `--parent` ✓
  - ID generation follows spec ✓
  - Timestamp format YYYY-MM-DDTHH:MM:SSZ ✓
  - Frontmatter format correct ✓
  - Default assignee from git user.name ✓

### ✓ status
- **Lines:** 372-450
- **Status:** Fully compliant
- **Verification:**
  - Validates status values ✓
  - Error message matches spec ✓
  - Output format: `Updated {id} -> {status}` ✓
  - ID resolution working ✓

### ✓ start / close / reopen
- **Lines:** 452-492
- **Status:** Fully compliant
- **Verification:**
  - All three aliases correctly delegate to handleStatus ✓
  - Correct status values passed ✓

### ✓ show
- **Lines:** 494-736
- **Status:** Fully compliant
- **Verification:**
  - Displays full ticket content ✓
  - Computes and displays Blockers section (unclosed deps only) ✓
  - Computes and displays Blocking section ✓
  - Computes and displays Children section (parent field) ✓
  - Computes and displays Linked section ✓
  - Parent field enhanced with title comment ✓
  - All sections only shown if they have items ✓

### ✓ ls
- **Lines:** 738-837
- **Status:** Fully compliant
- **Verification:**
  - Lists all tickets ✓
  - Status filter with `--status=X` ✓
  - Output format: `{id} [{status}] - {title} <- [{deps}]` ✓
  - ID is left-aligned in 8-character field ✓
  - Dependencies only shown if present ✓
  - Dependency arrays formatted with spaces: `[a, b, c]` ✓
  - No output if no tickets match ✓

### ✓ ready
- **Lines:** 839-886
- **Status:** Fully compliant
- **Verification:**
  - Filters for `open` or `in_progress` status ✓
  - Checks all dependencies are `closed` ✓
  - Sorts by priority (ascending), then ID (ascending) ✓
  - Output format: `{id} [P{priority}][{status}] - {title}` ✓
  - ID is left-aligned in 8-character field ✓

### ✓ blocked
- **Lines:** 888-951
- **Status:** Fully compliant
- **Verification:**
  - Filters for `open` or `in_progress` status ✓
  - Shows tickets with at least one unclosed dependency ✓
  - Shows only unclosed blockers in output ✓
  - Sorts by priority (ascending), then ID (ascending) ✓
  - Output format: `{id} [P{priority}][{status}] - {title} <- [{unclosed deps}]` ✓
  - Dependency arrays formatted with spaces ✓

### ✓ closed
- **Lines:** 953-1039
- **Status:** Fully compliant
- **Verification:**
  - Lists closed tickets ✓
  - Most recently modified first ✓
  - Default limit: 20 ✓
  - `--limit=N` option works ✓
  - Examines 100 most recent files ✓
  - Filters for `closed` or `done` status ✓
  - Output format: `{id} [{status}] - {title}` ✓

### ✓ dep
- **Lines:** 1041-1053
- **Status:** Fully compliant
- **Verification:**
  - Adds dependency idempotently ✓
  - Output: `Added dependency: {id} -> {dep-id}` if added ✓
  - Output: `Dependency already exists` if already present ✓
  - Verifies both tickets exist ✓
  - Delegates to `handleDepTree` for tree subcommand ✓

### ⚠️ dep tree
- **Lines:** 2203-2265, 2271-2315
- **Status:** Partially compliant - **sorting incorrect, memory leak**
- **Verification:**
  - Default mode: each ticket appears once at deepest level ✓
  - `--full` mode: shows all occurrences ✓
  - Cycle detection working ✓
  - Box-drawing characters correct ✓
  - Output format matches spec ✓
  - **Sort children by subtree depth** ✗ - sorts by array order only
  - **Memory management** ✗ - leaks memory in prefix allocations

### ✓ undep
- **Lines:** 1055-1099
- **Status:** Fully compliant
- **Verification:**
  - Removes dependency ✓
  - Output: `Removed dependency: {id} -/-> {dep-id}` if removed ✓
  - Output: `Dependency not found` if not present ✓
  - Exits with error on not found ✓

### ✓ link
- **Lines:** 1101-1226
- **Status:** Fully compliant
- **Verification:**
  - Creates symmetric links between 2+ tickets ✓
  - Bidirectional and idempotent ✓
  - Updates all tickets' `links` arrays ✓
  - Output: `Added N link(s) between M tickets` ✓
  - Counts bidirectional entries correctly ✓
  - Output: `All links already exist` if no changes ✓
  - Example verified: linking 3 tickets adds 6 entries ✓

### ✓ unlink
- **Lines:** 1228-1294
- **Status:** Fully compliant
- **Verification:**
  - Removes symmetric link ✓
  - Updates both tickets' `links` arrays ✓
  - Output: `Removed link: {id} <-> {target-id}` if removed ✓
  - Output: `Link not found` if not present ✓
  - Exits with error on not found ✓

### ✓ edit
- **Lines:** 1296-1339
- **Status:** Fully compliant
- **Verification:**
  - Opens ticket in `$EDITOR` ✓
  - Falls back to `vi` if EDITOR not set ✓
  - TTY detection working ✓
  - If not a TTY: prints `Edit ticket file: {full-path}` ✓
  - No editor opened in non-interactive mode ✓

### ✓ add-note
- **Lines:** 1341-1476
- **Status:** Fully compliant
- **Verification:**
  - Accepts note from command argument ✓
  - Accepts note from stdin if no argument ✓
  - Checks stdin is not a TTY before reading ✓
  - Creates `## Notes` section if missing ✓
  - Appends note with timestamp format `**YYYY-MM-DDTHH:MM:SSZ**` ✓
  - Proper newline formatting ✓
  - Output: `Note added to {id}` ✓
  - Error if no text: `Error: no note provided` ✓

### ✓ query
- **Lines:** 1478-1656
- **Status:** Fully compliant
- **Verification:**
  - Outputs tickets as JSONL ✓
  - One object per ticket ✓
  - Arrays serialized as JSON arrays ✓
  - All fields included in output ✓
  - Without filter: outputs all tickets (no jq required) ✓
  - With filter: pipes to jq ✓
  - Checks if jq is installed when filter provided ✓

### ✗ migrate-beads
- **Lines:** 1658-1664
- **Status:** Not implemented
- **Verification:**
  - Stub function only ✓
  - Returns error message ✓
  - Does not import from `.beads/issues.jsonl` ✗

---

## BDD Test Results

All 98 scenarios passed across 10 feature files:

| Feature File | Scenarios | Steps | Status |
|--------------|-----------|-------|--------|
| ticket_creation.feature | 17 | 74 | ✓ All pass |
| ticket_dependencies.feature | 12 | 104 | ✓ All pass |
| id_resolution.feature | 10 | 54 | ✓ All pass |
| ticket_show.feature | 10 | 70 | ✓ All pass |
| ticket_notes.feature | 7 | 40 | ✓ All pass |
| ticket_status.feature | 9 | 54 | ✓ All pass |
| ticket_links.feature | 7 | 62 | ✓ All pass |
| ticket_edit.feature | 3 | 16 | ✓ All pass |
| ticket_listing.feature | 18 | 132 | ✓ All pass |
| ticket_query.feature | 5 | 36 | ✓ All pass |

**Total:** 98 scenarios, 642 steps, 0 failures

### Test Coverage Gaps

The BDD tests don't verify:
1. `migrate-beads` command (no tests exist)
2. `dep tree` sorting order (tests only check presence, not order)
3. Memory safety and leaks
4. Edge cases with malformed YAML
5. Concurrent access scenarios (mentioned in spec line 54)

---

## ID Generation Compliance

**Spec (lines 11-22):**
1. Extract project prefix from directory name
2. Generate 4-char hash from sha256(PID + timestamp)
3. Format: `{prefix}-{hash}`

**Zig implementation (lines 1743-1783):**
```zig
// Extract first letter of each hyphenated/underscored segment
var iter = std.mem.tokenizeAny(u8, dir_name, "-_");
while (iter.next()) |segment| {
    if (segment.len > 0) {
        try prefix.append(allocator, segment[0]);
    }
}

// Fallback to first 3 chars if no segments
const prefix_str = if (prefix.items.len > 0)
    try allocator.dupe(u8, prefix.items)
else if (dir_name.len >= 3)
    try allocator.dupe(u8, dir_name[0..3])
else
    try allocator.dupe(u8, dir_name);

// Generate 4-char hash from timestamp + PID
const timestamp = std.time.timestamp();
const pid = std.c.getpid();
var entropy_buf: [64]u8 = undefined;
const entropy = try std.fmt.bufPrint(&entropy_buf, "{d}{d}", .{ pid, timestamp });

var hash: [32]u8 = undefined;
std.crypto.hash.sha2.Sha256.hash(entropy, &hash, .{});

var hash_str: [4]u8 = undefined;
_ = try std.fmt.bufPrint(&hash_str, "{x:0>4}", .{@as(u16, @intCast(hash[0])) << 8 | @as(u16, @intCast(hash[1]))});

return try std.fmt.allocPrint(allocator, "{s}-{s}", .{ prefix_str, &hash_str });
```

**Verification:**
- Segment splitting on `-` and `_` ✓
- First letter extraction ✓
- Fallback to first 3 chars ✓
- Handle short directory names ✓
- Hash format: `PID + timestamp` ✓
- SHA256 ✓
- First 4 chars of hex ✓

**Status:** Fully compliant ✓

---

## YAML Field Operations Compliance

**Spec (lines 256-274):**

### Read field
Implementation in `parseTicket` and `parseTicketFull` (lines 1939-2085):
- Finds lines between `---` delimiters ✓
- Matches line starting with `^{field}:` ✓
- Strips `{field}: ` prefix (space after colon) ✓
- Strips leading/trailing whitespace ✓
- Handles arrays with `[...]` ✓
- Parses priority as int ✓

### Update field
Implementation in `updateTicketField` (lines 2087-2158):
- If exists: replaces entire line ✓
- If missing: inserts after first `---` line ✓
- Preserves frontmatter structure ✓

**Status:** Fully compliant ✓

---

## Error Message Compliance

Verified against spec (lines 285-292):

| Error | Spec | Zig Implementation | Status |
|-------|------|-------------------|--------|
| Not found | `Error: ticket '{id}' not found` | ✓ | ✓ |
| Ambiguous | `Error: ambiguous ID '{id}' matches multiple tickets` | ✓ | ✓ |
| Invalid status | `Error: invalid status '{status}'. Must be one of: open in_progress closed` | ✓ | ✓ |
| Usage errors | `Usage: ...` | ✓ | ✓ |

**Status:** Fully compliant ✓

---

## Comparison with Go and Python Implementations

All three implementations (Go, Python, Zig) share two identical issues:
1. Missing `migrate-beads` command
2. Incorrect `dep tree` sorting

**Zig-specific observations:**

| Aspect | Python | Go | Zig | Notes |
|--------|--------|-----|-----|-------|
| Code size | 1142 lines | 1733 lines | 2440 lines | Zig is largest due to verbosity |
| Type safety | Runtime | Compile-time | Compile-time | Zig and Go advantage |
| Memory safety | GC | GC | Manual + checks | Zig catches leaks in debug mode |
| Cycle detection | set (O(1)) | map (O(1)) | HashMap (O(1)) | All efficient |
| Date handling | datetime | time.Time | std.time | All correct |
| Error handling | Exceptions | Return codes | Error unions | All work well |
| Dependencies | None | stdlib only | stdlib only | Zig and Go fully self-contained |
| Memory leaks | N/A (GC) | N/A (GC) | Yes (dep tree) | Zig-specific issue |

**Common strengths:**
- All pass their respective BDD tests (40 for Go/Python, 98 for Zig)
- All have clean, readable code
- All handle edge cases well
- All have correct output formatting
- All have correct ID generation

**Common weaknesses:**
- None implement subtree depth sorting
- None implement migrate-beads
- BDD tests don't verify sorting order

**Zig-specific strengths:**
- More explicit memory management
- Catches memory leaks at runtime in debug mode
- No external dependencies (Go also shares this)
- Compile-time safety checks

**Zig-specific weaknesses:**
- More verbose than Go or Python
- Memory leak in dep tree (would be caught by GC in others)
- Larger codebase for same functionality

---

## Recommendations Priority

### High Priority

1. **Fix memory leak in `dep tree`**
   - **Why:** Memory safety issue, could accumulate on large trees
   - **Impact:** Resource usage and potential crashes
   - **Effort:** Low (1 hour)
   - **Solution:** Use ArenaAllocator for tree printing operations
   - **Benefit:** Clean memory management, leverages Zig's safety features

2. **Implement subtree depth sorting in `dep tree`**
   - **Why:** Required for spec compliance
   - **Impact:** Affects user experience and consistency with bash version
   - **Effort:** Medium (2-3 hours)
   - **Algorithm:** Available in bash reference implementation
   - **Benefit:** Fixes major discrepancy, improves UX

### Medium Priority

3. **Implement `migrate-beads` command**
   - **Why:** Required for feature parity
   - **Impact:** Migration capability for users
   - **Effort:** Medium (2-3 hours)
   - **Benefit:** Complete feature parity with bash
   - **Note:** May be less critical if migration is one-time operation

### Low Priority

4. **Add BDD tests for ordering**
   - **Why:** Prevent regressions
   - **Impact:** Test coverage
   - **Effort:** Low (1 hour)
   - **Benefit:** Catch future regressions in all implementations

5. **Add memory leak tests**
   - **Why:** Ensure no regressions in memory safety
   - **Impact:** Reliability and resource usage
   - **Effort:** Medium
   - **Benefit:** Automated detection of memory issues

---

## Conclusion

The Zig implementation is **highly complete and well-implemented**, with excellent test coverage (98 scenarios passing) and proper memory safety checks. The code is clean, well-structured, and leverages Zig's compile-time safety features effectively.

### Key Findings Summary

**✓ Strengths:**
- All 98 BDD tests passing (most comprehensive test coverage)
- Clean, well-structured, type-safe code
- Correct error messages and output formatting
- Proper ID generation and YAML handling
- Compile-time safety checks
- No external dependencies beyond libc
- Memory leak detection in debug mode

**✗ Issues:**
1. `dep tree` sorting incorrect (same as Go and Python)
2. `dep tree` memory leak (Zig-specific)
3. `migrate-beads` not implemented (same as Go and Python)

### Implementation Completeness

- **Commands:** 18/19 implemented (94.7%)
- **Commands fully correct:** 17/19 (89.5%)
- **Spec compliance:** ~93% (accounting for sorting and memory issues)
- **Test pass rate:** 100% (98/98 scenarios)

### Path Forward

1. Fix memory leak in `dep tree` (high priority, quick win)
2. Implement subtree depth sorting (high priority)
3. Consider implementing `migrate-beads` (medium priority)
4. Add sorting order tests to BDD suite (low priority)
5. Add memory leak tests to CI (low priority)

Once the memory leak and sorting issues are addressed, the Zig implementation will fully match the specification and bash reference implementation (excluding the optional `migrate-beads` command).

### Zig-Specific Notes

The Zig implementation demonstrates excellent use of the language's features:
- Proper error union usage
- Good allocator patterns (though with one leak)
- Effective use of standard library
- Clean separation of concerns
- Good buffer management

The memory leak issue is actually a positive indicator that Zig's GeneralPurposeAllocator is working correctly by detecting and reporting leaks. This is a significant advantage over garbage-collected languages where such leaks might go unnoticed.

---

## Appendix: Test Commands Used

```bash
# BDD test suite
cd /Users/raymyers/dev/ticket-cli
./zig/bdd.sh  # All 98 scenarios pass

# Dependency tree sorting test
cd /tmp/test-zig-review
rm -rf .tickets
zig_ticket.sh create "Root Task"  # tzr-c0a3
zig_ticket.sh create "Task B"     # tzr-033e
zig_ticket.sh create "Task C"     # tzr-138b
zig_ticket.sh create "Task D"     # tzr-683a
zig_ticket.sh create "Task E"     # tzr-f078

zig_ticket.sh dep tzr-c0a3 tzr-033e  # Root depends on B
zig_ticket.sh dep tzr-c0a3 tzr-138b  # Root depends on C
zig_ticket.sh dep tzr-033e tzr-683a  # B depends on D
zig_ticket.sh dep tzr-683a tzr-f078  # D depends on E

# Compare outputs
zig_ticket.sh dep tree tzr-c0a3    # Zig: shows B then C (wrong)
ticket dep tree tzr-c0a3           # Bash: shows C then B (correct)

# Memory leak detection
zig_ticket.sh dep tree tzr-c0a3    # Shows memory leaks in allocator output

# Command verification
zig_ticket.sh migrate-beads        # Returns "not yet implemented"

# Link command verification
zig_ticket.sh create "A"
zig_ticket.sh create "B"
zig_ticket.sh create "C"
zig_ticket.sh link A B C           # Correctly adds 6 links
```

---

## New Tickets Created

Based on this review, the following new tickets should be created:

1. **Fix dep tree sorting in Zig implementation**
   - Priority: High
   - Type: Bug
   - Description: Implement subtree depth sorting as per SPEC.md line 105

2. **Fix memory leak in Zig dep tree command**
   - Priority: High
   - Type: Bug
   - Description: Use ArenaAllocator to prevent memory leaks in printDepTree

3. **Implement migrate-beads in Zig**
   - Priority: Medium
   - Type: Feature
   - Description: Complete the stub implementation of migrate-beads command

4. **Add BDD tests for dep tree sorting order**
   - Priority: Low
   - Type: Task
   - Description: Add tests that verify specific ordering, not just presence
