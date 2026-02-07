# Pareto Complexity Analysis: `--include-full-path` flag for `tk query`

## Pareto Assessment: PROCEED

**Value Delivered:** Enables programmatic consumers (AI agents, scripts) to map JSON ticket data back to the source file on disk -- a capability that cannot be reconstructed from `tk query` output alone without reimplementing the ID-to-path resolution logic.

**Complexity Cost:** 40 lines added, 4 lines removed across 4 files. The core logic change is 9 net new lines of bash/awk.

**Ratio:** High

## Analysis

### Value/Complexity Ratio

This is a textbook 80/20 implementation. The entire feature is delivered with:

- **6 lines** of flag parsing (a standard `while/case` loop matching the existing codebase patterns)
- **3 lines** of AWK logic (conditional printf of one additional JSON field)
- **2 lines** of help text updates in `ticket` and `README.md`
- **1 line** of changelog

The feature solves a real gap: `tk query` outputs structured ticket data but discards the file location. Downstream tooling that needs to read/modify the source file (common for AI agents) would otherwise need to reimplement `ticket_path()` resolution. This flag eliminates that duplication.

### Scope Creep Detection

None detected. The implementation touches exactly the files it should and nothing more. There is no tangential feature work, no refactoring of unrelated code, and no new abstractions.

### Premature Abstraction

None. The flag is passed directly as an AWK variable (`-v include_path="$include_full_path"`) and checked inline. There is no options framework, no configuration layer, no generalized "field inclusion" mechanism. This is the simplest possible approach.

### Integration Cost

Near zero. The change is fully additive and opt-in:
- Without `--include-full-path`, output is byte-identical to before.
- The flag is parsed in a local `while` loop scoped to `cmd_query()`.
- No other commands or subsystems are affected.
- The regression test (scenario: "Query without --include-full-path excludes file path") explicitly guards backward compatibility.

### Test Coverage Assessment

Three BDD scenarios are proportional to the feature's surface area:

1. Flag alone -- verifies the field appears and contains the expected filename
2. Flag with jq filter -- verifies flag and filter compose correctly (this is the key integration point)
3. Absence regression -- verifies the field does not leak into default output

This is sufficient. The feature has one boolean code path and one integration point (jq filter composition). Three scenarios cover both branches and the integration. More tests would be over-testing.

### Minor Observations (not blocking)

- The reviewer noted that file paths are not JSON-escaped. This is a pre-existing limitation across all fields in the AWK emitter and not introduced by this change. Fixing it here would violate the principle of minimal, focused changes. The risk is near-zero since `.tickets/` paths are controlled by the filesystem and ticket ID generator.
- The `while` loop treats any non-flag argument as the jq filter, silently accepting multiple positional arguments (last one wins). This matches the previous behavior where `$1` was used directly, and adding validation would be out of scope.

## Recommendation

Proceed as-is. This is a clean, minimal, well-tested feature addition. The implementation complexity is proportional to the value, follows existing codebase patterns, and introduces no unnecessary abstractions or scope creep.
