## Context

`tk` is a single-file bash script with a plugin system. Plugins are executables named `ticket-<cmd>` in PATH. The existing plugins (ticket-query, ticket-ls, ticket-edit, ticket-migrate-beads) demonstrate the pattern: receive `TICKETS_DIR` and `TK_SCRIPT` env vars, call back to core via `$TK_SCRIPT super <cmd>`.

Beads provides formulas (TOML workflow templates) and molecules (instantiated workflows). We want the template functionality without the molecule tracking overhead.

## Goals / Non-Goals

**Goals:**
- TOML workflow definitions compatible with beads formula syntax (s/formula/workflow/)
- `tk workflow list` to discover available workflows from project and user directories
- `tk workflow run <name> --var key=value` to instantiate a workflow as tickets with dependencies
- Variable substitution in step titles and descriptions using `{{var}}` syntax

**Non-Goals:**
- Gates (human/timer/GitHub async coordination)
- Wisps (ephemeral operations)
- Molecule tracking (no persistent workflow instance state beyond the created tickets)
- Bond points / aspect composition
- Step hooks (on_complete actions)
- TOML library dependency — we parse a constrained subset with awk/sed

## Decisions

### 1. Single plugin file with subcommands

The plugin `ticket-workflow` handles `list` and `run` as subcommands via `$1` dispatch. This keeps the plugin self-contained.

Alternative: Separate `ticket-workflow-list` and `ticket-workflow-run` plugins. Rejected because `tk workflow list` naturally routes to `ticket-workflow` with `list` as an argument — the plugin system already handles this.

### 2. TOML parsing with awk/sed (no external deps)

We parse a constrained TOML subset sufficient for workflow definitions: top-level key-value pairs, `[vars.*]` sections, and `[[steps]]` array tables. This keeps the zero-dependency philosophy.

Alternative: Require `tomlq` or Python. Rejected to maintain the coreutils-only requirement.

### 3. Search path: project then user

Workflows are searched in order:
1. `.tickets/workflows/*.toml` (project-level, version controlled)
2. `~/.config/ticket/workflows/*.toml` (user-level, personal)

This mirrors the beads formula search path pattern. Project workflows take precedence.

### 4. Workflow instantiation creates a parent ticket + child tickets

`tk workflow run release --var version=1.0.0` creates:
- A parent ticket with the workflow name/description as title
- Child tickets for each step, with `--parent` set to the parent ID
- Dependencies between child tickets based on `needs` declarations

This maps directly to existing `tk create` and `tk dep` commands.

### 5. Variable validation at run time

Required variables without defaults cause an error if not provided via `--var`. Pattern and enum constraints are validated before any tickets are created. This prevents partial workflow instantiation.

## Risks / Trade-offs

- **[Constrained TOML parsing]** → Only supports the subset needed for workflows. Malformed TOML outside this subset may produce confusing errors. Mitigation: clear error messages for parse failures, document supported syntax.
- **[No molecule state]** → Once tickets are created, there's no record linking them back to the workflow template. Mitigation: the parent-child relationship and a tag (workflow name) on created tickets provides sufficient traceability.
- **[Step type field ignored]** → We parse `type` on steps but don't enforce gate/human semantics. All steps become regular tickets. Mitigation: document that `type` is informational only.
