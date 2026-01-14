# AGENTS.md

See @README.md for usage documentation. Run `tk help` for command reference. Always update the README.md usage content when adding/changing commands and flags.

## Architecture

The ticket-cli project has five implementations:

### Bash (Reference Implementation)
Single-file bash implementation (~900 lines) at `./ticket`. Uses awk for performant bulk operations on large ticket sets.

Key functions:
- `generate_id()` - Creates IDs from directory name prefix + timestamp hash
- `ticket_path()` - Resolves partial IDs to full file paths
- `yaml_field()` / `update_yaml_field()` - YAML frontmatter manipulation via sed
- `cmd_*()` - Command handlers
- `cmd_ready()`, `cmd_blocked()`, `cmd_ls()` - awk-based bulk listing with sorting

Dependencies: bash, sed, awk, find. Optional: ripgrep (faster grep), jq (for query command).

### Language Ports

Five additional implementations have been created:

1. **Python** (`python/ticket/`) - Using uv, pytest, ruff. Entry point: `./py_ticket.sh`
2. **Go** (`go/ticket/`) - Using go modules. Entry point: `./go_ticket.sh`
3. **TypeScript** (`typescript/ticket/`) - Using Bun. Entry point: `./ts_ticket.sh`
4. **Zig** (`zig/ticket/`) - Native Zig. Entry point: `./zig_ticket.sh`
5. **ACL2/Common Lisp** (`acl2/ticket/`) - Using SBCL. Entry point: `./acl2_ticket.sh` (WIP - only `create` implemented)

Each implementation:
- Follows the specification in `docs/SPEC.md`
- Has its own BDD test suite (ported from Behave features)
- Has a wrapper script at the project root
- Has a `bdd.sh` script for running tests
- Is integrated into the root `Makefile`

**Spec Review Documents**: Each implementation has been reviewed against the specification:
- `docs/SPEC_REVIEW_PYTHON.md`
- `docs/SPEC_REVIEW_GO.md`
- `docs/SPEC_REVIEW_TYPESCRIPT.md`
- `docs/SPEC_REVIEW_ZIG.md`

**Porting Guide**: See `docs/NEW_IMPLEMENTATION.md` for instructions on porting to new languages.

## Behavior Driven Development

We use executable specifications using the Cucumber-like library Behave

To run the tests:
  uv run --with behave behave

  Or to run a specific feature:
  uv run --with behave behave features/ticket_creation.feature

Behave Tutorial
https://raw.githubusercontent.com/behave/behave/refs/heads/main/docs/tutorial.rst

If needed, full syntax reference here 
https://raw.githubusercontent.com/behave/behave/refs/heads/main/docs/gherkin.rst

## Changelog

When committing notable changes to the `ticket` script (new commands, flags, bug fixes, behavior changes), update CHANGELOG.md in the same commit:
- Create `## [Unreleased]` section at top if it doesn't exist
- Add bullet points under appropriate heading (Added, Fixed, Changed, Removed)
- Only script changes need logging; docs/workflow changes don't
