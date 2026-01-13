# AGENTS.md

Guidelines for AI agents working on the `ticket` codebase.

## Project Overview

Single-file bash script (`ticket`, ~1250 lines) implementing a minimal git-backed issue tracker. Installs as the `tk` command. Stores tickets as markdown files with YAML frontmatter in `.tickets/` directories.

MojoTech fork of the original [wedow/ticket](https://github.com/wedow/ticket) project.

## Commands

**There is no build step, test suite, or linter.** This is a portable bash script.

```bash
# Run the script directly
./ticket help

# Or if installed
tk help

# Manual testing - create test tickets
tk create "Test ticket" -d "Description"
tk ls
tk ready
tk blocked
```

**Verification**: After making changes, manually test affected commands. The script should work on both GNU/Linux and macOS (BSD).

## Architecture

### Key Functions

| Function | Purpose |
|----------|---------|
| `generate_id()` | Creates IDs from directory name prefix + timestamp hash |
| `ticket_path()` | Resolves partial IDs to full file paths |
| `yaml_field()` | Extract YAML frontmatter field value via sed |
| `update_yaml_field()` | Update YAML frontmatter field via sed |
| `cmd_*()` | Command handlers (one per CLI command) |
| `cmd_ready()`, `cmd_blocked()`, `cmd_ls()` | awk-based bulk listing with sorting |

### Portability Wrappers

The script must work on both GNU and BSD systems. Use these wrappers:

```bash
_grep()     # Uses ripgrep if available, falls back to grep
_sha256()   # Uses sha256sum (GNU) or shasum -a 256 (BSD)
_iso_date() # Portable ISO 8601 date format
_sed_i()    # Portable in-place sed (BSD requires temp file)
```

### Dependencies

- **Required**: bash, sed, awk, find, coreutils
- **Optional**: ripgrep (faster grep), jq (for `query` command)

## Code Style

### Shell Script Conventions

```bash
#!/usr/bin/env bash
set -euo pipefail  # Always at top - fail fast

local var="value"           # Use local variables in functions
"$variable"                 # Quote variables to prevent word splitting
[[ -f "$file" ]] && ...     # Use [[ ]] for conditionals
result=$(some_command)      # Command substitution with $()
```

### Function Naming

- Command handlers: `cmd_<command_name>()`
- Helper functions: lowercase with underscores
- Portable wrappers: `_<tool_name>()`

### YAML Manipulation

Use sed for YAML frontmatter. Do NOT introduce YAML parsing libraries.

```bash
yaml_field "$file" "status"              # Read field
update_yaml_field "$file" "status" "closed"  # Update field
```

### Awk Usage

Use awk for bulk operations on multiple files. Pattern:

```bash
awk '
BEGIN { FS=": "; in_front=0 }
FNR==1 { if (prev_file) store(); prev_file=FILENAME }
/^---$/ { in_front = !in_front; next }
in_front && /^field:/ { field = $2 }
function store() { /* save to arrays */ }
END { if (prev_file) store() }
' "$TICKETS_DIR"/*.md
```

### Error Handling

```bash
echo "Error: message" >&2; return 1      # Exit with error to stderr
if [[ $# -lt 1 ]]; then echo "Usage: ..." >&2; return 1; fi  # Validate early
count=$(echo "$matches" | grep -c . || true)  # || true for pipefail safety
```

## Documentation Requirements

### README.md

Update the Usage section when adding/changing commands and flags. Keep the command reference in sync with `cmd_help()`.

### CHANGELOG.md

When committing notable changes to the `ticket` script:

1. Create `## [Unreleased]` section at top if it doesn't exist
2. Add bullet points under: Added, Fixed, Changed, or Removed
3. Only script changes need logging (not docs/workflow changes)

### Releases

Before tagging a release:

1. Change `## [Unreleased]` to `## [X.Y.Z] - YYYY-MM-DD`
2. Commit changelog as part of the release
3. Tag with `vX.Y.Z`

The GitHub Actions workflow handles Nix derivation updates automatically.

## Common Patterns

### Adding a New Command

1. Add `cmd_<name>()` function
2. Add case to main dispatch at bottom of file
3. Add to `cmd_help()` output
4. Update README.md Usage section
5. Update CHANGELOG.md

### Adding a New Flag

1. Add to the argument parsing `while` loop in the command
2. Update `cmd_help()` output
3. Update README.md Usage section
4. Update CHANGELOG.md

### File Format

Tickets are markdown with YAML frontmatter:

```markdown
---
id: proj-a1b2
status: open
deps: []
links: []
created: 2026-01-13T12:00:00Z
type: task
priority: 2
assignee: Developer Name
---
# Ticket Title

Description text here.
```

## Do Not

- Add external dependencies beyond coreutils
- Use GNU-specific features without BSD fallback
- Introduce YAML/JSON parsing libraries
- Create test framework (manual testing is intentional)
- Add configuration files (the script is self-contained)

@AGENTS.local.md
