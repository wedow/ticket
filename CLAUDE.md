# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

See @README.md for usage documentation. Run `tk help` for command reference. Always update the README.md usage content when adding/changing commands and flags.

## Architecture

**Core script:** Single-file bash implementation (`ticket`, ~1000 lines). Uses awk for performant bulk operations on large ticket sets.

Key functions:
- `generate_id()` - Creates IDs from directory name prefix + random suffix
- `ticket_path()` - Resolves partial IDs to full file paths
- `yaml_field()` / `update_yaml_field()` - YAML frontmatter manipulation via sed
- `cmd_*()` - Command handlers
- `cmd_ready()`, `cmd_blocked()`, `cmd_ls()` - awk-based bulk listing with sorting

Dependencies: bash, sed, awk, find. Optional: ripgrep (faster grep).

**Plugin system:** Commands can be extended via external executables in PATH.
- `tk foo` checks for `tk-foo` then `ticket-foo` in PATH
- `tk super foo` bypasses plugins, runs built-in directly
- Plugins receive `TICKETS_DIR` and `TK_SCRIPT` environment variables
- See `plugins/README.md` for full conventions

## Plugins

### Directory Structure

```
plugins/
├── README.md              # Plugin conventions documentation
├── ticket-query           # Extracted from core (requires jq)
├── ticket-migrate-beads   # Extracted from core (requires jq)
└── ...

pkg/
├── extras.txt             # Curated list for ticket-extras meta-package
└── aur/                   # PKGBUILD templates
```

### Plugin File Conventions

Plugins in this repo use `ticket-` prefix (matching the script name). Both `tk-*` and `ticket-*` work at runtime.

Required metadata in first 10 lines:
```bash
#!/usr/bin/env bash
# tk-plugin: Short description for tk help
# tk-plugin-version: 1.0.0
```

### Extracting Commands to Plugins

When moving a command from core to a plugin:
1. Create `plugins/ticket-<name>` with the extracted logic
2. Add metadata comments (`tk-plugin:`, `tk-plugin-version:`)
3. Remove `cmd_<name>()` from core script
4. Remove from dispatch case statement
5. Add `<name>` to `pkg/extras.txt`
6. Update CHANGELOG.md (see below)
7. Update README.md usage section

### Creating New Plugins

For new functionality (not extracted from core):
1. Create `plugins/ticket-<name>`
2. Add metadata comments
3. Do NOT add to `pkg/extras.txt` (only core extractions go there)
4. Document in plugins/README.md if it's an official plugin

## Testing

BDD tests using [Behave](https://behave.readthedocs.io/). Run with `make test` (requires `uv`).

- Feature files: `features/*.feature` - Gherkin scenarios covering all commands
- Step definitions: `features/steps/ticket_steps.py`
- CI runs tests on push to master and all PRs

When adding new commands or flags, add corresponding scenarios to the appropriate feature file.

## Changelog

Update CHANGELOG.md when committing notable changes:

### Core Script Changes
- New commands, flags, bug fixes, behavior changes
- Add under appropriate heading (Added, Fixed, Changed, Removed)

### Plugin Changes
- Add a `### Plugins` subsection when plugins are added/modified
- Format: `- ticket-<name> <version>: <change description>`

Example:
```markdown
## [Unreleased]

### Added
- New `foo` command

### Plugins
- ticket-query 1.1.0: Added --format flag
- ticket-migrate-beads 1.0.1: Fixed date parsing
```

### What Doesn't Need Logging
- Documentation-only changes
- CI/workflow changes (unless they affect user-facing behavior)

## Releases & Packaging

### Package Structure

Three meta-packages plus individual plugin packages:
- `ticket` - Full installation (depends on ticket-core + ticket-extras)
- `ticket-core` - Core script only, no plugins
- `ticket-extras` - Curated plugins extracted from core (listed in `pkg/extras.txt`)
- `ticket-<name>` - Individual plugin packages

Users can mix and match:
```bash
brew install ticket           # Everything
brew install ticket-core      # Minimal
brew install ticket-core ticket-query  # Core + specific plugin
```

### Release Flow

1. Update CHANGELOG.md: change `## [Unreleased]` to version + date
2. Commit and tag:
   ```bash
   git commit -am "release: v0.4.0"
   git tag v0.4.0
   git push && git push origin v0.4.0
   ```

### CI Publishing

The release workflow (`.github/workflows/release.yml`) automatically:
1. Creates GitHub release with changelog body
2. Runs `scripts/publish-homebrew.sh` - updates all formulas in tap
3. Runs `scripts/publish-aur.sh` - updates all AUR packages

Plugins are only published if their `tk-plugin-version` changed (identical PKGBUILDs result in no-op pushes).

### Package Managers

- **Homebrew:** `wedow/homebrew-tools` tap
- **AUR:** Individual repos at `aur.archlinux.org/<pkgname>.git`

Both are updated automatically by CI. AUR repos are created on first push if they don't exist.
