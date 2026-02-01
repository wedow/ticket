# CLAUDE.md

This project uses `tk` for ticket management.

When adding/changing commands or flags, update:
1. The `cmd_help()` function in `ticket`
2. The Usage section in `README.md`

## Architecture

Single-file bash implementation (`ticket`, ~1400 lines). Uses awk for performant bulk operations on large ticket sets.

Key functions:
- `generate_id()` - Creates IDs from directory name prefix + timestamp hash
- `ticket_path()` - Resolves partial IDs to full file paths
- `yaml_field()` / `update_yaml_field()` - YAML frontmatter manipulation via sed
- `cmd_*()` - Command handlers (dispatch at bottom of file)
- `cmd_ready()`, `cmd_blocked()`, `cmd_ls()` - awk-based bulk listing with sorting

Tickets are markdown files with YAML frontmatter in `.tickets/` (configurable via `TICKETS_DIR` env var). Core YAML fields: `id`, `status`, `deps`, `links`, `created`, `type`, `priority`, `assignee`, `parent`, `tags`.

Dependencies: bash, sed, awk, find. Optional: ripgrep (faster grep), jq (for query command).

## Testing

No formal test suite. Manual testing workflow:
```bash
# Create test tickets
tk create "Test ticket" -d "Description"

# Verify commands
tk ls
tk ready
tk blocked
```

## Changelog

When committing notable changes to the `ticket` script (new commands, flags, bug fixes, behavior changes), update CHANGELOG.md in the same commit:
- Create `## [Unreleased]` section at top if it doesn't exist
- Add bullet points under appropriate heading (Added, Fixed, Changed, Removed)
- Only script changes need logging; docs/workflow changes don't

## Releases & Packaging

Before tagging a release:
1. Ensure CHANGELOG.md has a section for the new version with release date
2. Update "Unreleased" to the version number and today's date
3. Commit the changelog update as part of the release

```bash
# Example release flow
# 1. Update CHANGELOG.md: change "## [0.3.0] - Unreleased" to "## [0.3.0] - 2026-01-15"
# 2. Commit and tag
git commit -am "release: v0.3.0"
git tag v0.3.0
git push && git push origin v0.3.0
```

The GitHub Actions workflow (`.github/workflows/release.yml`) automatically:
1. Extracts the changelog section for this version as the release body
2. Updates the Homebrew formula in `wedow/homebrew-tools` tap
3. Updates the AUR package (builds `.SRCINFO` via Docker)

Both package managers install the script as `tk` in the user's PATH.
