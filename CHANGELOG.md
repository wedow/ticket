# Changelog

## [Unreleased]

### Added
- `list` alias for `ls` command
- `needs_testing` status
- `-s, --status` flag for `edit` command to change ticket status
- Hierarchy gating: `ready` only shows tickets whose parent is `in_progress`
- `--open` flag for `ready` to bypass hierarchy checks
- Status propagation: `needs_testing`/`closed` auto-bubble up parent chain
- `workflow` command outputs guide for LLM context
- `-t, --type` filter flag for `ls` command
- Interactive prompts when `tk create` is run with no arguments
- Support `TICKETS_DIR` environment variable for custom tickets directory location
- `dep cycle` command to detect dependency cycles in open tickets
- `add-note` command for appending timestamped notes to tickets
- `-a, --assignee` filter flag for `ls`, `ready`, `blocked`, and `closed` commands
- `--tags` flag for `create` command to add comma-separated tags
- `-T, --tag` filter flag for `ls`, `ready`, `blocked`, and `closed` commands
- `-P, --priority` filter flag for `ls` command
- `delete` command to remove ticket files

### Changed
- `create` command now displays full ticket details on success instead of just the ID
- `edit` command now uses CLI flags instead of opening $EDITOR

### Removed
- `start`, `testing`, `close`, `reopen`, `status` commands (use `edit -s` instead)

### Fixed
- `update_yaml_field` now works on BSD/macOS (was using GNU sed syntax)

## [0.2.0] - 2026-01-04

### Added
- `--parent` flag for `create` command to set parent ticket
- `link`/`unlink` commands for symmetric ticket relationships
- `show` command displays parent title and linked tickets
- `migrate-beads` now imports parent-child and related dependencies

## [0.1.1] - 2026-01-02

### Fixed
- `edit` command no longer hangs when run in non-TTY environments

## [0.1.0] - 2026-01-02

Initial release.
