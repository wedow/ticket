# Changelog

## [0.2.2] - 2026-01-13

### Added

- `--version` / `-V` flag to print the current version

## [0.2.1] - 2026-01-26

### Added

- `add-note` command for appending timestamped notes to tickets
- Nix flake support for installation via `nix run github:wedow/ticket`

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
