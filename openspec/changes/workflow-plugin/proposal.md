## Why

The beads workflow system (formulas/molecules) provides useful templated workflow creation, but beads requires SQLite sync and a background daemon. Since `tk` already replaces beads for issue tracking, it should also support workflow templates natively — allowing users to define repeatable multi-step processes (releases, feature development, etc.) and instantiate them as tickets with proper dependency graphs.

## What Changes

- New `ticket-workflow` plugin providing `tk workflow list` and `tk workflow run` commands
- Workflow definitions in TOML format (adapted from beads formula format, replacing "formula" with "workflow")
- Workflow search path: `.tickets/workflows/` (project-level) then `~/.config/ticket/workflows/` (user-level)
- `tk workflow run <name>` creates parent + child tickets with dependencies, supporting variable substitution via `--var key=value`
- No gates, wisps, or molecule tracking — just template instantiation into tickets

## Capabilities

### New Capabilities
- `workflow-templates`: TOML-based workflow definition format with variables, step types, and dependency declarations
- `workflow-commands`: CLI commands for listing available workflows and instantiating them as tickets

### Modified Capabilities

## Impact

- New plugin file: `plugins/ticket-workflow`
- New config directories: `.tickets/workflows/` and `~/.config/ticket/workflows/`
- Depends on core `tk create` and `tk dep` commands for ticket/dependency creation
- No changes to core script
- No new external dependencies beyond bash/sed/awk (TOML parsing done with awk/sed)
