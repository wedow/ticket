## ADDED Requirements

### Requirement: Workflow definition format
The system SHALL support workflow definitions in TOML files with the following top-level fields:
- `workflow` (string, required): workflow name
- `description` (string, optional): human-readable description
- `version` (integer, optional): schema version
- `type` (string, optional): informational type field (e.g., "workflow")

#### Scenario: Valid workflow file
- **WHEN** a file `.tickets/workflows/release.toml` contains `workflow = "release"` and `version = 1`
- **THEN** the system SHALL recognize it as a valid workflow definition

#### Scenario: Missing workflow name
- **WHEN** a TOML file in the workflows directory lacks a `workflow = "..."` line
- **THEN** the system SHALL skip that file and not list it as an available workflow

### Requirement: Workflow variables
The system SHALL support variable definitions under `[vars.<name>]` sections with the following fields:
- `description` (string, optional): human-readable description
- `required` (boolean, optional): whether the variable must be provided
- `default` (string, optional): default value if not provided
- `pattern` (string, optional): regex pattern for validation
- `enum` (array, optional): list of allowed values

#### Scenario: Required variable provided
- **WHEN** a workflow defines `[vars.version]` with `required = true` and the user provides `--var version=1.0.0`
- **THEN** the system SHALL substitute `{{version}}` with `1.0.0` in all step titles and descriptions

#### Scenario: Required variable missing
- **WHEN** a workflow defines `[vars.version]` with `required = true` and the user does not provide `--var version=...`
- **THEN** the system SHALL exit with an error listing the missing required variable

#### Scenario: Variable with default
- **WHEN** a workflow defines `[vars.env]` with `default = "staging"` and the user does not provide `--var env=...`
- **THEN** the system SHALL use `"staging"` as the value for `{{env}}`

#### Scenario: Variable with pattern constraint
- **WHEN** a workflow defines `[vars.version]` with `pattern = "^\d+\.\d+\.\d+$"` and the user provides `--var version=abc`
- **THEN** the system SHALL exit with an error indicating the value does not match the pattern

#### Scenario: Variable with enum constraint
- **WHEN** a workflow defines `[vars.env]` with `enum = ["staging", "production"]` and the user provides `--var env=dev`
- **THEN** the system SHALL exit with an error indicating the value is not in the allowed set

### Requirement: Workflow steps
The system SHALL support step definitions in `[[steps]]` array tables with the following fields:
- `id` (string, required): unique step identifier
- `title` (string, required): step title, supports `{{var}}` substitution
- `description` (string, optional): step description, supports `{{var}}` substitution
- `needs` (array, optional): list of step IDs this step depends on
- `type` (string, optional): informational step type

#### Scenario: Step with dependencies
- **WHEN** a workflow defines step "implement" with `needs = ["design"]`
- **THEN** the system SHALL create a dependency from the "implement" ticket to the "design" ticket

#### Scenario: Step with variable substitution in title
- **WHEN** a step has `title = "Deploy {{version}}"` and the variable `version` is `2.0.0`
- **THEN** the created ticket SHALL have title "Deploy 2.0.0"

### Requirement: Workflow search path
The system SHALL search for workflow files in the following order:
1. `.tickets/workflows/*.toml` (project-level)
2. `~/.config/ticket/workflows/*.toml` (user-level)

Project-level workflows SHALL take precedence over user-level workflows with the same name.

#### Scenario: Project workflow found
- **WHEN** `.tickets/workflows/release.toml` exists
- **THEN** it SHALL be listed and available for `tk workflow run release`

#### Scenario: User workflow found
- **WHEN** `~/.config/ticket/workflows/deploy.toml` exists and no project-level `deploy.toml` exists
- **THEN** it SHALL be listed and available for `tk workflow run deploy`

#### Scenario: Project overrides user workflow
- **WHEN** both `.tickets/workflows/release.toml` and `~/.config/ticket/workflows/release.toml` exist
- **THEN** the project-level version SHALL be used
