## ADDED Requirements

### Requirement: Workflow list command
The `tk workflow list` command SHALL display all available workflows from both project and user directories, showing the workflow name and description.

#### Scenario: List with workflows available
- **WHEN** the user runs `tk workflow list` and workflows exist in the search path
- **THEN** the system SHALL print each workflow's name and description, one per line

#### Scenario: List with no workflows
- **WHEN** the user runs `tk workflow list` and no workflow files exist in any search path
- **THEN** the system SHALL print a message indicating no workflows were found

#### Scenario: List shows source location
- **WHEN** workflows exist in both project and user directories
- **THEN** the list SHALL indicate the source (project vs user) for each workflow

### Requirement: Workflow run command
The `tk workflow run <name>` command SHALL instantiate a workflow by creating tickets for all steps with proper dependencies.

#### Scenario: Basic workflow instantiation
- **WHEN** the user runs `tk workflow run release --var version=1.0.0` and a `release` workflow with 3 steps exists
- **THEN** the system SHALL create a parent ticket titled with the workflow description and 3 child tickets, printing each created ticket ID

#### Scenario: Dependencies created between steps
- **WHEN** a workflow has step "deploy" with `needs = ["build", "test"]`
- **THEN** the created "deploy" ticket SHALL have dependencies on both the "build" and "test" tickets

#### Scenario: Parent-child relationship
- **WHEN** a workflow is instantiated
- **THEN** all step tickets SHALL be created with `--parent` set to the parent (workflow root) ticket ID

#### Scenario: Variable substitution in created tickets
- **WHEN** a workflow step has `title = "Bump version to {{version}}"` and `--var version=2.0.0` is provided
- **THEN** the created ticket SHALL have the title "Bump version to 2.0.0"

#### Scenario: Workflow not found
- **WHEN** the user runs `tk workflow run nonexistent`
- **THEN** the system SHALL exit with an error indicating the workflow was not found

#### Scenario: Dry run mode
- **WHEN** the user runs `tk workflow run release --var version=1.0.0 --dry-run`
- **THEN** the system SHALL print what tickets would be created without actually creating them

### Requirement: Workflow run variable syntax
The `tk workflow run` command SHALL accept variables via `--var key=value` flags, supporting multiple variables.

#### Scenario: Multiple variables
- **WHEN** the user runs `tk workflow run deploy --var version=1.0.0 --var env=production`
- **THEN** both `{{version}}` and `{{env}}` SHALL be substituted in step titles and descriptions

#### Scenario: No variables needed
- **WHEN** a workflow has no required variables and the user runs `tk workflow run simple`
- **THEN** the system SHALL create tickets without requiring any `--var` flags

### Requirement: Plugin metadata
The plugin SHALL include proper metadata comments for `tk help` discovery.

#### Scenario: Help listing
- **WHEN** the user runs `tk help`
- **THEN** the workflow plugin SHALL appear with its description in the plugins section
