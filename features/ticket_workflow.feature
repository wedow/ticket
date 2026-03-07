Feature: Workflow Templates
  As a user
  I want to define reusable workflow templates
  So that I can create repeatable multi-step processes as tickets

  Scenario: List workflows with none available
    Given a clean tickets directory
    When I run "ticket workflow list"
    Then the command should succeed
    And the output should contain "No workflows found"

  Scenario: List workflows from project directory
    Given a clean tickets directory
    And a workflow file "release" with description "Standard release"
    When I run "ticket workflow list"
    Then the command should succeed
    And the output should contain "release"
    And the output should contain "Standard release"
    And the output should contain "project"

  Scenario: Workflow run with dry-run
    Given a clean tickets directory
    And a workflow file "release" with steps
      | id      | title                      | needs         |
      | bump    | Bump version to {{version}} |               |
      | test    | Run tests                  | bump          |
      | publish | Publish {{version}}        | test          |
    When I run "ticket workflow run release --var version=1.0.0 --dry-run"
    Then the command should succeed
    And the output should contain "Dry run"
    And the output should contain "Bump version to 1.0.0"
    And the output should contain "Publish 1.0.0"

  Scenario: Workflow run creates parent and child tickets
    Given a clean tickets directory
    And a simple workflow file "deploy" with 2 steps
    When I run "ticket workflow run deploy"
    Then the command should succeed
    And the output should contain "Created parent:"
    And the output should contain "Created step:"

  Scenario: Workflow run creates dependencies between steps
    Given a clean tickets directory
    And a workflow file "release" with steps
      | id      | title         | needs    |
      | build   | Build project |          |
      | deploy  | Deploy        | build    |
    When I run "ticket workflow run release"
    Then the command should succeed
    And the output should contain "Created parent:"
    And the output should contain "2 steps"

  Scenario: Missing required variable fails
    Given a clean tickets directory
    And a workflow file "release" with required variable "version"
    When I run "ticket workflow run release"
    Then the command should fail
    And the output should contain "missing required variables"
    And the output should contain "version"

  Scenario: Pattern validation rejects bad values
    Given a clean tickets directory
    And a workflow file "release" with variable "version" pattern "^[0-9]+\.[0-9]+\.[0-9]+$"
    When I run "ticket workflow run release --var version=abc"
    Then the command should fail
    And the output should contain "does not match pattern"

  Scenario: Enum validation rejects bad values
    Given a clean tickets directory
    And a workflow file "deploy" with variable "env" enum "staging,production"
    When I run "ticket workflow run deploy --var env=dev"
    Then the command should fail
    And the output should contain "not in allowed values"

  Scenario: Default variable values are used
    Given a clean tickets directory
    And a workflow file "deploy" with variable "env" default "staging" in step title "Deploy to {{env}}"
    When I run "ticket workflow run deploy --dry-run"
    Then the command should succeed
    And the output should contain "Deploy to staging"

  Scenario: Workflow not found
    Given a clean tickets directory
    When I run "ticket workflow run nonexistent"
    Then the command should fail
    And the output should contain "workflow 'nonexistent' not found"

  Scenario: Multiline triple-quoted strings are parsed
    Given a clean tickets directory
    And a workflow file "tdd" with multiline description and variable "name"
    When I run "ticket workflow run tdd --var name=auth --dry-run"
    Then the command should succeed
    And the output should contain "multiline description for auth"
    And the output should contain "spans multiple lines"

  Scenario: No subcommand shows usage
    Given a clean tickets directory
    When I run "ticket workflow"
    Then the command should fail
    And the output should contain "Usage"
