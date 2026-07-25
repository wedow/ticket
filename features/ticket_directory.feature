Feature: Ticket Directory Resolution
  As a user
  I want tk to find .tickets by walking parent directories
  So that I can run commands from any subdirectory of my project

  Background:
    Given a clean tickets directory

  Scenario: Find tickets in current directory
    Given a ticket exists with ID "test-0001" and title "Test ticket"
    When I run "ticket ls"
    Then the command should succeed
    And the output should contain "test-0001"

  Scenario: Find tickets in parent directory
    Given a ticket exists with ID "test-0001" and title "Test ticket"
    And I am in subdirectory "src/components"
    When I run "ticket ls"
    Then the command should succeed
    And the output should contain "test-0001"

  Scenario: Find tickets in grandparent directory
    Given a ticket exists with ID "test-0001" and title "Test ticket"
    And I am in subdirectory "src/components/ui"
    When I run "ticket ls"
    Then the command should succeed
    And the output should contain "test-0001"

  Scenario: Create ticket initializes in current directory when no parent has tickets
    Given the tickets directory does not exist
    And I am in subdirectory "new-project"
    When I run "ticket create 'First ticket'"
    Then the command should succeed
    And the output should match a ticket ID pattern
    And tickets directory should exist in current subdirectory

  Scenario: Error when no tickets directory for read command
    Given the tickets directory does not exist
    When I run "ticket show nonexistent"
    Then the command should fail
    And the output should contain "no .tickets directory found"

  Scenario: Error when no tickets directory in any parent
    Given the tickets directory does not exist
    And I am in subdirectory "orphan/deep/path"
    When I run "ticket ready"
    Then the command should fail
    And the output should contain "no .tickets directory found"

  Scenario: TICKETS_DIR env var takes priority over parent walking
    Given a ticket exists with ID "parent-0001" and title "Parent ticket"
    And a separate tickets directory exists at "other-tickets" with ticket "other-0001" titled "Other ticket"
    And I am in subdirectory "src"
    When I run "ticket ls" with TICKETS_DIR set to "other-tickets"
    Then the command should succeed
    And the output should contain "other-0001"
    And the output should not contain "parent-0001"

  Scenario: Show command works from subdirectory
    Given a ticket exists with ID "test-0001" and title "Test ticket"
    And I am in subdirectory "src"
    When I run "ticket show test-0001"
    Then the command should succeed
    And the output should contain "id: test-0001"

  Scenario: Dep command works from subdirectory
    Given a ticket exists with ID "task-0001" and title "Main task"
    And a ticket exists with ID "task-0002" and title "Dependency"
    And I am in subdirectory "lib"
    When I run "ticket dep task-0001 task-0002"
    Then the command should succeed
    And the output should contain "Added dependency"

  Scenario: Help command works without tickets directory
    Given the tickets directory does not exist
    When I run "ticket help"
    Then the command should succeed
    And the output should contain "minimal ticket system"
