Feature: Ticket Update Command
  As a user or automation script
  I want to update ticket fields non-interactively
  So that I can modify tickets without opening an editor

  Background:
    Given a clean tickets directory
    And a ticket exists with ID "test-0001" and title "Test ticket"

  Scenario: Update existing field
    When I run "ticket update test-0001 --priority=1"
    Then the command should succeed
    And the output should be "Updated 1 field(s) on test-0001"
    And ticket "test-0001" should have field "priority" with value "1"

  Scenario: Update multiple fields
    When I run "ticket update test-0001 --priority=1 --assignee=alice"
    Then the command should succeed
    And the output should be "Updated 2 field(s) on test-0001"
    And ticket "test-0001" should have field "priority" with value "1"
    And ticket "test-0001" should have field "assignee" with value "alice"

  Scenario: Add new custom field
    When I run "ticket update test-0001 --custom_field=myvalue"
    Then the command should succeed
    And the output should be "Updated 1 field(s) on test-0001"
    And ticket "test-0001" should have field "custom_field" with value "myvalue"

  Scenario: Update with JSON array (requires jq)
    When I run "ticket update test-0001 '--tags=[\"bug\",\"urgent\"]'"
    Then the command should succeed
    And the output should be "Updated 1 field(s) on test-0001"
    And ticket "test-0001" should have field "tags" with value "[bug, urgent]"

  Scenario: Invalid JSON array
    When I run "ticket update test-0001 '--tags=[\"unclosed]'"
    Then the command should fail
    And the output should contain "Error: invalid JSON for tags"

  Scenario: Unknown argument without --field=value format
    When I run "ticket update test-0001 badarg"
    Then the command should fail
    And the output should contain "Error: unknown argument 'badarg'"

  Scenario: Missing field arguments
    When I run "ticket update test-0001"
    Then the command should fail
    And the output should contain "Usage:"

  Scenario: Update non-existent ticket
    When I run "ticket update nonexistent --priority=1"
    Then the command should fail
    And the output should contain "Error: ticket 'nonexistent' not found"

  Scenario: Update with partial ID
    When I run "ticket update 0001 --priority=0"
    Then the command should succeed
    And ticket "test-0001" should have field "priority" with value "0"
