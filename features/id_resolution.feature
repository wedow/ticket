Feature: Ticket ID Resolution
  As a user
  I want to use partial ticket IDs
  So that I can work faster without typing full IDs

  Background:
    Given a clean tickets directory

  Scenario: Exact ID match
    Given a ticket exists with ID "abc-1234" and title "Test ticket"
    When I run "ticket show abc-1234"
    Then the command should succeed
    And the output should contain "id: abc-1234"

  Scenario: Partial ID match by suffix
    Given a ticket exists with ID "abc-1234" and title "Test ticket"
    When I run "ticket show 1234"
    Then the command should succeed
    And the output should contain "id: abc-1234"

  Scenario: Partial ID match by prefix
    Given a ticket exists with ID "abc-1234" and title "Test ticket"
    When I run "ticket show abc"
    Then the command should succeed
    And the output should contain "id: abc-1234"

  Scenario: Partial ID match by substring
    Given a ticket exists with ID "abc-1234" and title "Test ticket"
    When I run "ticket show c-12"
    Then the command should succeed
    And the output should contain "id: abc-1234"

  Scenario: Ambiguous ID error
    Given a ticket exists with ID "abc-1234" and title "First ticket"
    And a ticket exists with ID "abc-5678" and title "Second ticket"
    When I run "ticket show abc"
    Then the command should fail
    And the output should contain "Error: ambiguous ID 'abc' matches multiple tickets"

  Scenario: Non-existent ID error
    When I run "ticket show nonexistent"
    Then the command should fail
    And the output should contain "Error: ticket 'nonexistent' not found"

  Scenario: Exact match takes precedence
    Given a ticket exists with ID "abc" and title "Short ID ticket"
    And a ticket exists with ID "abc-1234" and title "Long ID ticket"
    When I run "ticket show abc"
    Then the command should succeed
    And the output should contain "id: abc"
    And the output should contain "Short ID ticket"

  Scenario: ID resolution works with status command
    Given a ticket exists with ID "test-9999" and title "Test ticket"
    When I run "ticket status 9999 in_progress"
    Then the command should succeed
    And ticket "test-9999" should have field "status" with value "in_progress"

  Scenario: ID resolution works with dep command
    Given a ticket exists with ID "dep-aaaa" and title "Main"
    And a ticket exists with ID "dep-bbbb" and title "Dependency"
    When I run "ticket dep aaaa bbbb"
    Then the command should succeed
    And ticket "dep-aaaa" should have "bbbb" in deps

  Scenario: ID resolution works with link command
    Given a ticket exists with ID "link-cccc" and title "First"
    And a ticket exists with ID "link-dddd" and title "Second"
    When I run "ticket link cccc dddd"
    Then the command should succeed
    And ticket "link-cccc" should have "link-dddd" in links

  Scenario: Partial ID match works with symlinked tickets directory
    Given a symlinked tickets directory
    And a ticket exists with ID "sym-1234" and title "Symlink test"
    When I run "ticket show 1234"
    Then the command should succeed
    And the output should contain "id: sym-1234"
