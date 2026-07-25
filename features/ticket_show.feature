Feature: Ticket Show
  As a user
  I want to view ticket details
  So that I can see full information about a ticket

  Background:
    Given a clean tickets directory

  Scenario: Show displays ticket content
    Given a ticket exists with ID "show-001" and title "Test ticket"
    When I run "ticket show show-001"
    Then the command should succeed
    And the output should contain "id: show-001"
    And the output should contain "# Test ticket"

  Scenario: Show displays all frontmatter fields
    Given a ticket exists with ID "show-001" and title "Full ticket"
    When I run "ticket show show-001"
    Then the command should succeed
    And the output should contain "status: open"
    And the output should contain "deps: []"
    And the output should contain "links: []"
    And the output should contain "type: task"
    And the output should contain "priority: 2"

  Scenario: Show displays blockers section
    Given a ticket exists with ID "show-001" and title "Blocked ticket"
    And a ticket exists with ID "show-002" and title "Blocker ticket"
    And ticket "show-001" depends on "show-002"
    When I run "ticket show show-001"
    Then the command should succeed
    And the output should contain "## Blockers"
    And the output should contain "show-002 [open] Blocker ticket"

  Scenario: Show hides blockers section when all deps closed
    Given a ticket exists with ID "show-001" and title "Unblocked ticket"
    And a ticket exists with ID "show-002" and title "Closed blocker"
    And ticket "show-001" depends on "show-002"
    And ticket "show-002" has status "closed"
    When I run "ticket show show-001"
    Then the command should succeed
    And the output should not contain "## Blockers"

  Scenario: Show displays blocking section
    Given a ticket exists with ID "show-001" and title "Blocker"
    And a ticket exists with ID "show-002" and title "Blocked"
    And ticket "show-002" depends on "show-001"
    When I run "ticket show show-001"
    Then the command should succeed
    And the output should contain "## Blocking"
    And the output should contain "show-002 [open] Blocked"

  Scenario: Show displays children section
    Given a ticket exists with ID "show-001" and title "Parent"
    And a ticket exists with ID "show-002" and title "Child" with parent "show-001"
    When I run "ticket show show-001"
    Then the command should succeed
    And the output should contain "## Children"
    And the output should contain "show-002 [open] Child"

  Scenario: Show displays linked section
    Given a ticket exists with ID "show-001" and title "First"
    And a ticket exists with ID "show-002" and title "Second"
    And ticket "show-001" is linked to "show-002"
    When I run "ticket show show-001"
    Then the command should succeed
    And the output should contain "## Linked"
    And the output should contain "show-002 [open] Second"

  Scenario: Show enhances parent field with title
    Given a ticket exists with ID "show-001" and title "Parent ticket"
    And a ticket exists with ID "show-002" and title "Child ticket" with parent "show-001"
    When I run "ticket show show-002"
    Then the command should succeed
    And the output should contain "parent: show-001"
    And the output should contain "# Parent ticket"

  Scenario: Show non-existent ticket
    When I run "ticket show nonexistent"
    Then the command should fail
    And the output should contain "Error: ticket 'nonexistent' not found"

  Scenario: Show with partial ID
    Given a ticket exists with ID "show-001" and title "Test ticket"
    When I run "ticket show 001"
    Then the command should succeed
    And the output should contain "id: show-001"
