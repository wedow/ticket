Feature: Ticket Archive
  As a user with a large ticket store
  I want to archive closed tickets
  So that routine commands stay fast without losing history

  Background:
    Given a clean tickets directory

  Scenario: Archive all closed tickets
    Given a ticket exists with ID "done-0001" and title "Done ticket"
    And ticket "done-0001" has status "closed"
    And a ticket exists with ID "open-0001" and title "Open ticket"
    When I run "ticket archive"
    Then the command should succeed
    And the output should contain "Archived 1 ticket(s)"
    And ticket "done-0001" should be archived
    And ticket "open-0001" should remain active

  Scenario: Archived dependency remains resolved
    Given a ticket exists with ID "ready-0001" and title "Ready ticket"
    And a ticket exists with ID "done-0001" and title "Done dependency"
    And ticket "ready-0001" depends on "done-0001"
    And ticket "done-0001" has status "closed"
    When I run "ticket archive done-0001"
    And I run "ticket ready"
    Then the command should succeed
    And the output should contain "ready-0001"

  Scenario: Show archived ticket with relationships
    Given a ticket exists with ID "done-0001" and title "Done ticket"
    And a ticket exists with ID "open-0001" and title "Open child" with parent "done-0001"
    And ticket "done-0001" has status "closed"
    When I run "ticket archive done-0001"
    And I run "ticket show done-0001"
    Then the command should succeed
    And the output should contain "# Done ticket"
    And the output should contain "open-0001 [open] Open child"

  Scenario: Reopen archived ticket restores it to active tickets
    Given a ticket exists with ID "done-0001" and title "Done ticket"
    And ticket "done-0001" has status "closed"
    When I run "ticket archive done-0001"
    And I run "ticket reopen done-0001"
    Then the command should succeed
    And ticket "done-0001" should remain active
    And ticket "done-0001" should have field "status" with value "open"

  Scenario: List includes archived closed tickets
    Given a ticket exists with ID "done-0001" and title "Done ticket"
    And ticket "done-0001" has status "closed"
    When I run "ticket archive done-0001"
    And I run "ticket ls --status=closed"
    Then the command should succeed
    And the output should contain "done-0001"

  Scenario: Archived relationships require reopening
    Given a ticket exists with ID "done-0001" and title "Done ticket"
    And a ticket exists with ID "open-0001" and title "Open ticket"
    And ticket "done-0001" has status "closed"
    When I run "ticket archive done-0001"
    And I run "ticket link done-0001 open-0001"
    Then the command should fail
    And the output should contain "must be reopened before editing relationships"

  Scenario: Open ticket cannot be archived
    Given a ticket exists with ID "open-0001" and title "Open ticket"
    When I run "ticket archive open-0001"
    Then the command should fail
    And the output should contain "only closed tickets can be archived"
