Feature: Ticket Edit
  As a user
  I want to edit tickets in my editor
  So that I can make complex changes easily

  Background:
    Given a clean tickets directory
    And a ticket exists with ID "edit-0001" and title "Editable ticket"

  Scenario: Edit in non-TTY mode shows file path
    When I run "ticket edit edit-0001" in non-TTY mode
    Then the command should succeed
    And the output should contain "Edit ticket file:"
    And the output should contain ".tickets/edit-0001.md"

  Scenario: Edit non-existent ticket
    When I run "ticket edit nonexistent"
    Then the command should fail
    And the output should contain "Error: ticket 'nonexistent' not found"

  Scenario: Edit with partial ID
    When I run "ticket edit 0001" in non-TTY mode
    Then the command should succeed
    And the output should contain "edit-0001.md"

  Scenario: Edit with --children opens parent and child
    Given a ticket exists with ID "edit-child1" and title "Child ticket" with parent "edit-0001"
    When I run "ticket edit edit-0001 --children" in non-TTY mode
    Then the command should succeed
    And the output should contain "edit-0001.md"
    And the output should contain "edit-child1.md"

  Scenario: Edit with --children opens nested descendants
    Given a ticket exists with ID "edit-mid" and title "Middle ticket" with parent "edit-0001"
    And a ticket exists with ID "edit-leaf" and title "Leaf ticket" with parent "edit-mid"
    When I run "ticket edit edit-0001 --children" in non-TTY mode
    Then the command should succeed
    And the output should contain "edit-0001.md"
    And the output should contain "edit-mid.md"
    And the output should contain "edit-leaf.md"

  Scenario: Edit with --children on leaf ticket shows only that ticket
    When I run "ticket edit edit-0001 --children" in non-TTY mode
    Then the command should succeed
    And the output should contain "edit-0001.md"
    And the output line count should be 1

  Scenario: Edit with --children outputs parent before children
    Given a ticket exists with ID "edit-kid" and title "Kid ticket" with parent "edit-0001"
    When I run "ticket edit edit-0001 --children" in non-TTY mode
    Then the command should succeed
    And the output line 1 should contain "edit-0001.md"
    And the output line 2 should contain "edit-kid.md"

  Scenario: Edit with --children and partial ID
    Given a ticket exists with ID "edit-pc01" and title "Parent for partial"
    And a ticket exists with ID "edit-pc02" and title "Child for partial" with parent "edit-pc01"
    When I run "ticket edit pc01 --children" in non-TTY mode
    Then the command should succeed
    And the output should contain "edit-pc01.md"
    And the output should contain "edit-pc02.md"

  Scenario: Edit with --children flag before ID
    Given a ticket exists with ID "edit-flagfirst" and title "Flag first child" with parent "edit-0001"
    When I run "ticket edit --children edit-0001" in non-TTY mode
    Then the command should succeed
    And the output should contain "edit-0001.md"
    And the output should contain "edit-flagfirst.md"

  Scenario: Edit with --children on non-existent ticket
    When I run "ticket edit nonexistent --children"
    Then the command should fail
    And the output should contain "Error: ticket 'nonexistent' not found"
