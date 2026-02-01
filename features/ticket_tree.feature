Feature: Ticket Tree (Parent-Child Hierarchy)
  As a user
  I want to view the parent-child ticket hierarchy
  So that I can understand how tickets are organized

  Background:
    Given a clean tickets directory

  Scenario: Tree with no args shows root tickets and children
    Given a ticket exists with ID "proj-0001" and title "Epic Auth"
    And a ticket exists with ID "proj-0002" and title "Login page" with parent "proj-0001"
    And a ticket exists with ID "proj-0003" and title "Standalone task"
    When I run "ticket tree"
    Then the command should succeed
    And the output should contain "proj-0001"
    And the output should contain "proj-0002"
    And the output should contain "proj-0003"

  Scenario: Tree with specific ID shows subtree
    Given a ticket exists with ID "proj-0001" and title "Epic Auth"
    And a ticket exists with ID "proj-0002" and title "Login page" with parent "proj-0001"
    And a ticket exists with ID "proj-0003" and title "Standalone task"
    When I run "ticket tree proj-0001"
    Then the command should succeed
    And the output should contain "proj-0001"
    And the output should contain "proj-0002"
    And the output should not contain "proj-0003"

  Scenario: Tree uses box-drawing characters
    Given a ticket exists with ID "proj-0001" and title "Epic Auth"
    And a ticket exists with ID "proj-0002" and title "Login page" with parent "proj-0001"
    When I run "ticket tree proj-0001"
    Then the command should succeed
    And the output should match box-drawing tree format

  Scenario: Tree shows status and title
    Given a ticket exists with ID "proj-0001" and title "Epic Auth"
    And a ticket exists with ID "proj-0002" and title "Login page" with parent "proj-0001"
    And ticket "proj-0002" has status "in_progress"
    When I run "ticket tree"
    Then the command should succeed
    And the output should contain "[open]"
    And the output should contain "[in_progress]"
    And the output should contain "Epic Auth"
    And the output should contain "Login page"

  Scenario: Tree handles deep nesting (grandchildren)
    Given a ticket exists with ID "proj-0001" and title "Epic"
    And a ticket exists with ID "proj-0002" and title "Feature" with parent "proj-0001"
    And a ticket exists with ID "proj-0003" and title "Task" with parent "proj-0002"
    When I run "ticket tree"
    Then the command should succeed
    And the output should contain "proj-0001"
    And the output should contain "proj-0002"
    And the output should contain "proj-0003"
    And the tree output should have proj-0001 before proj-0002
    And the tree output should have proj-0002 before proj-0003

  Scenario: Tree handles leaf tickets (no children)
    Given a ticket exists with ID "proj-0001" and title "Leaf task"
    When I run "ticket tree proj-0001"
    Then the command should succeed
    And the output should contain "proj-0001"
    And the output should contain "Leaf task"
    And the output line count should be 1

  Scenario: Tree sorts children by subtree depth then ID
    Given a ticket exists with ID "proj-0001" and title "Root"
    And a ticket exists with ID "proj-0005" and title "Child B shallow" with parent "proj-0001"
    And a ticket exists with ID "proj-0003" and title "Child A shallow" with parent "proj-0001"
    And a ticket exists with ID "proj-0004" and title "Child C deep" with parent "proj-0001"
    And a ticket exists with ID "proj-0006" and title "Grandchild" with parent "proj-0004"
    When I run "ticket tree proj-0001"
    Then the command should succeed
    And the tree output should have proj-0003 before proj-0005
    And the tree output should have proj-0005 before proj-0004
    And the tree output should have proj-0004 before proj-0006

  Scenario: Tree sorts children by ID when same depth
    Given a ticket exists with ID "proj-0001" and title "Root"
    And a ticket exists with ID "proj-0005" and title "Child E" with parent "proj-0001"
    And a ticket exists with ID "proj-0002" and title "Child B" with parent "proj-0001"
    And a ticket exists with ID "proj-0004" and title "Child D" with parent "proj-0001"
    And a ticket exists with ID "proj-0003" and title "Child C" with parent "proj-0001"
    When I run "ticket tree proj-0001"
    Then the command should succeed
    And the tree output should have proj-0002 before proj-0003
    And the tree output should have proj-0003 before proj-0004
    And the tree output should have proj-0004 before proj-0005

  Scenario: Tree with status filter shows matching tickets and ancestors
    Given a ticket exists with ID "proj-0001" and title "Epic Auth"
    And a ticket exists with ID "proj-0002" and title "Login page" with parent "proj-0001"
    And a ticket exists with ID "proj-0003" and title "Signup page" with parent "proj-0001"
    And ticket "proj-0001" has status "closed"
    And ticket "proj-0002" has status "open"
    And ticket "proj-0003" has status "closed"
    When I run "ticket tree --status=open"
    Then the command should succeed
    And the output should contain "proj-0001"
    And the output should contain "proj-0002"
    And the output should not contain "proj-0003"

  Scenario: Tree with assignee filter
    Given a ticket exists with ID "proj-0001" and title "Epic Auth"
    And a ticket exists with ID "proj-0002" and title "Login page" with parent "proj-0001"
    And a ticket exists with ID "proj-0003" and title "Signup page" with parent "proj-0001"
    And ticket "proj-0002" has assignee "alice"
    And ticket "proj-0003" has assignee "bob"
    When I run "ticket tree -a alice"
    Then the command should succeed
    And the output should contain "proj-0001"
    And the output should contain "proj-0002"
    And the output should not contain "proj-0003"

  Scenario: Tree with tag filter
    Given a ticket exists with ID "proj-0001" and title "Epic Auth"
    And a ticket exists with ID "proj-0002" and title "Login page" with parent "proj-0001"
    And a ticket exists with ID "proj-0003" and title "Signup page" with parent "proj-0001"
    And ticket "proj-0002" has tags "frontend,ui"
    And ticket "proj-0003" has tags "backend"
    When I run "ticket tree -T frontend"
    Then the command should succeed
    And the output should contain "proj-0001"
    And the output should contain "proj-0002"
    And the output should not contain "proj-0003"

  Scenario: Tree with no matching tickets produces no output
    Given a ticket exists with ID "proj-0001" and title "Epic Auth"
    And ticket "proj-0001" has status "closed"
    When I run "ticket tree --status=in_progress"
    Then the command should succeed
    And the output should be empty

  Scenario: Tree supports partial ID matching
    Given a ticket exists with ID "proj-0001" and title "Epic Auth"
    And a ticket exists with ID "proj-0002" and title "Login page" with parent "proj-0001"
    When I run "ticket tree 0001"
    Then the command should succeed
    And the output should contain "proj-0001"
    And the output should contain "proj-0002"

  Scenario: Tree with non-existent ID fails
    Given a ticket exists with ID "proj-0001" and title "Epic Auth"
    When I run "ticket tree nonexistent"
    Then the command should fail
    And the output should contain "Error: ticket nonexistent not found"

  Scenario: Tree multiple roots sorted by subtree depth then ID
    Given a ticket exists with ID "proj-0003" and title "Root C (deep)"
    And a ticket exists with ID "proj-0001" and title "Root A (shallow)"
    And a ticket exists with ID "proj-0002" and title "Root B (shallow)"
    And a ticket exists with ID "proj-0004" and title "Child of C" with parent "proj-0003"
    When I run "ticket tree"
    Then the command should succeed
    And the tree output should have proj-0001 before proj-0002
    And the tree output should have proj-0002 before proj-0003
    And the tree output should have proj-0003 before proj-0004
