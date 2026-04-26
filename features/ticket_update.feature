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
    And ticket "test-0001" should have field "tags" with value "[\"bug\", \"urgent\"]"

  Scenario: JSON array with commas in values roundtrip
    When I run "ticket update test-0001 '--release_notes=[\"First entry, with comma.\", \"Second entry.\"]'"
    Then the command should succeed
    And the output should be "Updated 1 field(s) on test-0001"
    And ticket "test-0001" should have field "release_notes" with value "[\"First entry, with comma.\", \"Second entry.\"]"

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

  Scenario: Exit code is 0 on successful single update (set -e regression)
    When I run "ticket update test-0001 --priority=1"
    Then the command should succeed

  Scenario: Exit code is 0 on successful multiple updates
    When I run "ticket update test-0001 --priority=1 --assignee=alice"
    Then the command should succeed

  Scenario: Value containing ampersand is stored literally (sed & metachar)
    When I run "ticket update test-0001 --assignee='A & B'"
    Then the command should succeed
    And ticket "test-0001" should have field "assignee" with value "A & B"

  Scenario: Value containing forward slash is stored literally (sed / separator)
    When I run "ticket update test-0001 --external-ref='https://example.com/path'"
    Then the command should succeed
    And ticket "test-0001" should have field "external-ref" with value "https://example.com/path"

  Scenario: Cannot update id field (immutable)
    When I run "ticket update test-0001 --id=hacker"
    Then the command should fail
    And the output should contain "Error: field 'id' is immutable"

  Scenario: Cannot update created field (immutable)
    When I run "ticket update test-0001 --created=2020-01-01"
    Then the command should fail
    And the output should contain "immutable"

  Scenario: Cannot update status (routes to tk status)
    When I run "ticket update test-0001 --status=closed"
    Then the command should fail
    And the output should contain "use 'tk status"

  Scenario: Cannot update deps (routes to tk dep)
    When I run "ticket update test-0001 '--deps=[\"other\"]'"
    Then the command should fail
    And the output should contain "use 'tk dep"

  Scenario: Cannot update links (routes to tk link)
    When I run "ticket update test-0001 '--links=[\"other\"]'"
    Then the command should fail
    And the output should contain "use 'tk link"

  Scenario: Cannot update title (body content, not YAML)
    When I run "ticket update test-0001 --title=Renamed"
    Then the command should fail
    And the output should contain "markdown body"

  Scenario: Cannot update description (body content, not YAML)
    When I run "ticket update test-0001 --description=text"
    Then the command should fail
    And the output should contain "markdown body"

  Scenario: Cannot update design (body content, not YAML)
    When I run "ticket update test-0001 --design=text"
    Then the command should fail
    And the output should contain "markdown body"

  Scenario: Cannot update acceptance (body content, not YAML)
    When I run "ticket update test-0001 --acceptance=text"
    Then the command should fail
    And the output should contain "markdown body"

  Scenario: Priority must be 0-4
    When I run "ticket update test-0001 --priority=99"
    Then the command should fail
    And the output should contain "priority must be 0-4"

  Scenario: Priority accepts valid range
    When I run "ticket update test-0001 --priority=0"
    Then the command should succeed
    And ticket "test-0001" should have field "priority" with value "0"

  Scenario: Type must be one of known values
    When I run "ticket update test-0001 --type=banana"
    Then the command should fail
    And the output should contain "must be one of: bug, feature, task, epic, chore"

  Scenario: Parent must reference existing ticket
    When I run "ticket update test-0001 --parent=nonexistent"
    Then the command should fail
    And the output should contain "parent ticket 'nonexistent' not found"

  Scenario: Parent can reference an existing ticket (positive path)
    Given a ticket exists with ID "test-0002" and title "Parent ticket"
    When I run "ticket update test-0001 --parent=test-0002"
    Then the command should succeed
    And ticket "test-0001" should have field "parent" with value "test-0002"

  Scenario: Parent can be unset with empty value
    When I run "ticket update test-0001 --parent="
    Then the command should succeed

  Scenario: Invalid field name syntax is rejected (regex injection guard)
    When I run "ticket update test-0001 '--foo.bar=baz'"
    Then the command should fail
    And the output should contain "invalid field name"

  Scenario: Field-name validation precedes JSON validation (ordering)
    When I run "ticket update test-0001 '--foo.bar=[1,2,3]'"
    Then the command should fail
    And the output should contain "invalid field name"

  Scenario: tags array items must not contain commas (reader limitation)
    When I run "ticket update test-0001 '--tags=[\"urgent, internal\"]'"
    Then the command should fail
    And the output should contain "must not contain commas"

  Scenario: Comma-in-tag rejection error names the reason (not generic JSON error)
    When I run "ticket update test-0001 '--tags=[\"a, b\"]'"
    Then the command should fail
    And the output should contain "must not contain commas"

  Scenario: Comma-in-value is allowed for non-tag freeform fields (release_notes)
    When I run "ticket update test-0001 '--release_notes=[\"First, with comma.\", \"Second.\"]'"
    Then the command should succeed
    And ticket "test-0001" should have field "release_notes" with value "[\"First, with comma.\", \"Second.\"]"

  Scenario: Custom field names are allowed (hybrid policy, freeform category)
    When I run "ticket update test-0001 --sprint=42"
    Then the command should succeed
    And ticket "test-0001" should have field "sprint" with value "42"
