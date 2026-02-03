Feature: Plugin System
  As a user
  I want to extend tk with custom commands
  So that I can add domain-specific functionality

  Scenario: Plugin in PATH is executed for unknown command
    Given a plugin "tk-hello" that outputs "Hello from plugin!"
    When I run "ticket hello"
    Then the command should succeed
    And the output should be "Hello from plugin!"

  Scenario: Plugin receives command arguments
    Given a plugin "tk-echo" that outputs its arguments
    When I run "ticket echo foo bar baz"
    Then the command should succeed
    And the output should be "foo bar baz"

  Scenario: ticket- prefix plugins are also discovered
    Given a plugin "ticket-greet" that outputs "Greetings!"
    When I run "ticket greet"
    Then the command should succeed
    And the output should be "Greetings!"

  Scenario: tk- prefix takes precedence over ticket- prefix
    Given a plugin "tk-test" that outputs "tk-prefix"
    And a plugin "ticket-test" that outputs "ticket-prefix"
    When I run "ticket test"
    Then the command should succeed
    And the output should be "tk-prefix"

  Scenario: Super command bypasses plugins
    Given a clean tickets directory
    And a plugin "tk-create" that outputs "plugin create"
    When I run "ticket super create \"Test ticket\""
    Then the command should succeed
    And the output should match a ticket ID pattern

  Scenario: Plugin receives TICKETS_DIR environment variable
    Given a clean tickets directory
    And a plugin "tk-checkenv" that outputs TICKETS_DIR
    When I run "ticket checkenv"
    Then the command should succeed
    And the output should contain ".tickets"

  Scenario: Plugin receives TK_SCRIPT environment variable
    Given a plugin "tk-checkscript" that outputs TK_SCRIPT
    When I run "ticket checkscript"
    Then the command should succeed
    And the output should contain "ticket"

  Scenario: Help command lists installed plugins
    Given a plugin "tk-myplugin" with description "My custom plugin"
    When I run "ticket help"
    Then the command should succeed
    And the output should contain "myplugin"
    And the output should contain "My custom plugin"

  Scenario: Help shows plugins without description as no description
    Given a plugin "tk-nodesc" that outputs "test" without metadata
    When I run "ticket help"
    Then the command should succeed
    And the output should contain "nodesc"
    And the output should contain "(no description)"

  Scenario: Plugin can call built-in commands via super
    Given a clean tickets directory
    And a plugin "tk-wrapper" that calls super create
    When I run "ticket wrapper \"Wrapped ticket\""
    Then the command should succeed
    And the output should match a ticket ID pattern

  Scenario: Built-in commands still work with plugins present
    Given a clean tickets directory
    And a plugin "tk-hello" that outputs "Hello!"
    When I run "ticket create \"Normal ticket\""
    Then the command should succeed
    And the output should match a ticket ID pattern
