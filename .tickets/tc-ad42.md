---
id: tc-ad42
status: closed
deps: []
links: []
created: 2026-01-09T11:40:20Z
type: task
priority: 2
assignee: Ray Myers
---
# Feature: Specify scripts to run tests with

In features/steps/ticket_steps.py, it hard codes the bash implementation "./ticket".

Let's make that the default and accept and env var TICKET_SCRIPT to override.

This will allow us to use the same spec against multiple implementations.

## Notes

**2026-01-09T12:37:16Z**

Implemented TICKET_SCRIPT environment variable support. Added get_ticket_script() helper function that checks for TICKET_SCRIPT env var and defaults to ./ticket. Replaced all three hard-coded references in step_run_command_non_tty, step_run_command_no_stdin, and step_run_command. All 98 scenarios (642 steps) passing.
