# New Language Implementation Guide

This document describes the process for porting ticket-cli to a new programming language. The process uses the ticket system itself to track progress and ensure completeness.

## Overview

The port involves:
1. Setting up the project structure
2. Adding development tools (linting, type checking, testing)
3. Systematically porting each BDD feature
4. Integrating the full BDD suite into the build
5. Manual smoke testing
6. Comparing implementation against the specification

## Prerequisites

- Familiarity with the target language and its ecosystem
- Understanding of docs/SPEC.md (the ticket-cli specification)
- Working knowledge of BDD testing with the features/ directory

## Process

### Step 1: Create Tracking Tickets

Create tickets for all major phases of the port. Use the `./ticket` command to create and organize these tickets.

#### 1.1 Setup Ticket

Create the initial setup ticket:

```bash
./ticket create "Setup project for [LANGUAGE] port"
```

In the ticket description, specify:
- Target subdirectory (e.g., `python/ticket`, `rust/ticket`, `go/ticket`)
- Package/module name
- Build tool or package manager to use (e.g., uv, cargo, npm, go mod)
- Basic project structure (src layout, package structure, etc.)

#### 1.2 Development Tools Ticket

Create a ticket for adding quality checks:

```bash
./ticket create "Lint / checks in [LANGUAGE] port"
```

This ticket should:
- Depend on the setup ticket (use `./ticket dep <new-id> <setup-id>`)
- Specify linting tools (e.g., ruff for Python, clippy for Rust, eslint for JS)
- Specify type checking tools (e.g., mypy, TypeScript, go vet)
- Specify testing framework (e.g., pytest, Jest, go test)
- Add a target to the root `Makefile` that runs all checks for the new language

Example Makefile target structure:
```makefile
.PHONY: [language] [language]-lint [language]-type [language]-test [language]-check

[language]: [language]-check

[language]-check: [language]-lint [language]-type [language]-test

[language]-lint:
	@echo "Running linter..."
	# command to run linter

[language]-type:
	@echo "Running type checker..."
	# command to run type checker

[language]-test:
	@echo "Running tests..."
	# command to run test suite
```

#### 1.3 BDD Features Scoping Ticket

Create a ticket for organizing BDD feature ports:

```bash
./ticket create "Scope out BDD [LANGUAGE]"
```

This ticket should:
- Create one ticket per feature file in `features/`
- Each feature ticket should include instructions for adding test commands to `[language]/bdd.sh`
- Set up dependencies so feature tickets depend on this scoping ticket
- Order feature tickets in logical implementation sequence

The ticket description should include instructions like:

```
For each feature in features/, create a ticket with:

./ticket create "Port feature <feature_name> to [LANGUAGE]"

Add this to the ticket description:

"Implement the [feature_name] functionality. Add this line to [language]/bdd.sh:

```bash
# Run [feature_name] feature tests
[test-runner] features/<feature_name>.feature
```

Then make each new ticket depend on this scoping ticket:

./ticket dep <new-ticket-id> <this-ticket-id>
```

The features to port (as of this writing):
- ticket_creation
- ticket_show
- ticket_status
- ticket_listing
- ticket_notes
- ticket_edit
- ticket_dependencies
- ticket_links
- ticket_query
- id_resolution

Recommended implementation order:
1. ticket_creation (foundation)
2. ticket_show (basic display)
3. ticket_status (simple operations)
4. ticket_listing (more complex queries)
5. ticket_notes (depends on show)
6. ticket_edit (depends on show)
7. ticket_dependencies (depends on show and listing)
8. ticket_links (similar to dependencies)
9. ticket_query (depends on listing)
10. id_resolution (depends on query)

#### 1.4 Full BDD Suite Integration Ticket

Create a ticket for integrating all BDD tests:

```bash
./ticket create "Add full BDD suite to [LANGUAGE] build"
```

This ticket should:
- Depend on ALL feature port tickets
- Update `[language]/bdd.sh` to run the complete test suite
- Add a `[language]-bdd` target to the root Makefile
- Ensure the BDD suite can be run as part of CI/CD

The `[language]/bdd.sh` script should:
- Export the appropriate wrapper script (e.g., `export TICKET_SCRIPT="${PROJECT_ROOT}/[lang]_ticket.sh"`)
- Run all feature tests using the language-appropriate BDD test runner

Example structure:
```bash
#!/usr/bin/env bash
set -euo pipefail

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
export TICKET_SCRIPT="${PROJECT_ROOT}/[lang]_ticket.sh"

# Run each feature
[test-runner] "${PROJECT_ROOT}/features/ticket_creation.feature"
[test-runner] "${PROJECT_ROOT}/features/ticket_show.feature"
# ... etc
```

#### 1.5 Manual Smoke Test Ticket

Create a ticket for manual validation:

```bash
./ticket create "Manual smoke test of [LANGUAGE] port"
```

This ticket should:
- Depend on the full BDD suite ticket
- Test the implementation in a fresh directory
- Cover basic real-world use cases:
  - Creating tickets
  - Viewing tickets
  - Updating status
  - Adding dependencies
  - Listing tickets
  - Querying tickets
- Verify the CLI behaves correctly in actual usage

#### 1.6 Spec Review Ticket

Create a ticket for comparing against the specification:

```bash
./ticket create "Spec review vs [LANGUAGE] implementation"
```

This ticket should:
- Depend on the full BDD suite ticket
- Depend on the manual smoke test ticket
- Compare the implementation against docs/SPEC.md
- Document findings in docs/SPEC_REVIEW_[LANGUAGE].md

The review should check:
- All commands are implemented
- Command behavior matches specification
- Edge cases are handled correctly
- Error messages are appropriate
- Sorting and ordering match requirements

### Step 2: Execute the Setup Ticket

When implementing the setup ticket:

1. Create the language subdirectory structure
2. Initialize the project with appropriate tools
3. Create a basic CLI entry point
4. Add initial tests
5. Create a wrapper script at the project root

Example wrapper script (`[lang]_ticket.sh`):
```bash
#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Set up environment and run the CLI
# Language-specific execution command
```

6. Add notes to the ticket documenting what was created
7. Close the ticket when complete: `./ticket close <ticket-id>`

### Step 3: Execute Development Tools Ticket

When implementing the development tools ticket:

1. Add linting configuration
2. Add type checking configuration
3. Add testing framework
4. Create Makefile targets at project root
5. Verify all checks pass
6. Add notes documenting the tools and configuration
7. Close the ticket when complete

### Step 4: Execute BDD Features Scoping

When implementing the scoping ticket:

1. Create one ticket per feature file
2. Set appropriate dependencies
3. Add detailed instructions to each ticket
4. Order tickets logically
5. Add notes summarizing the tickets created
6. Close the scoping ticket

### Step 5: Execute Feature Port Tickets

For each feature ticket:

1. Mark ticket as in progress: `./ticket status <ticket-id> in_progress`
2. Implement the feature functionality
3. Add the feature test to `[language]/bdd.sh` as specified
4. Run the feature test to verify it passes
5. Run unit tests if applicable
6. Add notes about implementation details
7. Close the ticket when tests pass: `./ticket close <ticket-id>`

Continue until all feature tickets are complete.

### Step 6: Execute Full BDD Suite Integration

When implementing the full BDD suite ticket:

1. Verify `[language]/bdd.sh` includes all features
2. Run the complete BDD suite and verify all tests pass
3. Add Makefile target to run BDD suite
4. Document any issues found and fixed
5. Close the ticket when all BDD tests pass

### Step 7: Execute Manual Smoke Test

When implementing the smoke test ticket:

1. Create a fresh temporary directory
2. Initialize a new ticket repository
3. Test basic workflows manually
4. Document any issues found
5. Verify the CLI is user-friendly
6. Add notes with test results
7. Close the ticket when smoke tests pass

### Step 8: Execute Spec Review

When implementing the spec review ticket:

1. Read through docs/SPEC.md carefully
2. Compare each command and feature with your implementation
3. Test edge cases and sorting behavior
4. Compare with the reference bash implementation if needed
5. Document findings in docs/SPEC_REVIEW_[LANGUAGE].md
6. Create new tickets for any missing features or bugs
7. Add notes with summary of findings
8. Close the ticket when review is complete

## Key Files to Create

For each new language implementation, you'll typically create:

1. **Language subdirectory**: `[language]/ticket/`
   - Source code
   - Tests
   - Configuration files
   - Package/project metadata

2. **Wrapper script**: `[lang]_ticket.sh`
   - Executable script at project root
   - Sets up environment
   - Calls the language-specific CLI

3. **BDD test script**: `[language]/bdd.sh`
   - Exports `TICKET_SCRIPT` pointing to wrapper
   - Runs all BDD features
   - Should be executable

4. **Makefile targets**: Update root `Makefile`
   - `[language]`: Main target (runs checks)
   - `[language]-check`: Runs all quality checks
   - `[language]-lint`: Runs linter
   - `[language]-type`: Runs type checker
   - `[language]-test`: Runs unit tests
   - `[language]-bdd`: Runs BDD tests

5. **Spec review**: `docs/SPEC_REVIEW_[LANGUAGE].md`
   - Comparison with specification
   - List of discrepancies
   - Test results
   - Recommendations

## Tips and Best Practices

### Language-Specific Considerations

- **Python**: Use modern tooling (uv, ruff, mypy), src layout, type hints
- **Rust**: Use cargo workspaces, clippy, rustfmt, comprehensive error types
- **Go**: Follow standard layout, use go modules, leverage interfaces
- **JavaScript/TypeScript**: Use modern ES modules, TypeScript for type safety
- **Ruby**: Follow gem structure, use RuboCop, add type signatures with Sorbet/RBS

### Implementation Strategy

1. **Start simple**: Implement core functionality first (create, show, list)
2. **Use BDD tests**: Let the feature tests guide implementation
3. **Match the spec**: Follow docs/SPEC.md exactly, including error messages
4. **Handle edge cases**: Test with empty repos, invalid IDs, cycles, etc.
5. **Preserve behavior**: Compare output with bash reference implementation

### Common Pitfalls

- **Sorting**: Pay attention to sorting requirements (especially dep tree by subtree depth)
- **YAML handling**: Preserve formatting, handle dates correctly, maintain order
- **File operations**: Use atomic writes, handle concurrent access
- **Path handling**: Support both absolute and relative paths to .tickets
- **Error messages**: Match specification exactly for consistency
- **Exit codes**: Return appropriate codes for errors vs success

### Quality Checks

Before considering a port complete:

- [ ] All BDD features pass (40+ scenarios)
- [ ] All linting checks pass
- [ ] All type checking passes
- [ ] Manual smoke tests work in fresh directory
- [ ] Spec review completed and documented
- [ ] No missing commands from specification
- [ ] Sorting and ordering matches spec
- [ ] Error handling matches spec
- [ ] Can handle real-world usage patterns

## Example: Python Port Reference

The Python port serves as a reference implementation of this process. Key tickets:

- `tc-fa9d`: Setup project for python port
- `tc-e639`: Lint / checks in python port
- `tc-9c9c`: Scope out BDD Python (created 10 feature tickets)
- `tc-dd9d` through `tc-c8f8`: Individual feature ports
- `tc-32f5`: Spec review vs python implementation

Files created:
- `python/ticket/` - Main package directory
- `py_ticket.sh` - Wrapper script
- `python/bdd.sh` - BDD test runner
- `Makefile` - Added python targets
- `docs/SPEC_REVIEW_PYTHON.md` - Spec comparison

The Python port demonstrates:
- Using modern tooling (uv for package management)
- Systematic feature-by-feature implementation
- Integration with existing BDD tests
- Thorough spec review process

## Conclusion

This process ensures a systematic, trackable, and complete port to any new language. By using the ticket system to manage the port itself, you maintain visibility into progress and ensure no steps are missed.

Key benefits:
- **Systematic**: Each step is planned and tracked
- **Verifiable**: BDD tests ensure correctness
- **Complete**: Spec review catches missing features
- **Maintainable**: Documentation helps future ports

When starting a new port, begin by creating the tracking tickets as described in Step 1, then work through them systematically. The ticket system will guide you through the entire process.
