## 1. Plugin Scaffold

- [x] 1.1 Create `plugins/ticket-workflow` with shebang, metadata comments, and subcommand dispatch (list/run)
- [x] 1.2 Add usage/help output for `tk workflow` with no args or invalid subcommand

## 2. TOML Parser

- [x] 2.1 Implement awk/sed TOML parser for workflow files: extract top-level fields (workflow, description, version, type)
- [x] 2.2 Parse `[vars.<name>]` sections with description, required, default, pattern, enum fields
- [x] 2.3 Parse `[[steps]]` array tables with id, title, description, needs, type fields

## 3. Workflow Discovery

- [x] 3.1 Implement search path logic: `.tickets/workflows/*.toml` then `~/.config/ticket/workflows/*.toml`
- [x] 3.2 Implement `tk workflow list` — display name, description, and source for each workflow
- [x] 3.3 Handle project-overrides-user precedence for same-named workflows

## 4. Variable Handling

- [x] 4.1 Parse `--var key=value` flags from command line arguments
- [x] 4.2 Validate required variables are provided, apply defaults for optional ones
- [x] 4.3 Validate pattern constraints (regex match)
- [x] 4.4 Validate enum constraints (value in allowed set)
- [x] 4.5 Implement `{{var}}` substitution in step titles and descriptions

## 5. Workflow Instantiation

- [x] 5.1 Create parent ticket from workflow name/description using `$TK_SCRIPT super create`
- [x] 5.2 Create child tickets for each step with `--parent` set to parent ID
- [x] 5.3 Add dependencies between child tickets based on `needs` declarations using `$TK_SCRIPT super dep`
- [x] 5.4 Print summary of created tickets
- [x] 5.5 Implement `--dry-run` flag to preview without creating

## 6. Testing & Documentation

- [x] 6.1 Add behave scenarios for `tk workflow list` and `tk workflow run`
- [x] 6.2 Create an example workflow file (e.g., `release.toml`) for testing
- [x] 6.3 Update README.md usage section with workflow commands
- [x] 6.4 Update CHANGELOG.md
