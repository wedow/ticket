# Contributing to ticket

Thank you for your interest in contributing! This document outlines how to set up the project, run tests, and submit changes.

## Development Setup

### Prerequisites

- Bash (POSIX-compatible)
- coreutils
- `jq` (for the `query` command)
- `rg` / ripgrep (optional, falls back to grep)

### Install from Source

```bash
git clone https://github.com/wedow/ticket.git
cd ticket
ln -s "$PWD/ticket" ~/.local/bin/tk
# Or add to PATH
```

Verify installation:
```bash
tk help
```

### Running Tests

Tests use [behave](https://behave.readthedocs.io/) (Python BDD framework).

With `uv` installed:
```bash
make test
```

Without `uv`:
```bash
pip install behave
make test
```

## Coding Standards

- Write POSIX-compatible bash scripts
- Keep dependencies minimal (coreutils + optional jq/rg)
- Follow the Unix Philosophy: do one thing well
- Add plugin descriptions using `# tk-plugin: description` in the first 10 lines

## Submitting Changes

1. Fork the repository
2. Create a feature branch: `git checkout -b my-feature`
3. Make your changes
4. Run tests: `make test`
5. Commit with a clear message
6. Push to your fork
7. Open a pull request against `wedow/ticket`

## Project Structure

```
ticket          # Main executable script
CLAUDE.md      # AI agent instructions
Makefile       # Build/test targets
features/      # behave test scenarios
plugins/       # Bundled plugins
pkg/           # Library functions
scripts/       # Utility scripts
```

## Getting Help

- Open an issue for bugs or feature requests
- Use `tk help` for CLI reference
- Check README.md for usage examples
