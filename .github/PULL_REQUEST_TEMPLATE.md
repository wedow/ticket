## Description

Add comprehensive shell completion support for both Bash and Zsh shells.

## Changes

### New Files

- `ticket-completion.bash` - Bash completion script
- `ticket-completion.zsh` - Zsh completion script
- `install-completion.sh` - Automated installation script for both shells
- `diagnose-completion.sh` - Diagnostic tool for troubleshooting
- `fix-completion.sh` - Automatic fix script for common issues

### Modified Files

- `README.md` - Updated with autocomplete documentation

## Features

### Completion Support

- ✅ All commands (`create`, `start`, `close`, `reopen`, `status`, `dep`, `undep`, `link`, `unlink`, `ls`, `ready`, `blocked`, `closed`, `show`, `edit`, `add-note`, `query`, `migrate-beads`, `help`)
- ✅ Ticket IDs with partial matching support
- ✅ Status values (`open`, `in_progress`, `closed`)
- ✅ Ticket types (`bug`, `feature`, `task`, `epic`, `chore`)
- ✅ Priority levels (0-4)
- ✅ Command-line options (`--type`, `--priority`, `--parent`, etc.)
- ✅ Subcommands (`dep tree`, `--full`, etc.)
- ✅ Multi-argument commands (`link ID1 ID2 ID3`)
- ✅ Works with both `ticket` and `tk` commands

### Installation

```bash
./install-completion.sh
exec zsh  # or exec bash
```

### Usage Examples

```bash
tk <TAB>                    # List all commands
tk create --type <TAB>      # Show available types
tk show <TAB>               # List ticket IDs
tk status <ID> <TAB>        # Show status options
tk dep tree --<TAB>         # Show subcommand options
```

## Testing

Tested on:

- ✅ macOS with Zsh 5.9
- ✅ macOS with Homebrew
- ✅ Both `ticket` and `tk` commands

### Test Steps

1. Install completion: `./install-completion.sh`
2. Reload shell: `exec zsh`
3. Test basic completion: `tk <TAB>`
4. Create test ticket: `tk create "Test"`
5. Test ID completion: `tk show <TAB>`
6. Test option completion: `tk create --type <TAB>`

## Troubleshooting

If completion doesn't work:

```bash
./diagnose-completion.sh  # Identify issues
./fix-completion.sh       # Auto-fix common problems
exec zsh                  # Reload shell
```

## Breaking Changes

None. This is a pure addition with no changes to existing functionality.

## Checklist

- [x] Code follows project style guidelines
- [x] Tested on macOS with Zsh
- [x] Tested on macOS with Bash
- [x] Documentation updated (README.md)
- [x] Installation script provided
- [x] Diagnostic tools included
- [x] Works with both `ticket` and `tk` aliases
- [x] No breaking changes

## Related Issues

Closes #[issue-number] (if applicable)
