# Add Shell Completion Support (Bash & Zsh)

## 🎯 Overview

This PR adds comprehensive shell completion support for the `ticket` CLI tool, supporting both Bash and Zsh shells. The completion works with both the `ticket` command and the `tk` alias.

## 📦 What's New

### Completion Scripts

- **`ticket-completion.bash`** - Full Bash completion support
- **`ticket-completion.zsh`** - Native Zsh completion with descriptions

### Installation Tools

- **`install-completion.sh`** - Smart installer that detects shell and OS
- **`diagnose-completion.sh`** - Diagnostic tool for troubleshooting
- **`fix-completion.sh`** - Automatic fix for common configuration issues

### Documentation

- **`README.md`** - Updated with installation and usage instructions

## ✨ Features

### Complete Coverage

All commands and options are supported:

```bash
tk <TAB>                    # Commands: create, start, close, status, dep, etc.
tk create --type <TAB>      # Types: bug, feature, task, epic, chore
tk create --priority <TAB>  # Priorities: 0, 1, 2, 3, 4
tk show <TAB>               # Lists all ticket IDs from .tickets/
tk status <ID> <TAB>        # Statuses: open, in_progress, closed
tk dep <TAB>                # Subcommand 'tree' + ticket IDs
tk dep tree --<TAB>         # Options: --full
tk link <ID1> <TAB>         # Additional ticket IDs
```

### Smart Completion

- **Ticket ID discovery**: Automatically reads IDs from `.tickets/*.md`
- **Partial matching**: `tk show abc<TAB>` completes IDs starting with "abc"
- **Context-aware**: Different completions based on command position
- **Multi-argument**: Supports commands like `link` that accept multiple IDs

### Cross-Platform

- ✅ macOS (Homebrew & standalone)
- ✅ Linux (Debian, Ubuntu, Fedora, etc.)
- ✅ Bash 3.2+ and Zsh 5.0+

## 🚀 Installation

### Quick Start

```bash
./install-completion.sh
exec zsh  # or exec bash
```

### Manual Installation

**Bash:**

```bash
sudo cp ticket-completion.bash /usr/share/bash-completion/completions/ticket
```

**Zsh:**

```bash
cp ticket-completion.zsh $(brew --prefix)/share/zsh/site-functions/_ticket
```

## 🧪 Testing

### Test Scenarios Covered

1. ✅ Command completion
2. ✅ Ticket ID completion with partial matching
3. ✅ Status and type completion
4. ✅ Priority completion (0-4)
5. ✅ Subcommand completion (`dep tree`)
6. ✅ Options with values (`--type`, `--parent`)
7. ✅ Multi-argument commands (`link`)
8. ✅ Both `ticket` and `tk` aliases

### Tested On

- macOS Sequoia (Zsh 5.9)
- macOS with Homebrew
- Bash 3.2 (macOS default)
- Bash 5.x (Linux)

## 🔧 Troubleshooting

If completion doesn't work after installation:

```bash
# 1. Diagnose the issue
./diagnose-completion.sh

# 2. Apply automatic fixes
./fix-completion.sh

# 3. Reload shell
exec zsh
```

Common issues handled:

- Missing `fpath` configuration
- Missing `compinit` in `.zshrc`
- Completion cache issues

## 📝 Implementation Details

### Bash Completion

- Uses `complete -F` for function-based completion
- Leverages `compgen` for efficient word matching
- Avoids `_init_completion` dependency for portability

### Zsh Completion

- Native `#compdef` directive
- Uses `_arguments` for structured completion
- Provides command descriptions
- Supports advanced Zsh features (interactive selection)

### Design Decisions

1. **Separate files**: Bash and Zsh have different syntaxes - keeping them separate ensures maintainability
2. **No external dependencies**: Works with vanilla Bash/Zsh installations
3. **Graceful degradation**: If `.tickets/` doesn't exist, still completes commands
4. **Performance**: Ticket ID discovery is fast even with hundreds of tickets

## 🔄 Backward Compatibility

- ✅ No changes to existing code
- ✅ No changes to existing CLI behavior
- ✅ Purely additive feature
- ✅ Optional installation (doesn't affect users who don't install)

## 📚 Documentation

Updated `README.md` with:

- Quick installation guide
- Usage examples
- Troubleshooting section
- Feature list

## 🎓 Usage Examples

### Basic Workflow

```bash
# Create a ticket
tk create "Fix login bug" --type bug --priority 0

# Complete the ID
tk start <TAB>  # Shows all ticket IDs

# Complete status
tk status BUG-1234 <TAB>  # Shows: open in_progress closed

# Add dependency
tk dep BUG-1234 <TAB>  # Shows other ticket IDs

# View tree
tk dep tree --<TAB>  # Shows: --full
tk dep tree BUG-1234
```

### Advanced Features

```bash
# Link multiple tickets
tk link <TAB> <TAB> <TAB>  # Keep suggesting IDs

# Create with parent
tk create "Sub-task" --parent <TAB>  # Shows parent IDs

# Filter by status
tk ls --status=<TAB>  # Shows: open in_progress closed
```

## 🙏 Credits

Implemented following shell completion best practices:

- Bash completion framework conventions
- Zsh completion system guidelines
- Homebrew installation patterns

## 📋 Checklist

- [x] Bash completion implemented
- [x] Zsh completion implemented
- [x] Installation script provided
- [x] Diagnostic tools included
- [x] Documentation updated
- [x] Tested on macOS
- [x] Tested with both `ticket` and `tk`
- [x] No breaking changes
- [x] Follows conventional commits

## 🔗 Related

This PR addresses the need for improved developer experience when using the `ticket` CLI tool daily.

---

**Ready for review!** 🚀
