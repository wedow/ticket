# ticket

A git-backed issue tracker for AI agents. Rooted in the Unix Philosophy, `tk` is inspired by Joe Armstrong's [Minimal Viable Program](https://joearms.github.io/published/2014-06-25-minimal-viable-program.html) with additional quality of life features for managing and querying against complex issue dependency graphs.

Tickets are markdown files with YAML frontmatter in `.tickets/`. This allows AI agents to easily search them for relevant content without dumping ten thousand character JSONL lines into their context window.

Using ticket IDs as file names also allows IDEs to quickly navigate to the ticket. For example, you might run `git log` in your terminal and see something like:

```
nw-5c46: add SSE connection management
```

VS Code allows you to Ctrl+Click or Cmd+Click the ID and jump directly to the file to read the details.

## Install

```bash
git clone https://github.com/EnderRealm/ticket.git
cd ticket && ln -s "$PWD/ticket" ~/.local/bin/tk
```

Or just copy `ticket` to somewhere in your PATH.

## Requirements

`tk` is a portable bash script requiring only coreutils, so it works out of the box on any POSIX system with bash installed. The `query` command requires `jq`. Uses `rg` (ripgrep) if available, falls back to `grep`.

## Configuration

Set `TICKETS_DIR` to store tickets in a custom location (default: `.tickets`):

```bash
export TICKETS_DIR=".tasks"
tk create "my ticket"
```

## Agent Setup

Add this line to your `CLAUDE.md` or `AGENTS.md`:

```
This project uses a CLI ticket system for task management. Run `tk help` when you need to use it.
```

Claude Opus picks it up naturally from there. Other models may need additional guidance.

## Usage

```bash
tk - minimal ticket system with dependency tracking

Usage: tk <command> [args]

Commands:
  create [title] [options] Create ticket (interactive if no title)
    -d, --description      Description text
    --design               Design notes
    --acceptance           Acceptance criteria
    -t, --type             Type (bug|feature|task|epic|chore) [default: task]
    -p, --priority         Priority 0-4, 0=highest [default: 2]
    -a, --assignee         Assignee [default: git user.name]
    --id                   Custom ticket ID (e.g., --id my-feature) [default: auto-generated]
    --external-ref         External reference (e.g., gh-123, JIRA-456)
    --parent               Parent ticket ID
    --tags                 Comma-separated tags (e.g., --tags ui,backend,urgent)
  start <id>               Set status to in_progress
  close <id>               Set status to closed
  reopen <id>              Set status to open
  delete <id> [id...]      Delete ticket file(s)
  status <id> <status>     Update status (open|in_progress|closed)
  dep <id> <dep-id>        Add dependency (id depends on dep-id)
  dep tree [--full] <id>   Show dependency tree (--full disables dedup)
  dep cycle                Find dependency cycles in open tickets
  undep <id> <dep-id>      Remove dependency
  link <id> <id> [id...]   Link tickets together (symmetric)
  unlink <id> <target-id>  Remove link between tickets
  ls|list [options]        List tickets
  ready [options]          List actionable tickets (open/in_progress, deps resolved)
  blocked [options]        List tickets with unresolved deps
  closed [options]         List recently closed tickets (default 20, by mtime)
  show <id>                Display ticket
  edit <id> [options]      Update ticket fields (same flags as create)
  add-note <id> [text]     Append timestamped note (or pipe via stdin)
  query [jq-filter]        Output tickets as JSON, optionally filtered

Filter flags (for ls, ready, blocked, closed):
  -a, --assignee X         Filter by assignee
  -t, --type X             Filter by type
  -T, --tag X              Filter by tag
  -P, --priority X         Filter by priority (0-4)
  --status X               Filter by status (ls only)
  --limit N                Limit results (closed only)

Tickets stored as markdown files in .tickets/
Supports partial ID matching (e.g., 'tk show 5c4' matches 'nw-5c46')
```

## License

MIT
