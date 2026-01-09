"""Command-line interface for the ticket system."""

import hashlib
import os
import subprocess
import sys
from datetime import datetime, timezone
from pathlib import Path


TICKETS_DIR = ".tickets"


def generate_id() -> str:
    """Generate ticket ID from directory name + timestamp hash."""
    dir_name = Path.cwd().name

    # Extract first letter of each hyphenated/underscored segment
    segments = dir_name.replace("-", " ").replace("_", " ").split()
    prefix = "".join(s[0] for s in segments if s)

    # Fallback to first 3 chars if no segments
    if not prefix:
        prefix = dir_name[:3]

    # 4-char hash from timestamp + PID for entropy
    entropy = f"{os.getpid()}{int(datetime.now(timezone.utc).timestamp())}"
    hash_val = hashlib.sha256(entropy.encode()).hexdigest()[:4]

    return f"{prefix}-{hash_val}"


def iso_date() -> str:
    """Get current date in ISO format."""
    return datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def ensure_dir() -> None:
    """Ensure tickets directory exists."""
    Path(TICKETS_DIR).mkdir(parents=True, exist_ok=True)


def cmd_create(args: list[str]) -> int:
    """Create a new ticket."""
    ensure_dir()

    title = ""
    description = ""
    design = ""
    acceptance = ""
    priority = 2
    issue_type = "task"
    assignee = ""
    external_ref = ""
    parent = ""

    # Default assignee to git user.name if available
    try:
        result = subprocess.run(
            ["git", "config", "user.name"],
            capture_output=True,
            text=True,
            check=False,
        )
        if result.returncode == 0:
            assignee = result.stdout.strip()
    except Exception:
        pass

    # Parse args
    i = 0
    while i < len(args):
        arg = args[i]
        if arg in ("-d", "--description"):
            if i + 1 >= len(args):
                print(f"Error: {arg} requires an argument", file=sys.stderr)
                return 1
            description = args[i + 1]
            i += 2
        elif arg == "--design":
            if i + 1 >= len(args):
                print(f"Error: {arg} requires an argument", file=sys.stderr)
                return 1
            design = args[i + 1]
            i += 2
        elif arg == "--acceptance":
            if i + 1 >= len(args):
                print(f"Error: {arg} requires an argument", file=sys.stderr)
                return 1
            acceptance = args[i + 1]
            i += 2
        elif arg in ("-p", "--priority"):
            if i + 1 >= len(args):
                print(f"Error: {arg} requires an argument", file=sys.stderr)
                return 1
            priority = int(args[i + 1])
            i += 2
        elif arg in ("-t", "--type"):
            if i + 1 >= len(args):
                print(f"Error: {arg} requires an argument", file=sys.stderr)
                return 1
            issue_type = args[i + 1]
            i += 2
        elif arg in ("-a", "--assignee"):
            if i + 1 >= len(args):
                print(f"Error: {arg} requires an argument", file=sys.stderr)
                return 1
            assignee = args[i + 1]
            i += 2
        elif arg == "--external-ref":
            if i + 1 >= len(args):
                print(f"Error: {arg} requires an argument", file=sys.stderr)
                return 1
            external_ref = args[i + 1]
            i += 2
        elif arg == "--parent":
            if i + 1 >= len(args):
                print(f"Error: {arg} requires an argument", file=sys.stderr)
                return 1
            parent = args[i + 1]
            i += 2
        elif arg.startswith("-"):
            print(f"Unknown option: {arg}", file=sys.stderr)
            return 1
        else:
            title = arg
            i += 1

    title = title or "Untitled"
    ticket_id = generate_id()
    file_path = Path(TICKETS_DIR) / f"{ticket_id}.md"
    now = iso_date()

    # Build content
    content_parts = []
    content_parts.append("---")
    content_parts.append(f"id: {ticket_id}")
    content_parts.append("status: open")
    content_parts.append("deps: []")
    content_parts.append("links: []")
    content_parts.append(f"created: {now}")
    content_parts.append(f"type: {issue_type}")
    content_parts.append(f"priority: {priority}")
    if assignee:
        content_parts.append(f"assignee: {assignee}")
    if external_ref:
        content_parts.append(f"external-ref: {external_ref}")
    if parent:
        content_parts.append(f"parent: {parent}")
    content_parts.append("---")
    content_parts.append(f"# {title}")
    content_parts.append("")

    if description:
        content_parts.append(description)
        content_parts.append("")

    if design:
        content_parts.append("## Design")
        content_parts.append("")
        content_parts.append(design)
        content_parts.append("")

    if acceptance:
        content_parts.append("## Acceptance Criteria")
        content_parts.append("")
        content_parts.append(acceptance)
        content_parts.append("")

    file_path.write_text("\n".join(content_parts))

    print(ticket_id)
    return 0


def main() -> int:
    """Main entry point for the ticket CLI."""
    args = sys.argv[1:]

    if not args or args[0] in ("-h", "--help", "help"):
        print_help()
        return 0

    command = args[0]
    command_args = args[1:]

    if command == "create":
        return cmd_create(command_args)

    print("Ticket CLI - Python port (work in progress)")
    print(f"Command not yet implemented: {command}")
    return 1


def print_help() -> None:
    """Print help message."""
    print("""tk - minimal ticket system with dependency tracking

Usage: tk <command> [args]

Commands:
  create [title] [options] Create ticket, prints ID
    -d, --description      Description text
    --design               Design notes
    --acceptance           Acceptance criteria
    -t, --type             Type (bug|feature|task|epic|chore) [default: task]
    -p, --priority         Priority 0-4, 0=highest [default: 2]
    -a, --assignee         Assignee [default: git user.name]
    --external-ref         External reference (e.g., gh-123, JIRA-456)
    --parent               Parent ticket ID
  start <id>               Set status to in_progress
  close <id>               Set status to closed
  reopen <id>              Set status to open
  status <id> <status>     Update status (open|in_progress|closed)
  dep <id> <dep-id>        Add dependency (id depends on dep-id)
  dep tree [--full] <id>   Show dependency tree (--full disables dedup)
  undep <id> <dep-id>      Remove dependency
  link <id> <id> [id...]   Link tickets together (symmetric)
  unlink <id> <target-id>  Remove link between tickets
  ls [--status=X]          List tickets
  ready                    List open/in-progress tickets with deps resolved
  blocked                  List open/in-progress tickets with unresolved deps
  closed [--limit=N]       List recently closed tickets (default 20, by mtime)
  show <id>                Display ticket
  edit <id>                Open ticket in $EDITOR
  add-note <id> [text]     Append timestamped note (or pipe via stdin)
  query [jq-filter]        Output tickets as JSON, optionally filtered
  migrate-beads            Import tickets from .beads/issues.jsonl

Tickets stored as markdown files in .tickets/
Supports partial ID matching (e.g., 'tk show 5c4' matches 'nw-5c46')
""")


if __name__ == "__main__":
    sys.exit(main())
