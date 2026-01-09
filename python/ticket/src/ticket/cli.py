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


def ticket_path(ticket_id: str) -> Path:
    """Get ticket file path, supporting partial ID matching."""
    tickets_dir = Path(TICKETS_DIR)
    exact = tickets_dir / f"{ticket_id}.md"

    if exact.is_file():
        return exact

    # Try partial match (anywhere in filename)
    matches = list(tickets_dir.glob(f"*{ticket_id}*.md"))

    if len(matches) == 1:
        return matches[0]
    elif len(matches) > 1:
        print(
            f"Error: ambiguous ID '{ticket_id}' matches multiple tickets",
            file=sys.stderr,
        )
        sys.exit(1)
    else:
        print(f"Error: ticket '{ticket_id}' not found", file=sys.stderr)
        sys.exit(1)


def parse_ticket(file_path: Path) -> dict:
    """Parse ticket file and extract frontmatter and content."""
    content = file_path.read_text()
    lines = content.split("\n")

    frontmatter = {}
    body_lines = []
    in_frontmatter = False
    frontmatter_ended = False

    for line in lines:
        if line.strip() == "---":
            if not in_frontmatter:
                in_frontmatter = True
            else:
                frontmatter_ended = True
                in_frontmatter = False
            continue

        if in_frontmatter:
            if ":" in line:
                key, value = line.split(":", 1)
                frontmatter[key.strip()] = value.strip()
        elif frontmatter_ended:
            body_lines.append(line)

    # Extract title from body (first line starting with #)
    title = ""
    for line in body_lines:
        if line.startswith("# "):
            title = line[2:].strip()
            break

    return {
        "frontmatter": frontmatter,
        "body": "\n".join(body_lines),
        "title": title,
        "path": file_path,
    }


def load_all_tickets() -> dict[str, dict]:
    """Load all tickets and return dict keyed by ticket ID."""
    tickets_dir = Path(TICKETS_DIR)
    if not tickets_dir.exists():
        return {}

    tickets = {}
    for ticket_file in tickets_dir.glob("*.md"):
        ticket_data = parse_ticket(ticket_file)
        ticket_id = ticket_data["frontmatter"].get("id", "")
        if ticket_id:
            tickets[ticket_id] = ticket_data

    return tickets


def parse_list_field(value: str) -> list[str]:
    """Parse a YAML list field like '[a, b, c]' into a Python list."""
    if not value:
        return []
    value = value.strip()
    if value == "[]":
        return []
    # Remove brackets and split by comma
    value = value.strip("[]")
    return [item.strip() for item in value.split(",") if item.strip()]


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


def cmd_show(args: list[str]) -> int:
    """Display a ticket with related information."""
    if not args:
        print("Usage: ticket show <id>", file=sys.stderr)
        return 1

    ticket_id = args[0]

    # Get the ticket file path
    try:
        file_path = ticket_path(ticket_id)
    except SystemExit:
        return 1

    target_id = file_path.stem

    # Load all tickets to build relationships
    all_tickets = load_all_tickets()

    if target_id not in all_tickets:
        print(f"Error: ticket '{ticket_id}' not found", file=sys.stderr)
        return 1

    target_ticket = all_tickets[target_id]
    target_fm = target_ticket["frontmatter"]

    # Build relationship lists
    blockers = []
    blocking = []
    children = []
    linked = []

    # Parse target ticket's deps and links
    target_deps = parse_list_field(target_fm.get("deps", "[]"))
    target_links = parse_list_field(target_fm.get("links", "[]"))
    target_parent = target_fm.get("parent", "")

    # Scan all tickets for relationships
    for tid, ticket in all_tickets.items():
        fm = ticket["frontmatter"]
        status = fm.get("status", "open")

        # Check if this ticket depends on target (target is blocking it)
        deps = parse_list_field(fm.get("deps", "[]"))
        if target_id in deps and status != "closed":
            blocking.append((tid, status, ticket["title"]))

        # Check if this ticket is a child of target
        parent = fm.get("parent", "")
        if parent == target_id:
            children.append((tid, status, ticket["title"]))

    # Find blockers (unclosed dependencies)
    for dep in target_deps:
        if dep in all_tickets:
            dep_status = all_tickets[dep]["frontmatter"].get("status", "open")
            if dep_status != "closed":
                blockers.append((dep, dep_status, all_tickets[dep]["title"]))

    # Find linked tickets
    for link in target_links:
        if link in all_tickets:
            link_status = all_tickets[link]["frontmatter"].get("status", "open")
            linked.append((link, link_status, all_tickets[link]["title"]))

    # Output the ticket content with enhancements
    content = file_path.read_text()
    lines = content.split("\n")
    in_frontmatter = False

    for line in lines:
        if line.strip() == "---":
            if not in_frontmatter:
                in_frontmatter = True
                print(line)
            else:
                in_frontmatter = False
                print(line)
            continue

        if in_frontmatter:
            # Enhance parent field with title comment
            if line.startswith("parent:") and target_parent:
                if target_parent in all_tickets:
                    parent_title = all_tickets[target_parent]["title"]
                    print(f"{line}  # {parent_title}")
                else:
                    print(line)
            else:
                print(line)
        else:
            print(line)

    # Add relationship sections
    if blockers:
        print()
        print("## Blockers")
        print()
        for dep_id, status, title in blockers:
            print(f"- {dep_id} [{status}] {title}")

    if blocking:
        print()
        print("## Blocking")
        print()
        for tid, status, title in blocking:
            print(f"- {tid} [{status}] {title}")

    if children:
        print()
        print("## Children")
        print()
        for tid, status, title in children:
            print(f"- {tid} [{status}] {title}")

    if linked:
        print()
        print("## Linked")
        print()
        for tid, status, title in linked:
            print(f"- {tid} [{status}] {title}")

    return 0


def cmd_add_note(args: list[str]) -> int:
    """Add a timestamped note to a ticket."""
    if not args:
        print("Usage: ticket add-note <id> [note text]", file=sys.stderr)
        return 1

    ticket_id = args[0]

    try:
        file_path = ticket_path(ticket_id)
    except SystemExit:
        return 1

    target_id = file_path.stem

    # Get note text from args or stdin
    if len(args) > 1:
        note = " ".join(args[1:])
    elif not sys.stdin.isatty():
        note = sys.stdin.read().strip()
    else:
        print("Error: no note provided", file=sys.stderr)
        return 1

    timestamp = iso_date()

    # Read existing content
    content = file_path.read_text()

    # Add Notes section if missing
    if "## Notes" not in content:
        content += "\n## Notes\n"

    # Append timestamped note
    content += f"\n**{timestamp}**\n\n{note}\n"

    # Write back
    file_path.write_text(content)

    print(f"Note added to {target_id}")
    return 0


def update_yaml_field(file_path: Path, field: str, value: str) -> None:
    """Update a YAML field in the frontmatter of a ticket file."""
    content = file_path.read_text()
    lines = content.split("\n")
    
    updated = False
    in_frontmatter = False
    result_lines = []
    
    for line in lines:
        if line.strip() == "---":
            result_lines.append(line)
            if not in_frontmatter:
                in_frontmatter = True
            else:
                in_frontmatter = False
            continue
        
        if in_frontmatter and line.startswith(f"{field}:"):
            result_lines.append(f"{field}: {value}")
            updated = True
        else:
            result_lines.append(line)
    
    # If field wasn't found, insert it after first ---
    if not updated:
        new_lines = []
        first_marker_found = False
        for line in result_lines:
            new_lines.append(line)
            if not first_marker_found and line.strip() == "---":
                first_marker_found = True
                new_lines.append(f"{field}: {value}")
        result_lines = new_lines
    
    file_path.write_text("\n".join(result_lines))


def cmd_status(args: list[str]) -> int:
    """Update the status of a ticket."""
    VALID_STATUSES = ["open", "in_progress", "closed"]
    
    if len(args) < 2:
        print("Usage: ticket status <id> <status>", file=sys.stderr)
        print(f"Valid statuses: {' '.join(VALID_STATUSES)}", file=sys.stderr)
        return 1
    
    ticket_id = args[0]
    status = args[1]
    
    # Validate status
    if status not in VALID_STATUSES:
        print(f"Error: invalid status '{status}'. Must be one of: {' '.join(VALID_STATUSES)}", file=sys.stderr)
        return 1
    
    # Get ticket file
    try:
        file_path = ticket_path(ticket_id)
    except SystemExit:
        return 1
    
    target_id = file_path.stem
    
    # Update status field
    update_yaml_field(file_path, "status", status)
    
    print(f"Updated {target_id} -> {status}")
    return 0


def cmd_start(args: list[str]) -> int:
    """Set ticket status to in_progress."""
    if not args:
        print("Usage: ticket start <id>", file=sys.stderr)
        return 1
    
    return cmd_status([args[0], "in_progress"])


def cmd_close(args: list[str]) -> int:
    """Set ticket status to closed."""
    if not args:
        print("Usage: ticket close <id>", file=sys.stderr)
        return 1
    
    return cmd_status([args[0], "closed"])


def cmd_reopen(args: list[str]) -> int:
    """Set ticket status to open."""
    if not args:
        print("Usage: ticket reopen <id>", file=sys.stderr)
        return 1
    
    return cmd_status([args[0], "open"])


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
    elif command == "show":
        return cmd_show(command_args)
    elif command == "add-note":
        return cmd_add_note(command_args)
    elif command == "status":
        return cmd_status(command_args)
    elif command == "start":
        return cmd_start(command_args)
    elif command == "close":
        return cmd_close(command_args)
    elif command == "reopen":
        return cmd_reopen(command_args)

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
