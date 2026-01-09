const std = @import("std");

const stdout_file = std.fs.File{ .handle = std.posix.STDOUT_FILENO };
const stderr_file = std.fs.File{ .handle = std.posix.STDERR_FILENO };
const tickets_dir = ".tickets";

const RelationshipItem = struct {
    id: []const u8,
    status: []const u8,
    title: []const u8,
};

pub fn main() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        printUsage();
        return 1;
    }

    const command = args[1];

    if (std.mem.eql(u8, command, "create")) {
        return handleCreate(allocator, args[2..]);
    } else if (std.mem.eql(u8, command, "status")) {
        return handleStatus(allocator, args[2..]);
    } else if (std.mem.eql(u8, command, "start")) {
        return handleStart(allocator, args[2..]);
    } else if (std.mem.eql(u8, command, "close")) {
        return handleClose(allocator, args[2..]);
    } else if (std.mem.eql(u8, command, "reopen")) {
        return handleReopen(allocator, args[2..]);
    } else if (std.mem.eql(u8, command, "show")) {
        return handleShow(allocator, args[2..]);
    } else if (std.mem.eql(u8, command, "ls")) {
        return handleList(allocator, args[2..]);
    } else if (std.mem.eql(u8, command, "ready")) {
        return handleReady(allocator, args[2..]);
    } else if (std.mem.eql(u8, command, "blocked")) {
        return handleBlocked(allocator, args[2..]);
    } else if (std.mem.eql(u8, command, "closed")) {
        return handleClosedList(allocator, args[2..]);
    } else if (std.mem.eql(u8, command, "dep")) {
        return handleDep(allocator, args[2..]);
    } else if (std.mem.eql(u8, command, "undep")) {
        return handleUndep(allocator, args[2..]);
    } else if (std.mem.eql(u8, command, "link")) {
        return handleLink(allocator, args[2..]);
    } else if (std.mem.eql(u8, command, "unlink")) {
        return handleUnlink(allocator, args[2..]);
    } else if (std.mem.eql(u8, command, "edit")) {
        return handleEdit(allocator, args[2..]);
    } else if (std.mem.eql(u8, command, "add-note")) {
        return handleAddNote(allocator, args[2..]);
    } else if (std.mem.eql(u8, command, "query")) {
        return handleQuery(allocator, args[2..]);
    } else if (std.mem.eql(u8, command, "migrate-beads")) {
        return handleMigrateBeads(allocator, args[2..]);
    } else {
        var buf: [256]u8 = undefined;
        const msg = try std.fmt.bufPrint(&buf, "Error: unknown command '{s}'\n", .{command});
        try stdout_file.writeAll(msg);
        printUsage();
        return 1;
    }
}

fn printUsage() void {
    const stdout = stdout_file;
    stdout.writeAll(
        \\Usage: ticket <command> [options]
        \\
        \\Commands:
        \\  create [title] [options]  Create a new ticket
        \\  status <id> <status>      Set ticket status
        \\  start <id>                Set ticket status to in_progress
        \\  close <id>                Set ticket status to closed
        \\  reopen <id>               Set ticket status to open
        \\  show <id>                 Display full ticket details
        \\  ls [--status=X]           List all tickets
        \\  ready                     List ready tickets
        \\  blocked                   List blocked tickets
        \\  closed [--limit=N]        List closed tickets
        \\  dep <id> <dep-id>         Add dependency
        \\  dep tree [--full] <id>    Display dependency tree
        \\  undep <id> <dep-id>       Remove dependency
        \\  link <id> <id> [id...]    Create symmetric links
        \\  unlink <id> <target-id>   Remove symmetric link
        \\  edit <id>                 Edit ticket in $EDITOR
        \\  add-note <id> [text]      Add timestamped note
        \\  query [jq-filter]         Output tickets as JSONL
        \\  migrate-beads             Import from .beads/issues.jsonl
        \\
    ) catch {};
}

fn handleCreate(allocator: std.mem.Allocator, args: []const [:0]const u8) !u8 {
    try ensureTicketsDir();
    
    var title: ?[]const u8 = null;
    var description: ?[]const u8 = null;
    var design: ?[]const u8 = null;
    var acceptance: ?[]const u8 = null;
    var priority: i32 = 2;
    var issue_type: []const u8 = "task";
    var assignee: ?[]u8 = null;
    var external_ref: ?[]const u8 = null;
    var parent: ?[]const u8 = null;
    
    // Try to get git user.name as default assignee
    assignee = try getGitUserName(allocator);
    
    // Parse command line arguments
    var i: usize = 0;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        
        if (std.mem.eql(u8, arg, "-d") or std.mem.eql(u8, arg, "--description")) {
            if (i + 1 >= args.len) {
                try stderr_file.writeAll("Error: -d/--description requires an argument\n");
                return 1;
            }
            i += 1;
            description = args[i];
        } else if (std.mem.eql(u8, arg, "--design")) {
            if (i + 1 >= args.len) {
                try stderr_file.writeAll("Error: --design requires an argument\n");
                return 1;
            }
            i += 1;
            design = args[i];
        } else if (std.mem.eql(u8, arg, "--acceptance")) {
            if (i + 1 >= args.len) {
                try stderr_file.writeAll("Error: --acceptance requires an argument\n");
                return 1;
            }
            i += 1;
            acceptance = args[i];
        } else if (std.mem.eql(u8, arg, "-p") or std.mem.eql(u8, arg, "--priority")) {
            if (i + 1 >= args.len) {
                try stderr_file.writeAll("Error: -p/--priority requires an argument\n");
                return 1;
            }
            i += 1;
            priority = try std.fmt.parseInt(i32, args[i], 10);
        } else if (std.mem.eql(u8, arg, "-t") or std.mem.eql(u8, arg, "--type")) {
            if (i + 1 >= args.len) {
                try stderr_file.writeAll("Error: -t/--type requires an argument\n");
                return 1;
            }
            i += 1;
            issue_type = args[i];
        } else if (std.mem.eql(u8, arg, "-a") or std.mem.eql(u8, arg, "--assignee")) {
            if (i + 1 >= args.len) {
                try stderr_file.writeAll("Error: -a/--assignee requires an argument\n");
                return 1;
            }
            i += 1;
            if (assignee) |old_assignee| {
                allocator.free(old_assignee);
            }
            assignee = try allocator.dupe(u8, args[i]);
        } else if (std.mem.eql(u8, arg, "--external-ref")) {
            if (i + 1 >= args.len) {
                try stderr_file.writeAll("Error: --external-ref requires an argument\n");
                return 1;
            }
            i += 1;
            external_ref = args[i];
        } else if (std.mem.eql(u8, arg, "--parent")) {
            if (i + 1 >= args.len) {
                try stderr_file.writeAll("Error: --parent requires an argument\n");
                return 1;
            }
            i += 1;
            parent = args[i];
        } else if (std.mem.startsWith(u8, arg, "-")) {
            var buf: [256]u8 = undefined;
            const msg = try std.fmt.bufPrint(&buf, "Unknown option: {s}\n", .{arg});
            try stderr_file.writeAll(msg);
            return 1;
        } else {
            title = arg;
        }
    }
    
    defer if (assignee) |a| allocator.free(a);
    
    const final_title = title orelse "Untitled";
    const ticket_id = try generateTicketID(allocator);
    defer allocator.free(ticket_id);
    
    const timestamp = try getCurrentTimestamp(allocator);
    defer allocator.free(timestamp);
    
    // Build file path
    var file_path_buf: [256]u8 = undefined;
    const file_path = try std.fmt.bufPrint(&file_path_buf, "{s}/{s}.md", .{ tickets_dir, ticket_id });
    
    // Build content
    var content: std.ArrayList(u8) = .empty;
    defer content.deinit(allocator);
    
    try content.appendSlice(allocator, "---\n");
    try content.appendSlice(allocator, "id: ");
    try content.appendSlice(allocator, ticket_id);
    try content.appendSlice(allocator, "\nstatus: open\n");
    try content.appendSlice(allocator, "deps: []\n");
    try content.appendSlice(allocator, "links: []\n");
    try content.appendSlice(allocator, "created: ");
    try content.appendSlice(allocator, timestamp);
    try content.appendSlice(allocator, "\ntype: ");
    try content.appendSlice(allocator, issue_type);
    try content.appendSlice(allocator, "\npriority: ");
    
    var priority_buf: [16]u8 = undefined;
    const priority_str = try std.fmt.bufPrint(&priority_buf, "{d}", .{priority});
    try content.appendSlice(allocator, priority_str);
    try content.appendSlice(allocator, "\n");
    
    if (assignee) |a| {
        try content.appendSlice(allocator, "assignee: ");
        try content.appendSlice(allocator, a);
        try content.appendSlice(allocator, "\n");
    }
    
    if (external_ref) |ref| {
        try content.appendSlice(allocator, "external-ref: ");
        try content.appendSlice(allocator, ref);
        try content.appendSlice(allocator, "\n");
    }
    
    if (parent) |p| {
        try content.appendSlice(allocator, "parent: ");
        try content.appendSlice(allocator, p);
        try content.appendSlice(allocator, "\n");
    }
    
    try content.appendSlice(allocator, "---\n# ");
    try content.appendSlice(allocator, final_title);
    try content.appendSlice(allocator, "\n\n");
    
    if (description) |desc| {
        try content.appendSlice(allocator, desc);
        try content.appendSlice(allocator, "\n\n");
    }
    
    if (design) |des| {
        try content.appendSlice(allocator, "## Design\n\n");
        try content.appendSlice(allocator, des);
        try content.appendSlice(allocator, "\n\n");
    }
    
    if (acceptance) |acc| {
        try content.appendSlice(allocator, "## Acceptance Criteria\n\n");
        try content.appendSlice(allocator, acc);
        try content.appendSlice(allocator, "\n\n");
    }
    
    // Write file
    try std.fs.cwd().writeFile(.{ .sub_path = file_path, .data = content.items });
    
    // Output ticket ID
    try stdout_file.writeAll(ticket_id);
    try stdout_file.writeAll("\n");
    
    return 0;
}

fn handleStatus(allocator: std.mem.Allocator, args: []const [:0]const u8) !u8 {
    const valid_statuses = [_][]const u8{ "open", "in_progress", "closed" };
    
    if (args.len < 2) {
        try stderr_file.writeAll("Usage: ticket status <id> <status>\n");
        return 1;
    }

    const ticket_id = args[0];
    const new_status = args[1];

    // Validate status
    var is_valid = false;
    for (valid_statuses) |valid| {
        if (std.mem.eql(u8, new_status, valid)) {
            is_valid = true;
            break;
        }
    }
    
    if (!is_valid) {
        var buf: [512]u8 = undefined;
        const msg = try std.fmt.bufPrint(&buf, "Error: invalid status '{s}'. Must be one of: open in_progress closed\n", .{new_status});
        try stderr_file.writeAll(msg);
        return 1;
    }

    // Resolve ticket ID
    const file_path = resolveTicketID(allocator, ticket_id) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = if (err == error.NotFound)
            try std.fmt.bufPrint(&buf, "Error: ticket '{s}' not found\n", .{ticket_id})
        else if (err == error.Ambiguous)
            try std.fmt.bufPrint(&buf, "Error: ambiguous ID '{s}' matches multiple tickets\n", .{ticket_id})
        else
            try std.fmt.bufPrint(&buf, "Error resolving ticket ID\n", .{});
        try stderr_file.writeAll(msg);
        return 1;
    };
    defer allocator.free(file_path);

    // Extract the target ID from the file path
    const target_id = blk: {
        const basename = std.fs.path.basename(file_path);
        if (std.mem.endsWith(u8, basename, ".md")) {
            break :blk basename[0 .. basename.len - 3];
        }
        break :blk basename;
    };

    // Update the status field
    try updateYAMLScalarField(allocator, file_path, "status", new_status);

    var buf: [512]u8 = undefined;
    const msg = try std.fmt.bufPrint(&buf, "Updated {s} -> {s}\n", .{ target_id, new_status });
    try stdout_file.writeAll(msg);
    return 0;
}

fn handleStart(allocator: std.mem.Allocator, args: []const [:0]const u8) !u8 {
    if (args.len < 1) {
        try stderr_file.writeAll("Usage: ticket start <id>\n");
        return 1;
    }
    
    // Create a new args array with the status value added
    var new_args: [2][:0]const u8 = undefined;
    new_args[0] = args[0];
    new_args[1] = "in_progress";
    
    return try handleStatus(allocator, &new_args);
}

fn handleClose(allocator: std.mem.Allocator, args: []const [:0]const u8) !u8 {
    if (args.len < 1) {
        try stderr_file.writeAll("Usage: ticket close <id>\n");
        return 1;
    }
    
    // Create a new args array with the status value added
    var new_args: [2][:0]const u8 = undefined;
    new_args[0] = args[0];
    new_args[1] = "closed";
    
    return try handleStatus(allocator, &new_args);
}

fn handleReopen(allocator: std.mem.Allocator, args: []const [:0]const u8) !u8 {
    if (args.len < 1) {
        try stderr_file.writeAll("Usage: ticket reopen <id>\n");
        return 1;
    }
    
    // Create a new args array with the status value added
    var new_args: [2][:0]const u8 = undefined;
    new_args[0] = args[0];
    new_args[1] = "open";
    
    return try handleStatus(allocator, &new_args);
}

fn handleShow(allocator: std.mem.Allocator, args: []const [:0]const u8) !u8 {
    if (args.len < 1) {
        try stderr_file.writeAll("Usage: ticket show <id>\n");
        return 1;
    }

    const ticket_id = args[0];

    // Resolve ticket ID
    const file_path = resolveTicketID(allocator, ticket_id) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = if (err == error.NotFound)
            try std.fmt.bufPrint(&buf, "Error: ticket '{s}' not found\n", .{ticket_id})
        else if (err == error.Ambiguous)
            try std.fmt.bufPrint(&buf, "Error: ambiguous ID '{s}' matches multiple tickets\n", .{ticket_id})
        else
            try std.fmt.bufPrint(&buf, "Error resolving ticket ID\n", .{});
        try stderr_file.writeAll(msg);
        return 1;
    };
    defer allocator.free(file_path);

    // Extract the target ID from the file path
    const target_id = blk: {
        const basename = std.fs.path.basename(file_path);
        if (std.mem.endsWith(u8, basename, ".md")) {
            break :blk basename[0 .. basename.len - 3];
        }
        break :blk basename;
    };

    // Load all tickets to build relationships
    var all_tickets = try loadAllTickets(allocator);
    defer {
        var iter = all_tickets.iterator();
        while (iter.next()) |entry| {
            allocator.free(entry.key_ptr.*);
            var ticket = entry.value_ptr.*;
            ticket.deinit();
        }
        all_tickets.deinit();
    }

    const target_ticket = all_tickets.get(target_id) orelse {
        var buf: [512]u8 = undefined;
        const msg = try std.fmt.bufPrint(&buf, "Error: ticket '{s}' not found\n", .{ticket_id});
        try stderr_file.writeAll(msg);
        return 1;
    };

    // Build relationship lists
    var blockers: std.ArrayList(RelationshipItem) = .empty;
    defer blockers.deinit(allocator);
    var blocking: std.ArrayList(RelationshipItem) = .empty;
    defer blocking.deinit(allocator);
    var children: std.ArrayList(RelationshipItem) = .empty;
    defer children.deinit(allocator);
    var linked: std.ArrayList(RelationshipItem) = .empty;
    defer linked.deinit(allocator);

    // Scan all tickets for relationships
    var iter = all_tickets.iterator();
    while (iter.next()) |entry| {
        const tid = entry.key_ptr.*;
        const ticket = entry.value_ptr.*;

        // Check if this ticket depends on target (target is blocking it)
        for (ticket.deps) |dep| {
            if (std.mem.eql(u8, dep, target_id) and !std.mem.eql(u8, ticket.status, "closed")) {
                try blocking.append(allocator, .{ .id = tid, .status = ticket.status, .title = ticket.title });
                break;
            }
        }

        // Check if this ticket is a child of target
        if (ticket.parent.len > 0 and std.mem.eql(u8, ticket.parent, target_id)) {
            try children.append(allocator, .{ .id = tid, .status = ticket.status, .title = ticket.title });
        }
    }

    // Find blockers (unclosed dependencies)
    for (target_ticket.deps) |dep| {
        if (all_tickets.get(dep)) |dep_ticket| {
            if (!std.mem.eql(u8, dep_ticket.status, "closed")) {
                try blockers.append(allocator, .{ .id = dep, .status = dep_ticket.status, .title = dep_ticket.title });
            }
        }
    }

    // Find linked tickets
    for (target_ticket.links) |link| {
        if (all_tickets.get(link)) |link_ticket| {
            try linked.append(allocator, .{ .id = link, .status = link_ticket.status, .title = link_ticket.title });
        }
    }

    // Read and display the ticket file with enhancements
    const file = std.fs.cwd().openFile(file_path, .{}) catch {
        var buf: [512]u8 = undefined;
        const msg = try std.fmt.bufPrint(&buf, "Error: could not open ticket file\n", .{});
        try stderr_file.writeAll(msg);
        return 1;
    };
    defer file.close();

    const content = file.readToEndAlloc(allocator, 1024 * 1024) catch {
        try stderr_file.writeAll("Error: could not read ticket file\n");
        return 1;
    };
    defer allocator.free(content);

    // Parse and output with parent field enhancement
    var lines = std.mem.splitScalar(u8, content, '\n');
    var in_frontmatter = false;
    var line_count: usize = 0;

    while (lines.next()) |line| {
        line_count += 1;
        const trimmed = std.mem.trim(u8, line, " \t\r");

        if (std.mem.eql(u8, trimmed, "---")) {
            try stdout_file.writeAll(line);
            try stdout_file.writeAll("\n");
            in_frontmatter = !in_frontmatter;
            continue;
        }

        if (in_frontmatter and std.mem.startsWith(u8, trimmed, "parent:")) {
            if (target_ticket.parent.len > 0) {
                if (all_tickets.get(target_ticket.parent)) |parent_ticket| {
                    var buf: [1024]u8 = undefined;
                    const enhanced = try std.fmt.bufPrint(&buf, "{s}  # {s}\n", .{ line, parent_ticket.title });
                    try stdout_file.writeAll(enhanced);
                    continue;
                }
            }
        }

        try stdout_file.writeAll(line);
        try stdout_file.writeAll("\n");
    }

    // Add relationship sections
    if (blockers.items.len > 0) {
        try stdout_file.writeAll("\n## Blockers\n\n");
        for (blockers.items) |blocker| {
            var buf: [1024]u8 = undefined;
            const msg = try std.fmt.bufPrint(&buf, "- {s} [{s}] {s}\n", .{ blocker.id, blocker.status, blocker.title });
            try stdout_file.writeAll(msg);
        }
    }

    if (blocking.items.len > 0) {
        try stdout_file.writeAll("\n## Blocking\n\n");
        for (blocking.items) |block| {
            var buf: [1024]u8 = undefined;
            const msg = try std.fmt.bufPrint(&buf, "- {s} [{s}] {s}\n", .{ block.id, block.status, block.title });
            try stdout_file.writeAll(msg);
        }
    }

    if (children.items.len > 0) {
        try stdout_file.writeAll("\n## Children\n\n");
        for (children.items) |child| {
            var buf: [1024]u8 = undefined;
            const msg = try std.fmt.bufPrint(&buf, "- {s} [{s}] {s}\n", .{ child.id, child.status, child.title });
            try stdout_file.writeAll(msg);
        }
    }

    if (linked.items.len > 0) {
        try stdout_file.writeAll("\n## Linked\n\n");
        for (linked.items) |link_item| {
            var buf: [1024]u8 = undefined;
            const msg = try std.fmt.bufPrint(&buf, "- {s} [{s}] {s}\n", .{ link_item.id, link_item.status, link_item.title });
            try stdout_file.writeAll(msg);
        }
    }

    return 0;
}

fn handleList(allocator: std.mem.Allocator, args: []const [:0]const u8) !u8 {
    // Check if tickets directory exists
    var dir = std.fs.cwd().openDir(tickets_dir, .{}) catch {
        return 0;
    };
    dir.close();

    // Parse status filter
    var status_filter: ?[]const u8 = null;
    for (args) |arg| {
        if (std.mem.startsWith(u8, arg, "--status=")) {
            status_filter = arg[9..];
        }
    }

    // Load all tickets
    var tickets = try loadAllTickets(allocator);
    defer {
        var iter = tickets.iterator();
        while (iter.next()) |entry| {
            allocator.free(entry.key_ptr.*);
            var ticket = entry.value_ptr.*;
            ticket.deinit();
        }
        tickets.deinit();
    }

    // Sort ticket IDs
    var ticket_ids: std.ArrayList([]const u8) = .empty;
    defer ticket_ids.deinit(allocator);
    var iter = tickets.keyIterator();
    while (iter.next()) |ticket_id| {
        try ticket_ids.append(allocator, ticket_id.*);
    }
    std.mem.sort([]const u8, ticket_ids.items, {}, struct {
        fn lessThan(_: void, a: []const u8, b: []const u8) bool {
            return std.mem.order(u8, a, b) == .lt;
        }
    }.lessThan);

    // Print tickets
    for (ticket_ids.items) |ticket_id| {
        const ticket = tickets.get(ticket_id).?;

        // Apply status filter
        if (status_filter) |filter| {
            if (!std.mem.eql(u8, ticket.status, filter)) {
                continue;
            }
        }

        // Build dependency display
        var dep_buf: [512]u8 = undefined;
        var dep_display: []const u8 = "";
        if (ticket.deps.len > 0) {
            var fbs = std.io.fixedBufferStream(&dep_buf);
            var writer = fbs.writer();
            try writer.writeAll(" <- [");
            for (ticket.deps, 0..) |dep_id, i| {
                if (i > 0) try writer.writeAll(", ");
                try writer.writeAll(dep_id);
            }
            try writer.writeAll("]");
            dep_display = fbs.getWritten();
        }

        // Print ticket line
        var buf: [1024]u8 = undefined;
        const msg = try std.fmt.bufPrint(&buf, "{s:<8} [{s}] - {s}{s}\n", .{ ticket_id, ticket.status, ticket.title, dep_display });
        try stdout_file.writeAll(msg);
    }

    return 0;
}

fn handleReady(allocator: std.mem.Allocator, args: []const [:0]const u8) !u8 {
    _ = args;
    
    // Check if tickets directory exists
    var dir = std.fs.cwd().openDir(tickets_dir, .{}) catch {
        return 0;
    };
    dir.close();

    // Load all tickets
    var tickets = try loadAllTickets(allocator);
    defer {
        var iter = tickets.iterator();
        while (iter.next()) |entry| {
            allocator.free(entry.key_ptr.*);
            var ticket = entry.value_ptr.*;
            ticket.deinit();
        }
        tickets.deinit();
    }

    // Find ready tickets (open/in_progress with all deps closed)
    const ReadyTicket = struct {
        priority: i32,
        id: []const u8,
        status: []const u8,
        title: []const u8,
    };
    var ready_tickets: std.ArrayList(ReadyTicket) = .empty;
    defer ready_tickets.deinit(allocator);

    var iter = tickets.iterator();
    while (iter.next()) |entry| {
        const ticket = entry.value_ptr.*;
        
        // Only consider open or in_progress tickets
        if (!std.mem.eql(u8, ticket.status, "open") and !std.mem.eql(u8, ticket.status, "in_progress")) {
            continue;
        }

        // Check if all dependencies are closed
        var ready = true;
        for (ticket.deps) |dep_id| {
            if (tickets.get(dep_id)) |dep_ticket| {
                if (!std.mem.eql(u8, dep_ticket.status, "closed")) {
                    ready = false;
                    break;
                }
            } else {
                // If dependency doesn't exist, consider ticket as not ready
                ready = false;
                break;
            }
        }

        if (ready) {
            try ready_tickets.append(allocator, .{
                .priority = ticket.priority,
                .id = ticket.id,
                .status = ticket.status,
                .title = ticket.title,
            });
        }
    }

    // Sort by priority, then by ID
    std.mem.sort(ReadyTicket, ready_tickets.items, {}, struct {
        fn lessThan(_: void, a: ReadyTicket, b: ReadyTicket) bool {
            if (a.priority != b.priority) {
                return a.priority < b.priority;
            }
            return std.mem.order(u8, a.id, b.id) == .lt;
        }
    }.lessThan);

    // Print ready tickets
    for (ready_tickets.items) |ticket| {
        var buf: [1024]u8 = undefined;
        const msg = try std.fmt.bufPrint(&buf, "{s:<8} [P{d}][{s}] - {s}\n", .{ ticket.id, ticket.priority, ticket.status, ticket.title });
        try stdout_file.writeAll(msg);
    }

    return 0;
}

fn handleBlocked(allocator: std.mem.Allocator, args: []const [:0]const u8) !u8 {
    _ = args;
    
    // Check if tickets directory exists
    var dir = std.fs.cwd().openDir(tickets_dir, .{}) catch {
        return 0;
    };
    dir.close();

    // Load all tickets
    var tickets = try loadAllTickets(allocator);
    defer {
        var iter = tickets.iterator();
        while (iter.next()) |entry| {
            allocator.free(entry.key_ptr.*);
            var ticket = entry.value_ptr.*;
            ticket.deinit();
        }
        tickets.deinit();
    }

    // Find blocked tickets (open/in_progress with unclosed deps)
    const BlockedTicket = struct {
        priority: i32,
        id: []const u8,
        status: []const u8,
        title: []const u8,
        blockers: std.ArrayList([]const u8),
    };
    var blocked_tickets: std.ArrayList(BlockedTicket) = .empty;
    defer {
        for (blocked_tickets.items) |*ticket| {
            ticket.blockers.deinit(allocator);
        }
        blocked_tickets.deinit(allocator);
    }

    var iter = tickets.iterator();
    while (iter.next()) |entry| {
        const ticket = entry.value_ptr.*;
        
        // Only consider open or in_progress tickets
        if (!std.mem.eql(u8, ticket.status, "open") and !std.mem.eql(u8, ticket.status, "in_progress")) {
            continue;
        }

        // Skip tickets with no dependencies
        if (ticket.deps.len == 0) {
            continue;
        }

        // Find unclosed blockers
        var unclosed_blockers: std.ArrayList([]const u8) = .empty;
        for (ticket.deps) |dep_id| {
            if (tickets.get(dep_id)) |dep_ticket| {
                if (!std.mem.eql(u8, dep_ticket.status, "closed")) {
                    try unclosed_blockers.append(allocator, dep_id);
                }
            } else {
                // If dependency doesn't exist, consider it blocking
                try unclosed_blockers.append(allocator, dep_id);
            }
        }

        if (unclosed_blockers.items.len > 0) {
            try blocked_tickets.append(allocator, .{
                .priority = ticket.priority,
                .id = ticket.id,
                .status = ticket.status,
                .title = ticket.title,
                .blockers = unclosed_blockers,
            });
        } else {
            unclosed_blockers.deinit(allocator);
        }
    }

    // Sort by priority, then by ID
    std.mem.sort(BlockedTicket, blocked_tickets.items, {}, struct {
        fn lessThan(_: void, a: BlockedTicket, b: BlockedTicket) bool {
            if (a.priority != b.priority) {
                return a.priority < b.priority;
            }
            return std.mem.order(u8, a.id, b.id) == .lt;
        }
    }.lessThan);

    // Print blocked tickets
    for (blocked_tickets.items) |ticket| {
        // Build blockers string
        var blockers_buf: [512]u8 = undefined;
        var fbs = std.io.fixedBufferStream(&blockers_buf);
        var writer = fbs.writer();
        for (ticket.blockers.items, 0..) |blocker_id, i| {
            if (i > 0) try writer.writeAll(", ");
            try writer.writeAll(blocker_id);
        }
        const blockers_str = fbs.getWritten();

        var buf: [1024]u8 = undefined;
        const msg = try std.fmt.bufPrint(&buf, "{s:<8} [P{d}][{s}] - {s} <- [{s}]\n", .{ ticket.id, ticket.priority, ticket.status, ticket.title, blockers_str });
        try stdout_file.writeAll(msg);
    }

    return 0;
}

fn handleClosedList(allocator: std.mem.Allocator, args: []const [:0]const u8) !u8 {
    // Check if tickets directory exists
    var dir = std.fs.cwd().openDir(tickets_dir, .{}) catch {
        return 0;
    };
    defer dir.close();

    // Parse limit
    var limit: usize = 20;
    for (args) |arg| {
        if (std.mem.startsWith(u8, arg, "--limit=")) {
            limit = try std.fmt.parseInt(usize, arg[8..], 10);
        }
    }

    // Get all ticket files with their modification times
    const FileInfo = struct {
        name: []const u8,
        mtime: i128,
    };
    var files: std.ArrayList(FileInfo) = .empty;
    defer {
        for (files.items) |file| {
            allocator.free(file.name);
        }
        files.deinit(allocator);
    }

    var iter = dir.iterate();
    while (try iter.next()) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.endsWith(u8, entry.name, ".md")) continue;

        const stat = try dir.statFile(entry.name);
        try files.append(allocator, .{
            .name = try allocator.dupe(u8, entry.name),
            .mtime = stat.mtime,
        });
    }

    // Sort by modification time (most recent first)
    std.mem.sort(FileInfo, files.items, {}, struct {
        fn lessThan(_: void, a: FileInfo, b: FileInfo) bool {
            return a.mtime > b.mtime;
        }
    }.lessThan);

    // Check up to 100 most recently modified files
    const check_limit = @min(files.items.len, 100);
    var closed_count: usize = 0;

    for (files.items[0..check_limit]) |file| {
        if (closed_count >= limit) break;

        const file_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ tickets_dir, file.name });
        defer allocator.free(file_path);

        const ticket = parseTicket(allocator, file_path) catch continue;
        defer {
            var t = ticket;
            t.deinit();
        }

        if (std.mem.eql(u8, ticket.status, "closed") or std.mem.eql(u8, ticket.status, "done")) {
            var buf: [1024]u8 = undefined;
            const msg = try std.fmt.bufPrint(&buf, "{s:<8} [{s}] - {s}\n", .{ ticket.id, ticket.status, ticket.title });
            try stdout_file.writeAll(msg);
            closed_count += 1;
        }
    }

    return 0;
}

fn handleDep(allocator: std.mem.Allocator, args: []const [:0]const u8) !u8 {
    // Handle subcommands
    if (args.len > 0 and std.mem.eql(u8, args[0], "tree")) {
        return handleDepTree(allocator, args[1..]);
    }

    if (args.len < 2) {
        try stderr_file.writeAll("Usage: ticket dep <id> <dependency-id>\n");
        try stderr_file.writeAll("       ticket dep tree [--full] <id>  - show dependency tree\n");
        return 1;
    }

    const ticket_id = args[0];
    const dep_id = args[1];

    // Resolve both ticket IDs
    const file_path = resolveTicketID(allocator, ticket_id) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = if (err == error.NotFound)
            try std.fmt.bufPrint(&buf, "Error: ticket '{s}' not found\n", .{ticket_id})
        else if (err == error.Ambiguous)
            try std.fmt.bufPrint(&buf, "Error: ambiguous ID '{s}' matches multiple tickets\n", .{ticket_id})
        else
            try std.fmt.bufPrint(&buf, "Error resolving ticket ID\n", .{});
        try stderr_file.writeAll(msg);
        return 1;
    };
    defer allocator.free(file_path);

    const dep_path = resolveTicketID(allocator, dep_id) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = if (err == error.NotFound)
            try std.fmt.bufPrint(&buf, "Error: ticket '{s}' not found\n", .{dep_id})
        else if (err == error.Ambiguous)
            try std.fmt.bufPrint(&buf, "Error: ambiguous ID '{s}' matches multiple tickets\n", .{dep_id})
        else
            try std.fmt.bufPrint(&buf, "Error resolving ticket ID\n", .{});
        try stderr_file.writeAll(msg);
        return 1;
    };
    defer allocator.free(dep_path);

    // Get actual IDs from file stems
    const actual_id = try getTicketIDFromPath(allocator, file_path);
    defer allocator.free(actual_id);
    const actual_dep_id = try getTicketIDFromPath(allocator, dep_path);
    defer allocator.free(actual_dep_id);

    // Read current ticket
    const content = std.fs.cwd().readFileAlloc(allocator, file_path, 1024 * 1024) catch {
        try stderr_file.writeAll("Error reading ticket\n");
        return 1;
    };
    defer allocator.free(content);

    // Parse deps field
    const existing_deps = try parseListField(allocator, content, "deps");
    defer {
        for (existing_deps) |dep| {
            allocator.free(dep);
        }
        allocator.free(existing_deps);
    }

    // Check if dependency already exists
    for (existing_deps) |dep| {
        if (std.mem.eql(u8, dep, actual_dep_id)) {
            try stdout_file.writeAll("Dependency already exists\n");
            return 0;
        }
    }

    // Add new dependency
    const new_deps = try allocator.alloc([]const u8, existing_deps.len + 1);
    defer allocator.free(new_deps);
    for (existing_deps, 0..) |dep, i| {
        new_deps[i] = dep;
    }
    new_deps[existing_deps.len] = actual_dep_id;

    // Update the file
    try updateYAMLField(allocator, file_path, "deps", new_deps);

    var buf: [512]u8 = undefined;
    const msg = try std.fmt.bufPrint(&buf, "Added dependency: {s} -> {s}\n", .{ actual_id, actual_dep_id });
    try stdout_file.writeAll(msg);
    return 0;
}

fn handleUndep(allocator: std.mem.Allocator, args: []const [:0]const u8) !u8 {
    if (args.len < 2) {
        try stderr_file.writeAll("Usage: ticket undep <id> <dependency-id>\n");
        return 1;
    }

    const ticket_id = args[0];
    const dep_id = args[1];

    // Resolve both ticket IDs
    const file_path = resolveTicketID(allocator, ticket_id) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = if (err == error.NotFound)
            try std.fmt.bufPrint(&buf, "Error: ticket '{s}' not found\n", .{ticket_id})
        else if (err == error.Ambiguous)
            try std.fmt.bufPrint(&buf, "Error: ambiguous ID '{s}' matches multiple tickets\n", .{ticket_id})
        else
            try std.fmt.bufPrint(&buf, "Error resolving ticket ID\n", .{});
        try stderr_file.writeAll(msg);
        return 1;
    };
    defer allocator.free(file_path);

    const dep_path = resolveTicketID(allocator, dep_id) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = if (err == error.NotFound)
            try std.fmt.bufPrint(&buf, "Error: ticket '{s}' not found\n", .{dep_id})
        else if (err == error.Ambiguous)
            try std.fmt.bufPrint(&buf, "Error: ambiguous ID '{s}' matches multiple tickets\n", .{dep_id})
        else
            try std.fmt.bufPrint(&buf, "Error resolving ticket ID\n", .{});
        try stderr_file.writeAll(msg);
        return 1;
    };
    defer allocator.free(dep_path);

    // Get actual IDs from file stems
    const actual_id = try getTicketIDFromPath(allocator, file_path);
    defer allocator.free(actual_id);
    const actual_dep_id = try getTicketIDFromPath(allocator, dep_path);
    defer allocator.free(actual_dep_id);

    // Read current ticket
    const content = std.fs.cwd().readFileAlloc(allocator, file_path, 1024 * 1024) catch {
        try stderr_file.writeAll("Error reading ticket\n");
        return 1;
    };
    defer allocator.free(content);

    // Parse deps field
    const existing_deps = try parseListField(allocator, content, "deps");
    defer {
        for (existing_deps) |dep| {
            allocator.free(dep);
        }
        allocator.free(existing_deps);
    }

    // Check if dependency exists and build new list without it
    var found = false;
    var new_deps: std.ArrayList([]const u8) = .empty;
    defer new_deps.deinit(allocator);

    for (existing_deps) |dep| {
        if (std.mem.eql(u8, dep, actual_dep_id)) {
            found = true;
        } else {
            try new_deps.append(allocator, dep);
        }
    }

    if (!found) {
        try stdout_file.writeAll("Dependency not found\n");
        return 1;
    }

    // Update the file
    try updateYAMLField(allocator, file_path, "deps", new_deps.items);

    var buf: [512]u8 = undefined;
    const msg = try std.fmt.bufPrint(&buf, "Removed dependency: {s} -/-> {s}\n", .{ actual_id, actual_dep_id });
    try stdout_file.writeAll(msg);
    return 0;
}

fn handleLink(allocator: std.mem.Allocator, args: []const [:0]const u8) !u8 {
    if (args.len < 2) {
        try stderr_file.writeAll("Usage: ticket link <id> <id> [id...]\n");
        return 1;
    }

    // Resolve all ticket IDs
    var resolved_paths: std.ArrayList([]const u8) = .empty;
    defer {
        for (resolved_paths.items) |path| {
            allocator.free(path);
        }
        resolved_paths.deinit(allocator);
    }

    var resolved_ids: std.ArrayList([]const u8) = .empty;
    defer {
        for (resolved_ids.items) |id| {
            allocator.free(id);
        }
        resolved_ids.deinit(allocator);
    }

    for (args) |ticket_id| {
        const file_path = resolveTicketID(allocator, ticket_id) catch |err| {
            var buf: [512]u8 = undefined;
            const msg = if (err == error.NotFound)
                try std.fmt.bufPrint(&buf, "Error: ticket '{s}' not found\n", .{ticket_id})
            else if (err == error.Ambiguous)
                try std.fmt.bufPrint(&buf, "Error: ambiguous ID '{s}' matches multiple tickets\n", .{ticket_id})
            else
                try std.fmt.bufPrint(&buf, "Error resolving ticket ID\n", .{});
            try stderr_file.writeAll(msg);
            return 1;
        };
        try resolved_paths.append(allocator, file_path);

        const actual_id = try getTicketIDFromPath(allocator, file_path);
        try resolved_ids.append(allocator, actual_id);
    }

    // For each ticket, add all other tickets to its links
    var links_added: usize = 0;
    for (resolved_paths.items, 0..) |file_path, i| {
        // Read current ticket
        const content = std.fs.cwd().readFileAlloc(allocator, file_path, 1024 * 1024) catch {
            try stderr_file.writeAll("Error reading ticket\n");
            return 1;
        };
        defer allocator.free(content);

        // Parse links field
        const existing_links = try parseListField(allocator, content, "links");
        defer {
            for (existing_links) |link| {
                allocator.free(link);
            }
            allocator.free(existing_links);
        }

        // Build new links list (existing + missing ones)
        var new_links_list: std.ArrayList([]const u8) = .empty;
        defer {
            for (new_links_list.items) |link| {
                allocator.free(link);
            }
            new_links_list.deinit(allocator);
        }

        // Add existing links
        for (existing_links) |link| {
            try new_links_list.append(allocator, try allocator.dupe(u8, link));
        }

        // Add new links (all other tickets in the list)
        for (resolved_ids.items, 0..) |other_id, j| {
            if (i == j) continue; // Skip self

            // Check if already linked
            var already_linked = false;
            for (existing_links) |link| {
                if (std.mem.eql(u8, link, other_id)) {
                    already_linked = true;
                    break;
                }
            }

            if (!already_linked) {
                try new_links_list.append(allocator, try allocator.dupe(u8, other_id));
                links_added += 1;
            }
        }

        // Update the file if there are new links
        if (new_links_list.items.len > existing_links.len) {
            try updateYAMLField(allocator, file_path, "links", new_links_list.items);
        }
    }

    if (links_added == 0) {
        try stdout_file.writeAll("All links already exist\n");
    } else {
        var buf: [512]u8 = undefined;
        const msg = try std.fmt.bufPrint(&buf, "Added {d} link(s) between {d} tickets\n", .{ links_added, resolved_ids.items.len });
        try stdout_file.writeAll(msg);
    }

    return 0;
}

fn handleUnlink(allocator: std.mem.Allocator, args: []const [:0]const u8) !u8 {
    if (args.len != 2) {
        try stderr_file.writeAll("Usage: ticket unlink <id> <target-id>\n");
        return 1;
    }

    const ticket_id = args[0];
    const target_id = args[1];

    // Resolve both ticket IDs
    const file_path = resolveTicketID(allocator, ticket_id) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = if (err == error.NotFound)
            try std.fmt.bufPrint(&buf, "Error: ticket '{s}' not found\n", .{ticket_id})
        else if (err == error.Ambiguous)
            try std.fmt.bufPrint(&buf, "Error: ambiguous ID '{s}' matches multiple tickets\n", .{ticket_id})
        else
            try std.fmt.bufPrint(&buf, "Error resolving ticket ID\n", .{});
        try stderr_file.writeAll(msg);
        return 1;
    };
    defer allocator.free(file_path);

    const target_path = resolveTicketID(allocator, target_id) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = if (err == error.NotFound)
            try std.fmt.bufPrint(&buf, "Error: ticket '{s}' not found\n", .{target_id})
        else if (err == error.Ambiguous)
            try std.fmt.bufPrint(&buf, "Error: ambiguous ID '{s}' matches multiple tickets\n", .{target_id})
        else
            try std.fmt.bufPrint(&buf, "Error resolving ticket ID\n", .{});
        try stderr_file.writeAll(msg);
        return 1;
    };
    defer allocator.free(target_path);

    // Get actual IDs from file paths
    const actual_id = try getTicketIDFromPath(allocator, file_path);
    defer allocator.free(actual_id);
    
    const actual_target_id = try getTicketIDFromPath(allocator, target_path);
    defer allocator.free(actual_target_id);

    // Check if link exists in first ticket
    const content = try std.fs.cwd().readFileAlloc(allocator, file_path, 1024 * 1024);
    defer allocator.free(content);

    const existing_links = try parseListField(allocator, content, "links");
    defer {
        for (existing_links) |link| {
            allocator.free(link);
        }
        allocator.free(existing_links);
    }

    // Check if target is in the links
    var link_exists = false;
    for (existing_links) |link| {
        if (std.mem.eql(u8, link, actual_target_id)) {
            link_exists = true;
            break;
        }
    }

    if (!link_exists) {
        try stdout_file.writeAll("Link not found\n");
        return 1;
    }

    // Remove link from first ticket
    var new_links: std.ArrayList([]const u8) = .empty;
    defer {
        for (new_links.items) |link| {
            allocator.free(link);
        }
        new_links.deinit(allocator);
    }

    for (existing_links) |link| {
        if (!std.mem.eql(u8, link, actual_target_id)) {
            try new_links.append(allocator, try allocator.dupe(u8, link));
        }
    }

    try updateYAMLField(allocator, file_path, "links", new_links.items);

    // Remove link from second ticket
    const target_content = try std.fs.cwd().readFileAlloc(allocator, target_path, 1024 * 1024);
    defer allocator.free(target_content);

    const target_links = try parseListField(allocator, target_content, "links");
    defer {
        for (target_links) |link| {
            allocator.free(link);
        }
        allocator.free(target_links);
    }

    var new_target_links: std.ArrayList([]const u8) = .empty;
    defer {
        for (new_target_links.items) |link| {
            allocator.free(link);
        }
        new_target_links.deinit(allocator);
    }

    for (target_links) |link| {
        if (!std.mem.eql(u8, link, actual_id)) {
            try new_target_links.append(allocator, try allocator.dupe(u8, link));
        }
    }

    try updateYAMLField(allocator, target_path, "links", new_target_links.items);

    // Print success message
    var buf: [512]u8 = undefined;
    const msg = try std.fmt.bufPrint(&buf, "Removed link: {s} <-> {s}\n", .{ actual_id, actual_target_id });
    try stdout_file.writeAll(msg);

    return 0;
}

fn handleEdit(allocator: std.mem.Allocator, args: []const [:0]const u8) !u8 {
    if (args.len < 1) {
        try stderr_file.writeAll("Usage: ticket edit <id>\n");
        return 1;
    }

    const ticket_id = args[0];

    // Resolve ticket ID
    const file_path = resolveTicketID(allocator, ticket_id) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = if (err == error.NotFound)
            try std.fmt.bufPrint(&buf, "Error: ticket '{s}' not found\n", .{ticket_id})
        else if (err == error.Ambiguous)
            try std.fmt.bufPrint(&buf, "Error: ambiguous ID '{s}' matches multiple tickets\n", .{ticket_id})
        else
            try std.fmt.bufPrint(&buf, "Error resolving ticket ID\n", .{});
        try stderr_file.writeAll(msg);
        return 1;
    };
    defer allocator.free(file_path);

    // Check if stdin and stdout are both TTY
    const stdin_is_tty = std.posix.isatty(std.posix.STDIN_FILENO);
    const stdout_is_tty = std.posix.isatty(std.posix.STDOUT_FILENO);
    const is_tty = stdin_is_tty and stdout_is_tty;

    if (is_tty) {
        // Open in editor
        const editor = std.posix.getenv("EDITOR") orelse "vi";
        
        var child = std.process.Child.init(&[_][]const u8{ editor, file_path }, allocator);
        child.stdin_behavior = .Inherit;
        child.stdout_behavior = .Inherit;
        child.stderr_behavior = .Inherit;
        
        _ = try child.spawnAndWait();
    } else {
        // Non-TTY mode: just print the file path
        var buf: [512]u8 = undefined;
        const msg = try std.fmt.bufPrint(&buf, "Edit ticket file: {s}\n", .{file_path});
        try stdout_file.writeAll(msg);
    }

    return 0;
}

fn handleAddNote(allocator: std.mem.Allocator, args: []const [:0]const u8) !u8 {
    if (args.len < 1) {
        try stderr_file.writeAll("Usage: ticket add-note <id> [note text]\n");
        return 1;
    }

    const ticket_id = args[0];
    
    // Get note text from remaining args
    const note_text = if (args.len > 1) blk: {
        var result: std.ArrayList(u8) = .empty;
        defer result.deinit(allocator);
        
        for (args[1..], 0..) |arg, i| {
            if (i > 0) try result.append(allocator, ' ');
            try result.appendSlice(allocator, arg);
        }
        break :blk try result.toOwnedSlice(allocator);
    } else "";
    defer if (args.len > 1) allocator.free(note_text);

    // Resolve ticket ID
    const file_path = resolveTicketID(allocator, ticket_id) catch |err| {
        var buf: [512]u8 = undefined;
        const msg = if (err == error.NotFound)
            try std.fmt.bufPrint(&buf, "Error: ticket '{s}' not found\n", .{ticket_id})
        else if (err == error.Ambiguous)
            try std.fmt.bufPrint(&buf, "Error: ambiguous ID '{s}' matches multiple tickets\n", .{ticket_id})
        else
            try std.fmt.bufPrint(&buf, "Error resolving ticket ID\n", .{});
        try stderr_file.writeAll(msg);
        return 1;
    };
    defer allocator.free(file_path);

    // Extract the target ID from the file path
    const target_id = blk: {
        const basename = std.fs.path.basename(file_path);
        if (std.mem.endsWith(u8, basename, ".md")) {
            break :blk basename[0 .. basename.len - 3];
        }
        break :blk basename;
    };

    // Read existing content
    const content = try std.fs.cwd().readFileAlloc(allocator, file_path, 1024 * 1024);
    defer allocator.free(content);

    // Get current timestamp
    const timestamp = try getCurrentTimestamp(allocator);
    defer allocator.free(timestamp);

    // Build new content
    var result: std.ArrayList(u8) = .empty;
    defer result.deinit(allocator);

    // Check if Notes section exists
    const has_notes = std.mem.indexOf(u8, content, "## Notes") != null;
    
    if (has_notes) {
        // Append to existing content
        try result.appendSlice(allocator, content);
    } else {
        // Add Notes section
        try result.appendSlice(allocator, content);
        if (!std.mem.endsWith(u8, content, "\n")) {
            try result.append(allocator, '\n');
        }
        try result.appendSlice(allocator, "\n## Notes\n");
    }

    // Append timestamped note
    try result.append(allocator, '\n');
    try result.appendSlice(allocator, "**");
    try result.appendSlice(allocator, timestamp);
    try result.appendSlice(allocator, "**\n\n");
    try result.appendSlice(allocator, note_text);
    try result.append(allocator, '\n');

    // Write back to file
    try std.fs.cwd().writeFile(.{ .sub_path = file_path, .data = result.items });

    var buf: [512]u8 = undefined;
    const msg = try std.fmt.bufPrint(&buf, "Note added to {s}\n", .{target_id});
    try stdout_file.writeAll(msg);
    return 0;
}

fn handleQuery(allocator: std.mem.Allocator, args: []const [:0]const u8) !u8 {
    _ = allocator;
    _ = args;
    const stdout = stdout_file;
    try stdout.writeAll("Error: query command not yet implemented\n");
    return 1;
}

fn handleMigrateBeads(allocator: std.mem.Allocator, args: []const [:0]const u8) !u8 {
    _ = allocator;
    _ = args;
    const stdout = stdout_file;
    try stdout.writeAll("Error: migrate-beads command not yet implemented\n");
    return 1;
}

// Helper function to get current timestamp in ISO 8601 format
fn getCurrentTimestamp(allocator: std.mem.Allocator) ![]u8 {
    const timestamp = std.time.timestamp();
    const epoch_seconds = @as(u64, @intCast(timestamp));
    
    // Calculate date and time components
    const SECONDS_PER_DAY = 86400;
    const SECONDS_PER_HOUR = 3600;
    const SECONDS_PER_MINUTE = 60;
    
    // Days since Unix epoch (1970-01-01)
    const days_since_epoch = epoch_seconds / SECONDS_PER_DAY;
    const seconds_today = epoch_seconds % SECONDS_PER_DAY;
    
    // Calculate hours, minutes, seconds
    const hours = seconds_today / SECONDS_PER_HOUR;
    const minutes = (seconds_today % SECONDS_PER_HOUR) / SECONDS_PER_MINUTE;
    const seconds = seconds_today % SECONDS_PER_MINUTE;
    
    // Calculate year, month, day from days_since_epoch
    // This is a simplified calculation
    var year: u32 = 1970;
    var remaining_days = days_since_epoch;
    
    // Approximate year
    const days_per_year: u64 = 365;
    const days_per_leap_cycle: u64 = 1461; // 4 years
    
    // Fast forward by 400-year cycles (146097 days)
    const cycles_400 = remaining_days / 146097;
    year += @as(u32, @intCast(cycles_400 * 400));
    remaining_days %= 146097;
    
    // Fast forward by 100-year cycles (36524 days, but not 36525)
    var cycles_100 = remaining_days / 36524;
    if (cycles_100 > 3) cycles_100 = 3;
    year += @as(u32, @intCast(cycles_100 * 100));
    remaining_days -= cycles_100 * 36524;
    
    // Fast forward by 4-year cycles (1461 days)
    const cycles_4 = remaining_days / days_per_leap_cycle;
    year += @as(u32, @intCast(cycles_4 * 4));
    remaining_days %= days_per_leap_cycle;
    
    // Remaining years
    var cycles_1 = remaining_days / days_per_year;
    if (cycles_1 > 3) cycles_1 = 3;
    year += @as(u32, @intCast(cycles_1));
    remaining_days -= cycles_1 * days_per_year;
    
    // Determine if current year is a leap year
    const is_leap = (year % 4 == 0 and year % 100 != 0) or (year % 400 == 0);
    
    // Days in each month
    const days_in_month = [12]u8{
        31, if (is_leap) 29 else 28, 31, 30, 31, 30,
        31, 31, 30, 31, 30, 31
    };
    
    // Calculate month and day
    var month: u8 = 1;
    var day_of_month = remaining_days + 1;
    
    for (days_in_month) |days| {
        if (day_of_month <= days) break;
        day_of_month -= days;
        month += 1;
    }
    
    // Format as ISO 8601: YYYY-MM-DDTHH:MM:SSZ
    return try std.fmt.allocPrint(
        allocator,
        "{d:0>4}-{d:0>2}-{d:0>2}T{d:0>2}:{d:0>2}:{d:0>2}Z",
        .{ year, month, day_of_month, hours, minutes, seconds }
    );
}

fn generateTicketID(allocator: std.mem.Allocator) ![]u8 {
    const cwd = try std.fs.cwd().realpathAlloc(allocator, ".");
    defer allocator.free(cwd);
    
    const dir_name = std.fs.path.basename(cwd);
    
    // Extract first letter of each hyphenated/underscored segment
    var prefix: std.ArrayList(u8) = .empty;
    defer prefix.deinit(allocator);
    
    var iter = std.mem.tokenizeAny(u8, dir_name, "-_");
    while (iter.next()) |segment| {
        if (segment.len > 0) {
            try prefix.append(allocator, segment[0]);
        }
    }
    
    // Fallback to first 3 chars if no segments
    const prefix_str = if (prefix.items.len > 0)
        try allocator.dupe(u8, prefix.items)
    else if (dir_name.len >= 3)
        try allocator.dupe(u8, dir_name[0..3])
    else
        try allocator.dupe(u8, dir_name);
    defer allocator.free(prefix_str);
    
    // Generate 4-char hash from timestamp + PID
    const timestamp = std.time.timestamp();
    const pid = std.c.getpid();
    
    var entropy_buf: [64]u8 = undefined;
    const entropy = try std.fmt.bufPrint(&entropy_buf, "{d}{d}", .{ pid, timestamp });
    
    var hash: [32]u8 = undefined;
    std.crypto.hash.sha2.Sha256.hash(entropy, &hash, .{});
    
    var hash_str: [4]u8 = undefined;
    _ = try std.fmt.bufPrint(&hash_str, "{x:0>4}", .{@as(u16, @intCast(hash[0])) << 8 | @as(u16, @intCast(hash[1]))});
    
    return try std.fmt.allocPrint(allocator, "{s}-{s}", .{ prefix_str, &hash_str });
}

fn ensureTicketsDir() !void {
    std.fs.cwd().makeDir(tickets_dir) catch |err| {
        if (err != error.PathAlreadyExists) {
            return err;
        }
    };
}

fn getGitUserName(allocator: std.mem.Allocator) !?[]u8 {
    const result = std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{ "git", "config", "user.name" },
    }) catch {
        return null;
    };
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);
    
    if (result.term.Exited != 0) {
        return null;
    }
    
    const trimmed = std.mem.trim(u8, result.stdout, " \t\n\r");
    if (trimmed.len == 0) {
        return null;
    }
    
    return try allocator.dupe(u8, trimmed);
}

// Helper function to resolve ticket ID (exact or partial match)
fn resolveTicketID(allocator: std.mem.Allocator, ticket_id: []const u8) ![]const u8 {
    // Try exact match first
    const exact_path = try std.fmt.allocPrint(allocator, "{s}/{s}.md", .{ tickets_dir, ticket_id });
    defer allocator.free(exact_path);

    if (std.fs.cwd().access(exact_path, .{})) {
        return try allocator.dupe(u8, exact_path);
    } else |err| {
        if (err != error.FileNotFound) {
            return err;
        }
    }

    // Try partial match
    var dir = std.fs.cwd().openDir(tickets_dir, .{ .iterate = true }) catch {
        return error.NotFound;
    };
    defer dir.close();

    var iter = dir.iterate();
    var matches: std.ArrayList([]const u8) = .empty;
    defer {
        for (matches.items) |match| {
            allocator.free(match);
        }
        matches.deinit(allocator);
    }

    while (try iter.next()) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.endsWith(u8, entry.name, ".md")) continue;

        if (std.mem.indexOf(u8, entry.name, ticket_id) != null) {
            const match = try allocator.dupe(u8, entry.name);
            try matches.append(allocator, match);
        }
    }

    if (matches.items.len == 0) {
        return error.NotFound;
    } else if (matches.items.len > 1) {
        return error.Ambiguous;
    }

    return try std.fmt.allocPrint(allocator, "{s}/{s}", .{ tickets_dir, matches.items[0] });
}

// Helper function to extract ticket ID from file path
fn getTicketIDFromPath(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    const basename = std.fs.path.basename(path);
    if (std.mem.endsWith(u8, basename, ".md")) {
        return try allocator.dupe(u8, basename[0 .. basename.len - 3]);
    }
    return try allocator.dupe(u8, basename);
}

// Helper function to parse a list field from ticket content
fn parseListField(allocator: std.mem.Allocator, content: []const u8, field: []const u8) ![][]const u8 {
    var result: std.ArrayList([]const u8) = .empty;
    errdefer {
        for (result.items) |item| {
            allocator.free(item);
        }
        result.deinit(allocator);
    }

    var lines = std.mem.splitScalar(u8, content, '\n');
    var in_frontmatter = false;
    var frontmatter_count: u8 = 0;

    while (lines.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " \t\r");

        if (std.mem.eql(u8, trimmed, "---")) {
            frontmatter_count += 1;
            if (frontmatter_count == 1) {
                in_frontmatter = true;
            } else if (frontmatter_count == 2) {
                break;
            }
            continue;
        }

        if (!in_frontmatter) continue;

        if (std.mem.startsWith(u8, trimmed, field)) {
            const colon_idx = std.mem.indexOfScalar(u8, trimmed, ':') orelse continue;
            const value_start = colon_idx + 1;
            if (value_start >= trimmed.len) break;

            const value = std.mem.trim(u8, trimmed[value_start..], " \t");

            // Parse array syntax: [item1, item2, ...]
            if (std.mem.startsWith(u8, value, "[") and std.mem.endsWith(u8, value, "]")) {
                const inner = value[1 .. value.len - 1];
                const inner_trimmed = std.mem.trim(u8, inner, " \t");

                if (inner_trimmed.len == 0) {
                    break;
                }

                var items = std.mem.splitScalar(u8, inner_trimmed, ',');
                while (items.next()) |item| {
                    const item_trimmed = std.mem.trim(u8, item, " \t");
                    if (item_trimmed.len > 0) {
                        try result.append(allocator, try allocator.dupe(u8, item_trimmed));
                    }
                }
            }
            break;
        }
    }

    return try result.toOwnedSlice(allocator);
}

// Helper function to update a YAML field in a ticket file
fn updateYAMLScalarField(allocator: std.mem.Allocator, file_path: []const u8, field: []const u8, value: []const u8) !void {
    const content = try std.fs.cwd().readFileAlloc(allocator, file_path, 1024 * 1024);
    defer allocator.free(content);

    var result: std.ArrayList(u8) = .empty;
    defer result.deinit(allocator);

    var lines = std.mem.splitScalar(u8, content, '\n');
    var in_frontmatter = false;
    var frontmatter_count: u8 = 0;
    var updated = false;

    while (lines.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " \t\r");

        if (std.mem.eql(u8, trimmed, "---")) {
            try result.appendSlice(allocator, line);
            try result.append(allocator, '\n');
            frontmatter_count += 1;
            if (frontmatter_count == 1) {
                in_frontmatter = true;
            } else if (frontmatter_count == 2) {
                in_frontmatter = false;
            }
            continue;
        }

        if (in_frontmatter and std.mem.startsWith(u8, trimmed, field) and std.mem.indexOfScalar(u8, trimmed, ':') != null) {
            // Replace this field
            try result.appendSlice(allocator, field);
            try result.appendSlice(allocator, ": ");
            try result.appendSlice(allocator, value);
            try result.append(allocator, '\n');
            updated = true;
        } else {
            try result.appendSlice(allocator, line);
            try result.append(allocator, '\n');
        }
    }

    // If field wasn't found, we'd need to add it (not implemented for simplicity)
    if (!updated) {
        return error.FieldNotFound;
    }

    // Write back to file
    try std.fs.cwd().writeFile(.{ .sub_path = file_path, .data = result.items });
}

fn updateYAMLField(allocator: std.mem.Allocator, file_path: []const u8, field: []const u8, values: []const []const u8) !void {
    const content = try std.fs.cwd().readFileAlloc(allocator, file_path, 1024 * 1024);
    defer allocator.free(content);

    var result: std.ArrayList(u8) = .empty;
    defer result.deinit(allocator);

    var lines = std.mem.splitScalar(u8, content, '\n');
    var in_frontmatter = false;
    var frontmatter_count: u8 = 0;
    var updated = false;

    while (lines.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " \t\r");

        if (std.mem.eql(u8, trimmed, "---")) {
            try result.appendSlice(allocator, line);
            try result.append(allocator, '\n');
            frontmatter_count += 1;
            if (frontmatter_count == 1) {
                in_frontmatter = true;
            } else if (frontmatter_count == 2) {
                in_frontmatter = false;
            }
            continue;
        }

        if (in_frontmatter and std.mem.startsWith(u8, trimmed, field) and std.mem.indexOfScalar(u8, trimmed, ':') != null) {
            // Replace this field
            try result.appendSlice(allocator, field);
            try result.appendSlice(allocator, ": [");
            for (values, 0..) |value, i| {
                if (i > 0) {
                    try result.appendSlice(allocator, ", ");
                }
                try result.appendSlice(allocator, value);
            }
            try result.appendSlice(allocator, "]\n");
            updated = true;
        } else {
            try result.appendSlice(allocator, line);
            try result.append(allocator, '\n');
        }
    }

    // If field wasn't found, we'd need to add it (not implemented for simplicity)
    if (!updated) {
        return error.FieldNotFound;
    }

    // Write back to file
    try std.fs.cwd().writeFile(.{ .sub_path = file_path, .data = result.items });
}

// Helper struct for ticket data
const TicketData = struct {
    id: []const u8,
    status: []const u8,
    title: []const u8,
    deps: [][]const u8,
    links: [][]const u8,
    parent: []const u8,
    priority: i32,
    allocator: std.mem.Allocator,

    fn deinit(self: *TicketData) void {
        self.allocator.free(self.id);
        self.allocator.free(self.status);
        self.allocator.free(self.title);
        for (self.deps) |dep| {
            self.allocator.free(dep);
        }
        self.allocator.free(self.deps);
        for (self.links) |link| {
            self.allocator.free(link);
        }
        self.allocator.free(self.links);
        self.allocator.free(self.parent);
    }
};

// Helper function to parse a ticket file
fn parseTicket(allocator: std.mem.Allocator, file_path: []const u8) !TicketData {
    const content = try std.fs.cwd().readFileAlloc(allocator, file_path, 1024 * 1024);
    defer allocator.free(content);

    var id: []const u8 = "";
    var status: []const u8 = "open";
    var title: []const u8 = "";
    var parent: []const u8 = "";
    var priority: i32 = 2;
    var deps: std.ArrayList([]const u8) = .empty;
    var links: std.ArrayList([]const u8) = .empty;

    var lines = std.mem.splitScalar(u8, content, '\n');
    var in_frontmatter = false;
    var frontmatter_count: u8 = 0;
    var after_frontmatter = false;

    while (lines.next()) |line| {
        const trimmed = std.mem.trim(u8, line, " \t\r");

        if (std.mem.eql(u8, trimmed, "---")) {
            frontmatter_count += 1;
            if (frontmatter_count == 1) {
                in_frontmatter = true;
            } else if (frontmatter_count == 2) {
                in_frontmatter = false;
                after_frontmatter = true;
            }
            continue;
        }

        if (in_frontmatter) {
            if (std.mem.startsWith(u8, trimmed, "id:")) {
                const colon_idx = std.mem.indexOfScalar(u8, trimmed, ':') orelse continue;
                id = std.mem.trim(u8, trimmed[colon_idx + 1 ..], " \t");
            } else if (std.mem.startsWith(u8, trimmed, "status:")) {
                const colon_idx = std.mem.indexOfScalar(u8, trimmed, ':') orelse continue;
                status = std.mem.trim(u8, trimmed[colon_idx + 1 ..], " \t");
            } else if (std.mem.startsWith(u8, trimmed, "parent:")) {
                const colon_idx = std.mem.indexOfScalar(u8, trimmed, ':') orelse continue;
                const value = std.mem.trim(u8, trimmed[colon_idx + 1 ..], " \t");
                if (std.mem.indexOf(u8, value, "#")) |hash_idx| {
                    parent = std.mem.trim(u8, value[0..hash_idx], " \t");
                } else {
                    parent = value;
                }
            } else if (std.mem.startsWith(u8, trimmed, "priority:")) {
                const colon_idx = std.mem.indexOfScalar(u8, trimmed, ':') orelse continue;
                const value = std.mem.trim(u8, trimmed[colon_idx + 1 ..], " \t");
                priority = std.fmt.parseInt(i32, value, 10) catch 2;
            } else if (std.mem.startsWith(u8, trimmed, "deps:")) {
                const colon_idx = std.mem.indexOfScalar(u8, trimmed, ':') orelse continue;
                const value = std.mem.trim(u8, trimmed[colon_idx + 1 ..], " \t");
                if (std.mem.startsWith(u8, value, "[") and std.mem.endsWith(u8, value, "]")) {
                    const inner = value[1 .. value.len - 1];
                    const inner_trimmed = std.mem.trim(u8, inner, " \t");
                    if (inner_trimmed.len > 0) {
                        var items = std.mem.splitScalar(u8, inner_trimmed, ',');
                        while (items.next()) |item| {
                            const item_trimmed = std.mem.trim(u8, item, " \t");
                            if (item_trimmed.len > 0) {
                                try deps.append(allocator, try allocator.dupe(u8, item_trimmed));
                            }
                        }
                    }
                }
            } else if (std.mem.startsWith(u8, trimmed, "links:")) {
                const colon_idx = std.mem.indexOfScalar(u8, trimmed, ':') orelse continue;
                const value = std.mem.trim(u8, trimmed[colon_idx + 1 ..], " \t");
                if (std.mem.startsWith(u8, value, "[") and std.mem.endsWith(u8, value, "]")) {
                    const inner = value[1 .. value.len - 1];
                    const inner_trimmed = std.mem.trim(u8, inner, " \t");
                    if (inner_trimmed.len > 0) {
                        var items = std.mem.splitScalar(u8, inner_trimmed, ',');
                        while (items.next()) |item| {
                            const item_trimmed = std.mem.trim(u8, item, " \t");
                            if (item_trimmed.len > 0) {
                                try links.append(allocator, try allocator.dupe(u8, item_trimmed));
                            }
                        }
                    }
                }
            }
        } else if (after_frontmatter and title.len == 0) {
            if (std.mem.startsWith(u8, trimmed, "# ")) {
                title = std.mem.trim(u8, trimmed[2..], " \t");
                break;
            }
        }
    }

    return TicketData{
        .id = try allocator.dupe(u8, id),
        .status = try allocator.dupe(u8, status),
        .title = try allocator.dupe(u8, title),
        .deps = try deps.toOwnedSlice(allocator),
        .links = try links.toOwnedSlice(allocator),
        .parent = try allocator.dupe(u8, parent),
        .priority = priority,
        .allocator = allocator,
    };
}

// Helper function to load all tickets
fn loadAllTickets(allocator: std.mem.Allocator) !std.StringHashMap(TicketData) {
    var tickets = std.StringHashMap(TicketData).init(allocator);
    errdefer {
        var iter = tickets.valueIterator();
        while (iter.next()) |ticket| {
            var t = ticket.*;
            t.deinit();
        }
        tickets.deinit();
    }

    var dir = std.fs.cwd().openDir(tickets_dir, .{ .iterate = true }) catch {
        return tickets;
    };
    defer dir.close();

    var iter = dir.iterate();
    while (try iter.next()) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.endsWith(u8, entry.name, ".md")) continue;

        const file_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ tickets_dir, entry.name });
        defer allocator.free(file_path);

        const ticket = parseTicket(allocator, file_path) catch continue;
        if (ticket.id.len > 0) {
            const key = try allocator.dupe(u8, ticket.id);
            try tickets.put(key, ticket);
        }
    }

    return tickets;
}

// Handle dep tree subcommand
fn handleDepTree(allocator: std.mem.Allocator, args: []const [:0]const u8) !u8 {
    var full_mode = false;
    var root_id: ?[]const u8 = null;

    for (args) |arg| {
        if (std.mem.eql(u8, arg, "--full")) {
            full_mode = true;
        } else {
            root_id = arg;
        }
    }

    if (root_id == null) {
        try stderr_file.writeAll("Usage: ticket dep tree [--full] <id>\n");
        return 1;
    }

    // Load all tickets
    var tickets = try loadAllTickets(allocator);
    defer {
        var iter = tickets.iterator();
        while (iter.next()) |entry| {
            allocator.free(entry.key_ptr.*);
            var ticket = entry.value_ptr.*;
            ticket.deinit();
        }
        tickets.deinit();
    }

    if (tickets.count() == 0) {
        var buf: [512]u8 = undefined;
        const msg = try std.fmt.bufPrint(&buf, "Error: ticket {s} not found\n", .{root_id.?});
        try stderr_file.writeAll(msg);
        return 1;
    }

    // Resolve partial ID
    var root: ?[]const u8 = null;
    var iter = tickets.keyIterator();
    while (iter.next()) |ticket_id| {
        if (std.mem.indexOf(u8, ticket_id.*, root_id.?) != null) {
            if (root != null) {
                var buf: [512]u8 = undefined;
                const msg = try std.fmt.bufPrint(&buf, "Error: ambiguous ID {s}\n", .{root_id.?});
                try stderr_file.writeAll(msg);
                return 1;
            }
            root = ticket_id.*;
        }
    }

    if (root == null) {
        var buf: [512]u8 = undefined;
        const msg = try std.fmt.bufPrint(&buf, "Error: ticket {s} not found\n", .{root_id.?});
        try stderr_file.writeAll(msg);
        return 1;
    }

    // Print tree
    var printed = std.StringHashMap(void).init(allocator);
    defer printed.deinit();

    try printDepTree(allocator, &tickets, root.?, "", true, root.?, &printed, full_mode);

    return 0;
}

// Recursive function to print dependency tree
fn printDepTree(
    allocator: std.mem.Allocator,
    tickets: *std.StringHashMap(TicketData),
    ticket_id: []const u8,
    prefix: []const u8,
    is_last: bool,
    root: []const u8,
    printed: *std.StringHashMap(void),
    full_mode: bool,
) !void {
    // Skip if already printed in non-full mode
    if (!full_mode and printed.contains(ticket_id)) {
        return;
    }

    const ticket = tickets.get(ticket_id) orelse return;

    // Print current ticket
    var buf: [1024]u8 = undefined;
    const msg = if (std.mem.eql(u8, ticket_id, root))
        try std.fmt.bufPrint(&buf, "{s} [{s}] {s}\n", .{ ticket_id, ticket.status, ticket.title })
    else blk: {
        const connector = if (is_last) "└── " else "├── ";
        break :blk try std.fmt.bufPrint(&buf, "{s}{s}{s} [{s}] {s}\n", .{ prefix, connector, ticket_id, ticket.status, ticket.title });
    };
    try stdout_file.writeAll(msg);

    try printed.put(try allocator.dupe(u8, ticket_id), {});

    // Print dependencies
    if (ticket.deps.len > 0) {
        const new_prefix = if (std.mem.eql(u8, ticket_id, root))
            try allocator.dupe(u8, "")
        else if (is_last)
            try std.fmt.allocPrint(allocator, "{s}    ", .{prefix})
        else
            try std.fmt.allocPrint(allocator, "{s}│   ", .{prefix});
        defer allocator.free(new_prefix);

        for (ticket.deps, 0..) |dep_id, i| {
            const is_last_dep = (i == ticket.deps.len - 1);
            try printDepTree(allocator, tickets, dep_id, new_prefix, is_last_dep, root, printed, full_mode);
        }
    }
}

test "basic CLI argument parsing" {
    const allocator = std.testing.allocator;

    // Test that we can create args array
    var args: std.ArrayList([]const u8) = .empty;
    defer args.deinit(allocator);

    try args.append(allocator, "ticket");
    try args.append(allocator, "create");
    try args.append(allocator, "Test ticket");

    try std.testing.expectEqual(@as(usize, 3), args.items.len);
    try std.testing.expectEqualStrings("ticket", args.items[0]);
    try std.testing.expectEqualStrings("create", args.items[1]);
}

test "command string comparison" {
    try std.testing.expect(std.mem.eql(u8, "create", "create"));
    try std.testing.expect(!std.mem.eql(u8, "create", "status"));
}

test "TicketData struct initialization and cleanup" {
    const allocator = std.testing.allocator;

    var deps: std.ArrayList([]const u8) = .empty;
    try deps.append(allocator, try allocator.dupe(u8, "tc-1234"));
    try deps.append(allocator, try allocator.dupe(u8, "tc-5678"));

    var links: std.ArrayList([]const u8) = .empty;
    try links.append(allocator, try allocator.dupe(u8, "tc-abcd"));

    var ticket = TicketData{
        .id = try allocator.dupe(u8, "tc-test"),
        .status = try allocator.dupe(u8, "open"),
        .title = try allocator.dupe(u8, "Test Ticket"),
        .deps = try deps.toOwnedSlice(allocator),
        .links = try links.toOwnedSlice(allocator),
        .parent = try allocator.dupe(u8, ""),
        .allocator = allocator,
    };
    defer ticket.deinit();

    try std.testing.expectEqualStrings("tc-test", ticket.id);
    try std.testing.expectEqualStrings("open", ticket.status);
    try std.testing.expectEqualStrings("Test Ticket", ticket.title);
    try std.testing.expectEqual(@as(usize, 2), ticket.deps.len);
    try std.testing.expectEqual(@as(usize, 1), ticket.links.len);
}

test "parseListField with empty array" {
    const allocator = std.testing.allocator;
    const content =
        \\---
        \\id: tc-1234
        \\deps: []
        \\---
    ;

    const result = try parseListField(allocator, content, "deps:");
    defer {
        for (result) |item| {
            allocator.free(item);
        }
        allocator.free(result);
    }

    try std.testing.expectEqual(@as(usize, 0), result.len);
}

test "parseListField with single item" {
    const allocator = std.testing.allocator;
    const content =
        \\---
        \\id: tc-1234
        \\deps: [tc-5678]
        \\---
    ;

    const result = try parseListField(allocator, content, "deps:");
    defer {
        for (result) |item| {
            allocator.free(item);
        }
        allocator.free(result);
    }

    try std.testing.expectEqual(@as(usize, 1), result.len);
    try std.testing.expectEqualStrings("tc-5678", result[0]);
}

test "parseListField with multiple items" {
    const allocator = std.testing.allocator;
    const content =
        \\---
        \\id: tc-1234
        \\deps: [tc-5678, tc-9abc, tc-def0]
        \\---
    ;

    const result = try parseListField(allocator, content, "deps:");
    defer {
        for (result) |item| {
            allocator.free(item);
        }
        allocator.free(result);
    }

    try std.testing.expectEqual(@as(usize, 3), result.len);
    try std.testing.expectEqualStrings("tc-5678", result[0]);
    try std.testing.expectEqualStrings("tc-9abc", result[1]);
    try std.testing.expectEqualStrings("tc-def0", result[2]);
}

test "RelationshipItem struct" {
    const item = RelationshipItem{
        .id = "tc-1234",
        .status = "open",
        .title = "Test ticket",
    };

    try std.testing.expectEqualStrings("tc-1234", item.id);
    try std.testing.expectEqualStrings("open", item.status);
    try std.testing.expectEqualStrings("Test ticket", item.title);
}
