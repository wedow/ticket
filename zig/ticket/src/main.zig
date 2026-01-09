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
    _ = allocator;
    _ = args;
    const stdout = stdout_file;
    try stdout.writeAll("Error: create command not yet implemented\n");
    return 1;
}

fn handleStatus(allocator: std.mem.Allocator, args: []const [:0]const u8) !u8 {
    if (args.len < 2) {
        try stderr_file.writeAll("Usage: ticket status <id> <status>\n");
        return 1;
    }

    const ticket_id = args[0];
    const new_status = args[1];

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

    // Update the status field
    try updateYAMLScalarField(allocator, file_path, "status", new_status);

    var buf: [512]u8 = undefined;
    const msg = try std.fmt.bufPrint(&buf, "Updated status to: {s}\n", .{new_status});
    try stdout_file.writeAll(msg);
    return 0;
}

fn handleStart(allocator: std.mem.Allocator, args: []const [:0]const u8) !u8 {
    _ = allocator;
    _ = args;
    const stdout = stdout_file;
    try stdout.writeAll("Error: start command not yet implemented\n");
    return 1;
}

fn handleClose(allocator: std.mem.Allocator, args: []const [:0]const u8) !u8 {
    _ = allocator;
    _ = args;
    const stdout = stdout_file;
    try stdout.writeAll("Error: close command not yet implemented\n");
    return 1;
}

fn handleReopen(allocator: std.mem.Allocator, args: []const [:0]const u8) !u8 {
    _ = allocator;
    _ = args;
    const stdout = stdout_file;
    try stdout.writeAll("Error: reopen command not yet implemented\n");
    return 1;
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
    _ = allocator;
    _ = args;
    const stdout = stdout_file;
    try stdout.writeAll("Error: ls command not yet implemented\n");
    return 1;
}

fn handleReady(allocator: std.mem.Allocator, args: []const [:0]const u8) !u8 {
    _ = allocator;
    _ = args;
    const stdout = stdout_file;
    try stdout.writeAll("Error: ready command not yet implemented\n");
    return 1;
}

fn handleBlocked(allocator: std.mem.Allocator, args: []const [:0]const u8) !u8 {
    _ = allocator;
    _ = args;
    const stdout = stdout_file;
    try stdout.writeAll("Error: blocked command not yet implemented\n");
    return 1;
}

fn handleClosedList(allocator: std.mem.Allocator, args: []const [:0]const u8) !u8 {
    _ = allocator;
    _ = args;
    const stdout = stdout_file;
    try stdout.writeAll("Error: closed command not yet implemented\n");
    return 1;
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
    _ = allocator;
    _ = args;
    const stdout = stdout_file;
    try stdout.writeAll("Error: unlink command not yet implemented\n");
    return 1;
}

fn handleEdit(allocator: std.mem.Allocator, args: []const [:0]const u8) !u8 {
    _ = allocator;
    _ = args;
    const stdout = stdout_file;
    try stdout.writeAll("Error: edit command not yet implemented\n");
    return 1;
}

fn handleAddNote(allocator: std.mem.Allocator, args: []const [:0]const u8) !u8 {
    _ = allocator;
    _ = args;
    const stdout = stdout_file;
    try stdout.writeAll("Error: add-note command not yet implemented\n");
    return 1;
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
