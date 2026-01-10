#!/usr/bin/env bun

import * as crypto from "crypto";
import * as fs from "fs";
import * as path from "path";
import { spawnSync } from "child_process";

const TICKETS_DIR = ".tickets";

const HELP_TEXT = `tk - minimal ticket system with dependency tracking

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
`;

function printHelp(): void {
  console.log(HELP_TEXT);
}

function ensureDir(): void {
  if (!fs.existsSync(TICKETS_DIR)) {
    fs.mkdirSync(TICKETS_DIR, { recursive: true });
  }
}

function generateId(): string {
  const dirName = path.basename(process.cwd());
  
  const segments = dirName.replace(/-/g, " ").replace(/_/g, " ").split(" ");
  let prefix = segments.map(s => s[0]).join("");
  
  if (!prefix) {
    prefix = dirName.substring(0, 3);
  }
  
  const entropy = `${process.pid}${Math.floor(Date.now() / 1000)}`;
  const hashVal = crypto.createHash("sha256").update(entropy).digest("hex").substring(0, 4);
  
  return `${prefix}-${hashVal}`;
}

function getGitUserName(): string {
  try {
    const result = spawnSync("git", ["config", "user.name"], {
      encoding: "utf-8",
    });
    
    if (result.status === 0) {
      return result.stdout.trim();
    }
  } catch (error) {
    // Ignore errors
  }
  return "";
}

function ticketPath(ticketId: string): string | null {
  const exactPath = path.join(TICKETS_DIR, `${ticketId}.md`);
  
  if (fs.existsSync(exactPath)) {
    return exactPath;
  }
  
  if (!fs.existsSync(TICKETS_DIR)) {
    console.error(`Error: ticket '${ticketId}' not found`);
    return null;
  }
  
  const files = fs.readdirSync(TICKETS_DIR);
  const matches = files
    .filter(f => f.endsWith(".md") && f.includes(ticketId))
    .map(f => path.join(TICKETS_DIR, f));
  
  if (matches.length === 1) {
    return matches[0];
  } else if (matches.length > 1) {
    console.error(`Error: ambiguous ID '${ticketId}' matches multiple tickets`);
    return null;
  } else {
    console.error(`Error: ticket '${ticketId}' not found`);
    return null;
  }
}

interface Ticket {
  frontmatter: Record<string, string>;
  body: string;
}

function parseTicket(filePath: string): Ticket {
  const content = fs.readFileSync(filePath, "utf-8");
  const lines = content.split("\n");
  
  const frontmatter: Record<string, string> = {};
  const bodyLines: string[] = [];
  let inFrontmatter = false;
  let frontmatterEnded = false;
  
  for (const line of lines) {
    if (line.trim() === "---") {
      if (!inFrontmatter) {
        inFrontmatter = true;
      } else {
        frontmatterEnded = true;
        inFrontmatter = false;
      }
      continue;
    }
    
    if (inFrontmatter) {
      const colonIndex = line.indexOf(":");
      if (colonIndex !== -1) {
        const key = line.substring(0, colonIndex).trim();
        const value = line.substring(colonIndex + 1).trim();
        frontmatter[key] = value;
      }
    } else if (frontmatterEnded) {
      bodyLines.push(line);
    }
  }
  
  return {
    frontmatter,
    body: bodyLines.join("\n"),
  };
}

function parseListField(value: string): string[] {
  if (!value) {
    return [];
  }
  value = value.trim();
  if (value === "[]") {
    return [];
  }
  value = value.replace(/^\[|\]$/g, "");
  return value.split(",").map(item => item.trim()).filter(item => item);
}

interface TicketData {
  frontmatter: Record<string, string>;
  body: string;
  title: string;
  filePath: string;
}

function loadAllTickets(): Record<string, TicketData> {
  if (!fs.existsSync(TICKETS_DIR)) {
    return {};
  }
  
  const tickets: Record<string, TicketData> = {};
  const files = fs.readdirSync(TICKETS_DIR);
  
  for (const file of files) {
    if (!file.endsWith(".md")) {
      continue;
    }
    
    const filePath = path.join(TICKETS_DIR, file);
    const ticket = parseTicket(filePath);
    const ticketId = ticket.frontmatter.id || path.basename(file, ".md");
    
    let title = "";
    const bodyLines = ticket.body.split("\n");
    for (const line of bodyLines) {
      if (line.startsWith("# ")) {
        title = line.substring(2).trim();
        break;
      }
    }
    
    tickets[ticketId] = {
      frontmatter: ticket.frontmatter,
      body: ticket.body,
      title,
      filePath,
    };
  }
  
  return tickets;
}

function updateYamlField(filePath: string, field: string, value: string): void {
  const content = fs.readFileSync(filePath, "utf-8");
  const lines = content.split("\n");
  
  let updated = false;
  let inFrontmatter = false;
  const resultLines: string[] = [];
  
  for (const line of lines) {
    if (line.trim() === "---") {
      resultLines.push(line);
      if (!inFrontmatter) {
        inFrontmatter = true;
      } else {
        inFrontmatter = false;
      }
      continue;
    }
    
    if (inFrontmatter && line.startsWith(`${field}:`)) {
      resultLines.push(`${field}: ${value}`);
      updated = true;
    } else {
      resultLines.push(line);
    }
  }
  
  if (!updated) {
    const newLines: string[] = [];
    let firstMarkerFound = false;
    for (const line of resultLines) {
      newLines.push(line);
      if (!firstMarkerFound && line.trim() === "---") {
        firstMarkerFound = true;
        newLines.push(`${field}: ${value}`);
      }
    }
    fs.writeFileSync(filePath, newLines.join("\n"), "utf-8");
  } else {
    fs.writeFileSync(filePath, resultLines.join("\n"), "utf-8");
  }
}

function cmdLink(args: string[]): number {
  if (args.length < 2) {
    console.error("Usage: ticket link <id> <id> [id...]");
    return 1;
  }
  
  const ticketIds: string[] = [];
  const ticketPaths: string[] = [];
  
  for (const ticketId of args) {
    const filePath = ticketPath(ticketId);
    if (!filePath) {
      return 1;
    }
    ticketIds.push(path.basename(filePath, ".md"));
    ticketPaths.push(filePath);
  }
  
  let totalAdded = 0;
  
  for (let i = 0; i < ticketPaths.length; i++) {
    const filePath = ticketPaths[i];
    const currentId = ticketIds[i];
    const ticket = parseTicket(filePath);
    
    const existingLinksStr = ticket.frontmatter.links || "[]";
    const existingLinks = parseListField(existingLinksStr);
    
    const linksToAdd: string[] = [];
    for (let j = 0; j < ticketIds.length; j++) {
      if (i !== j && !existingLinks.includes(ticketIds[j])) {
        linksToAdd.push(ticketIds[j]);
      }
    }
    
    if (linksToAdd.length > 0) {
      const newLinks = [...existingLinks, ...linksToAdd];
      const newLinksStr = "[" + newLinks.join(", ") + "]";
      updateYamlField(filePath, "links", newLinksStr);
      totalAdded += linksToAdd.length;
    }
  }
  
  if (totalAdded === 0) {
    console.log("All links already exist");
  } else {
    console.log(`Added ${totalAdded} link(s) between ${ticketIds.length} tickets`);
  }
  
  return 0;
}

function cmdUnlink(args: string[]): number {
  if (args.length !== 2) {
    console.error("Usage: ticket unlink <id> <target-id>");
    return 1;
  }
  
  const ticketId = args[0];
  const targetId = args[1];
  
  const filePath = ticketPath(ticketId);
  const targetPath = ticketPath(targetId);
  
  if (!filePath || !targetPath) {
    return 1;
  }
  
  const actualId = path.basename(filePath, ".md");
  const actualTargetId = path.basename(targetPath, ".md");
  
  const ticket = parseTicket(filePath);
  const existingLinksStr = ticket.frontmatter.links || "[]";
  const existingLinks = parseListField(existingLinksStr);
  
  if (!existingLinks.includes(actualTargetId)) {
    console.log("Link not found");
    return 1;
  }
  
  const newLinks = existingLinks.filter(link => link !== actualTargetId);
  const newLinksStr = "[" + newLinks.join(", ") + "]";
  updateYamlField(filePath, "links", newLinksStr);
  
  const targetTicket = parseTicket(targetPath);
  const targetLinksStr = targetTicket.frontmatter.links || "[]";
  const targetLinks = parseListField(targetLinksStr);
  
  const newTargetLinks = targetLinks.filter(link => link !== actualId);
  const newTargetLinksStr = "[" + newTargetLinks.join(", ") + "]";
  updateYamlField(targetPath, "links", newTargetLinksStr);
  
  console.log(`Removed link: ${actualId} <-> ${actualTargetId}`);
  return 0;
}

function cmdShow(args: string[]): number {
  if (args.length === 0) {
    console.error("Usage: ticket show <id>");
    return 1;
  }
  
  const ticketId = args[0];
  const filePath = ticketPath(ticketId);
  
  if (!filePath) {
    return 1;
  }
  
  const targetId = path.basename(filePath, ".md");
  
  const allTickets = loadAllTickets();
  
  if (!(targetId in allTickets)) {
    console.error(`Error: ticket '${ticketId}' not found`);
    return 1;
  }
  
  const targetTicket = allTickets[targetId];
  const targetFm = targetTicket.frontmatter;
  
  const blockers: Array<[string, string, string]> = [];
  const blocking: Array<[string, string, string]> = [];
  const children: Array<[string, string, string]> = [];
  const linked: Array<[string, string, string]> = [];
  
  const targetDeps = parseListField(targetFm.deps || "[]");
  const targetLinks = parseListField(targetFm.links || "[]");
  const targetParent = targetFm.parent || "";
  
  for (const [tid, ticket] of Object.entries(allTickets)) {
    const fm = ticket.frontmatter;
    const status = fm.status || "open";
    
    const deps = parseListField(fm.deps || "[]");
    if (deps.includes(targetId) && status !== "closed") {
      blocking.push([tid, status, ticket.title]);
    }
    
    const parent = fm.parent || "";
    if (parent === targetId) {
      children.push([tid, status, ticket.title]);
    }
  }
  
  for (const dep of targetDeps) {
    if (dep in allTickets) {
      const depStatus = allTickets[dep].frontmatter.status || "open";
      if (depStatus !== "closed") {
        blockers.push([dep, depStatus, allTickets[dep].title]);
      }
    }
  }
  
  for (const link of targetLinks) {
    if (link in allTickets) {
      const linkStatus = allTickets[link].frontmatter.status || "open";
      linked.push([link, linkStatus, allTickets[link].title]);
    }
  }
  
  const content = fs.readFileSync(filePath, "utf-8");
  const lines = content.split("\n");
  let inFrontmatter = false;
  
  for (const line of lines) {
    if (line.trim() === "---") {
      if (!inFrontmatter) {
        inFrontmatter = true;
        console.log(line);
      } else {
        inFrontmatter = false;
        console.log(line);
      }
      continue;
    }
    
    if (inFrontmatter) {
      if (line.startsWith("parent:") && targetParent) {
        if (targetParent in allTickets) {
          const parentTitle = allTickets[targetParent].title;
          console.log(`${line}  # ${parentTitle}`);
        } else {
          console.log(line);
        }
      } else {
        console.log(line);
      }
    } else {
      console.log(line);
    }
  }
  
  if (blockers.length > 0) {
    console.log();
    console.log("## Blockers");
    console.log();
    for (const [depId, status, title] of blockers) {
      console.log(`- ${depId} [${status}] ${title}`);
    }
  }
  
  if (blocking.length > 0) {
    console.log();
    console.log("## Blocking");
    console.log();
    for (const [tid, status, title] of blocking) {
      console.log(`- ${tid} [${status}] ${title}`);
    }
  }
  
  if (children.length > 0) {
    console.log();
    console.log("## Children");
    console.log();
    for (const [tid, status, title] of children) {
      console.log(`- ${tid} [${status}] ${title}`);
    }
  }
  
  if (linked.length > 0) {
    console.log();
    console.log("## Linked");
    console.log();
    for (const [tid, status, title] of linked) {
      console.log(`- ${tid} [${status}] ${title}`);
    }
  }
  
  return 0;
}

function cmdStatus(args: string[]): number {
  const VALID_STATUSES = ["open", "in_progress", "closed"];
  
  if (args.length < 2) {
    console.error("Usage: ticket status <id> <status>");
    console.error(`Valid statuses: ${VALID_STATUSES.join(" ")}`);
    return 1;
  }
  
  const ticketId = args[0];
  const status = args[1];
  
  if (!VALID_STATUSES.includes(status)) {
    console.error(`Error: invalid status '${status}'. Must be one of: ${VALID_STATUSES.join(" ")}`);
    return 1;
  }
  
  const filePath = ticketPath(ticketId);
  
  if (!filePath) {
    return 1;
  }
  
  const actualId = path.basename(filePath, ".md");
  updateYamlField(filePath, "status", status);
  
  console.log(`Updated ${actualId} -> ${status}`);
  return 0;
}

function cmdStart(args: string[]): number {
  if (args.length === 0) {
    console.error("Usage: ticket start <id>");
    return 1;
  }
  
  return cmdStatus([args[0], "in_progress"]);
}

function cmdClose(args: string[]): number {
  if (args.length === 0) {
    console.error("Usage: ticket close <id>");
    return 1;
  }
  
  return cmdStatus([args[0], "closed"]);
}

function cmdReopen(args: string[]): number {
  if (args.length === 0) {
    console.error("Usage: ticket reopen <id>");
    return 1;
  }
  
  return cmdStatus([args[0], "open"]);
}

function cmdDep(args: string[]): number {
  if (args.length < 2) {
    console.error("Usage: ticket dep <id> <dependency-id>");
    return 1;
  }
  
  const ticketId = args[0];
  const depId = args[1];
  
  const filePath = ticketPath(ticketId);
  const depPath = ticketPath(depId);
  
  if (!filePath || !depPath) {
    return 1;
  }
  
  const actualId = path.basename(filePath, ".md");
  const actualDepId = path.basename(depPath, ".md");
  
  const ticket = parseTicket(filePath);
  const existingDepsStr = ticket.frontmatter.deps || "[]";
  const existingDeps = parseListField(existingDepsStr);
  
  if (existingDeps.includes(actualDepId)) {
    console.log("Dependency already exists");
    return 0;
  }
  
  const newDeps = [...existingDeps, actualDepId];
  const newDepsStr = "[" + newDeps.join(", ") + "]";
  updateYamlField(filePath, "deps", newDepsStr);
  
  console.log(`Added dependency: ${actualId} -> ${actualDepId}`);
  return 0;
}

function cmdQuery(args: string[]): number {
  if (!fs.existsSync(TICKETS_DIR)) {
    return 0;
  }
  
  const jqFilter = args.length > 0 ? args[0] : null;
  
  const jsonLines: string[] = [];
  const files = fs.readdirSync(TICKETS_DIR)
    .filter(f => f.endsWith(".md"))
    .sort();
  
  for (const file of files) {
    const filePath = path.join(TICKETS_DIR, file);
    const ticket = parseTicket(filePath);
    
    const jsonObj: Record<string, any> = {};
    for (const [key, value] of Object.entries(ticket.frontmatter)) {
      if (key === "deps" || key === "links") {
        jsonObj[key] = parseListField(value);
      } else {
        jsonObj[key] = value;
      }
    }
    
    jsonLines.push(JSON.stringify(jsonObj));
  }
  
  if (jqFilter) {
    try {
      const { spawnSync } = require("child_process");
      const input = jsonLines.join("\n");
      
      const result = spawnSync("jq", ["-c", `select(${jqFilter})`], {
        input,
        encoding: "utf-8",
      });
      
      if (result.error) {
        console.error("Error: jq is required for filtering");
        return 1;
      }
      
      if (result.status !== 0) {
        return 1;
      }
      
      process.stdout.write(result.stdout);
    } catch (error) {
      console.error("Error: jq is required for filtering");
      return 1;
    }
  } else {
    for (const line of jsonLines) {
      console.log(line);
    }
  }
  
  return 0;
}

function isoDate(): string {
  return new Date().toISOString().replace(/\.\d{3}Z$/, "Z");
}

function cmdCreate(args: string[]): number {
  ensureDir();

  let title = "";
  let description = "";
  let design = "";
  let acceptance = "";
  let priority = 2;
  let issueType = "task";
  let assignee = getGitUserName();
  let externalRef = "";
  let parent = "";

  let i = 0;
  while (i < args.length) {
    const arg = args[i];
    if (arg === "-d" || arg === "--description") {
      if (i + 1 >= args.length) {
        console.error(`Error: ${arg} requires an argument`);
        return 1;
      }
      description = args[i + 1];
      i += 2;
    } else if (arg === "--design") {
      if (i + 1 >= args.length) {
        console.error(`Error: ${arg} requires an argument`);
        return 1;
      }
      design = args[i + 1];
      i += 2;
    } else if (arg === "--acceptance") {
      if (i + 1 >= args.length) {
        console.error(`Error: ${arg} requires an argument`);
        return 1;
      }
      acceptance = args[i + 1];
      i += 2;
    } else if (arg === "-p" || arg === "--priority") {
      if (i + 1 >= args.length) {
        console.error(`Error: ${arg} requires an argument`);
        return 1;
      }
      priority = parseInt(args[i + 1]);
      i += 2;
    } else if (arg === "-t" || arg === "--type") {
      if (i + 1 >= args.length) {
        console.error(`Error: ${arg} requires an argument`);
        return 1;
      }
      issueType = args[i + 1];
      i += 2;
    } else if (arg === "-a" || arg === "--assignee") {
      if (i + 1 >= args.length) {
        console.error(`Error: ${arg} requires an argument`);
        return 1;
      }
      assignee = args[i + 1];
      i += 2;
    } else if (arg === "--external-ref") {
      if (i + 1 >= args.length) {
        console.error(`Error: ${arg} requires an argument`);
        return 1;
      }
      externalRef = args[i + 1];
      i += 2;
    } else if (arg === "--parent") {
      if (i + 1 >= args.length) {
        console.error(`Error: ${arg} requires an argument`);
        return 1;
      }
      parent = args[i + 1];
      i += 2;
    } else if (arg.startsWith("-")) {
      console.error(`Unknown option: ${arg}`);
      return 1;
    } else {
      title = arg;
      i += 1;
    }
  }

  title = title || "Untitled";
  const ticketId = generateId();
  const filePath = path.join(TICKETS_DIR, `${ticketId}.md`);
  const now = isoDate();

  const contentParts: string[] = [];
  contentParts.push("---");
  contentParts.push(`id: ${ticketId}`);
  contentParts.push("status: open");
  contentParts.push("deps: []");
  contentParts.push("links: []");
  contentParts.push(`created: ${now}`);
  contentParts.push(`type: ${issueType}`);
  contentParts.push(`priority: ${priority}`);
  if (assignee) {
    contentParts.push(`assignee: ${assignee}`);
  }
  if (externalRef) {
    contentParts.push(`external-ref: ${externalRef}`);
  }
  if (parent) {
    contentParts.push(`parent: ${parent}`);
  }
  contentParts.push("---");
  contentParts.push(`# ${title}`);
  contentParts.push("");

  if (description) {
    contentParts.push(description);
    contentParts.push("");
  }

  if (design) {
    contentParts.push("## Design");
    contentParts.push("");
    contentParts.push(design);
    contentParts.push("");
  }

  if (acceptance) {
    contentParts.push("## Acceptance Criteria");
    contentParts.push("");
    contentParts.push(acceptance);
    contentParts.push("");
  }

  fs.writeFileSync(filePath, contentParts.join("\n"), "utf-8");

  console.log(ticketId);
  return 0;
}

function cmdAddNote(args: string[]): number {
  if (args.length === 0) {
    console.error("Usage: ticket add-note <id> [note text]");
    return 1;
  }

  const ticketId = args[0];
  const filePath = ticketPath(ticketId);

  if (!filePath) {
    return 1;
  }

  const actualId = path.basename(filePath, ".md");

  const note = args.length > 1 ? args.slice(1).join(" ") : "";

  const timestamp = isoDate();

  let content = fs.readFileSync(filePath, "utf-8");

  if (!content.includes("## Notes")) {
    content += "\n## Notes\n";
  }

  content += `\n**${timestamp}**\n\n${note}\n`;

  fs.writeFileSync(filePath, content, "utf-8");

  console.log(`Note added to ${actualId}`);
  return 0;
}

function cmdLs(args: string[]): number {
  if (!fs.existsSync(TICKETS_DIR)) {
    return 0;
  }

  let statusFilter = "";
  for (const arg of args) {
    if (arg.startsWith("--status=")) {
      statusFilter = arg.split("=")[1];
    }
  }

  const allTickets = loadAllTickets();
  const sortedIds = Object.keys(allTickets).sort();

  for (const ticketId of sortedIds) {
    const ticket = allTickets[ticketId];
    const status = ticket.frontmatter.status || "open";

    if (statusFilter && status !== statusFilter) {
      continue;
    }

    const title = ticket.title;
    const depsStr = ticket.frontmatter.deps || "[]";
    const deps = parseListField(depsStr);

    let depDisplay = "";
    if (deps.length > 0) {
      depDisplay = ` <- [${deps.join(", ")}]`;
    }

    console.log(`${ticketId.padEnd(8)} [${status}] - ${title}${depDisplay}`);
  }

  return 0;
}

function cmdReady(args: string[]): number {
  if (!fs.existsSync(TICKETS_DIR)) {
    return 0;
  }

  const allTickets = loadAllTickets();
  const readyTickets: Array<[number, string, string, string]> = [];

  for (const [ticketId, ticket] of Object.entries(allTickets)) {
    const status = ticket.frontmatter.status || "open";

    if (status !== "open" && status !== "in_progress") {
      continue;
    }

    const depsStr = ticket.frontmatter.deps || "[]";
    const deps = parseListField(depsStr);

    let ready = true;
    for (const depId of deps) {
      if (depId in allTickets) {
        const depStatus = allTickets[depId].frontmatter.status || "open";
        if (depStatus !== "closed") {
          ready = false;
          break;
        }
      } else {
        ready = false;
        break;
      }
    }

    if (ready) {
      const priority = parseInt(ticket.frontmatter.priority || "2");
      const title = ticket.title;
      readyTickets.push([priority, ticketId, status, title]);
    }
  }

  readyTickets.sort((a, b) => {
    if (a[0] !== b[0]) {
      return a[0] - b[0];
    }
    return a[1].localeCompare(b[1]);
  });

  for (const [priority, ticketId, status, title] of readyTickets) {
    console.log(`${ticketId.padEnd(8)} [P${priority}][${status}] - ${title}`);
  }

  return 0;
}

function cmdBlocked(args: string[]): number {
  if (!fs.existsSync(TICKETS_DIR)) {
    return 0;
  }

  const allTickets = loadAllTickets();
  const blockedTickets: Array<[number, string, string, string, string[]]> = [];

  for (const [ticketId, ticket] of Object.entries(allTickets)) {
    const status = ticket.frontmatter.status || "open";

    if (status !== "open" && status !== "in_progress") {
      continue;
    }

    const depsStr = ticket.frontmatter.deps || "[]";
    const deps = parseListField(depsStr);

    if (deps.length === 0) {
      continue;
    }

    const unclosedBlockers: string[] = [];
    for (const depId of deps) {
      if (depId in allTickets) {
        const depStatus = allTickets[depId].frontmatter.status || "open";
        if (depStatus !== "closed") {
          unclosedBlockers.push(depId);
        }
      } else {
        unclosedBlockers.push(depId);
      }
    }

    if (unclosedBlockers.length > 0) {
      const priority = parseInt(ticket.frontmatter.priority || "2");
      const title = ticket.title;
      blockedTickets.push([priority, ticketId, status, title, unclosedBlockers]);
    }
  }

  blockedTickets.sort((a, b) => {
    if (a[0] !== b[0]) {
      return a[0] - b[0];
    }
    return a[1].localeCompare(b[1]);
  });

  for (const [priority, ticketId, status, title, blockers] of blockedTickets) {
    const blockersStr = blockers.join(", ");
    console.log(`${ticketId.padEnd(8)} [P${priority}][${status}] - ${title} <- [${blockersStr}]`);
  }

  return 0;
}

function cmdClosed(args: string[]): number {
  if (!fs.existsSync(TICKETS_DIR)) {
    return 0;
  }

  let limit = 20;
  for (const arg of args) {
    if (arg.startsWith("--limit=")) {
      limit = parseInt(arg.split("=")[1]);
    }
  }

  const ticketFiles = fs.readdirSync(TICKETS_DIR)
    .filter(f => f.endsWith(".md"))
    .map(f => ({
      name: f,
      path: path.join(TICKETS_DIR, f),
      mtime: fs.statSync(path.join(TICKETS_DIR, f)).mtimeMs,
    }))
    .sort((a, b) => b.mtime - a.mtime);

  const closedTickets: Array<[string, string, string]> = [];
  
  for (const file of ticketFiles.slice(0, 100)) {
    const ticket = parseTicket(file.path);
    const status = ticket.frontmatter.status || "open";

    if (status === "closed" || status === "done") {
      const ticketId = ticket.frontmatter.id || path.basename(file.name, ".md");
      
      let title = "";
      const bodyLines = ticket.body.split("\n");
      for (const line of bodyLines) {
        if (line.startsWith("# ")) {
          title = line.substring(2).trim();
          break;
        }
      }
      
      closedTickets.push([ticketId, status, title]);

      if (closedTickets.length >= limit) {
        break;
      }
    }
  }

  for (const [ticketId, status, title] of closedTickets) {
    console.log(`${ticketId.padEnd(8)} [${status}] - ${title}`);
  }

  return 0;
}

function main(): number {
  const args = process.argv.slice(2);

  if (args.length === 0 || args[0] === "-h" || args[0] === "--help" || args[0] === "help") {
    printHelp();
    return 0;
  }

  const command = args[0];
  const commandArgs = args.slice(1);

  if (command === "create") {
    return cmdCreate(commandArgs);
  } else if (command === "show") {
    return cmdShow(commandArgs);
  } else if (command === "status") {
    return cmdStatus(commandArgs);
  } else if (command === "start") {
    return cmdStart(commandArgs);
  } else if (command === "close") {
    return cmdClose(commandArgs);
  } else if (command === "reopen") {
    return cmdReopen(commandArgs);
  } else if (command === "dep") {
    return cmdDep(commandArgs);
  } else if (command === "link") {
    return cmdLink(commandArgs);
  } else if (command === "unlink") {
    return cmdUnlink(commandArgs);
  } else if (command === "query") {
    return cmdQuery(commandArgs);
  } else if (command === "add-note") {
    return cmdAddNote(commandArgs);
  } else if (command === "ls") {
    return cmdLs(commandArgs);
  } else if (command === "ready") {
    return cmdReady(commandArgs);
  } else if (command === "blocked") {
    return cmdBlocked(commandArgs);
  } else if (command === "closed") {
    return cmdClosed(commandArgs);
  }

  console.log("Ticket CLI - TypeScript port (work in progress)");
  console.log(`Command not yet implemented: ${command}`);
  return 1;
}

const exitCode = main();
process.exit(exitCode);
