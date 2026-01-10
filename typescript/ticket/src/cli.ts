#!/usr/bin/env bun

import * as fs from "fs";
import * as path from "path";

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
  
  const content = fs.readFileSync(filePath, "utf-8");
  console.log(content);
  
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

function main(): number {
  const args = process.argv.slice(2);

  if (args.length === 0 || args[0] === "-h" || args[0] === "--help" || args[0] === "help") {
    printHelp();
    return 0;
  }

  const command = args[0];
  const commandArgs = args.slice(1);

  if (command === "show") {
    return cmdShow(commandArgs);
  } else if (command === "status") {
    return cmdStatus(commandArgs);
  } else if (command === "dep") {
    return cmdDep(commandArgs);
  } else if (command === "link") {
    return cmdLink(commandArgs);
  } else if (command === "unlink") {
    return cmdUnlink(commandArgs);
  } else if (command === "query") {
    return cmdQuery(commandArgs);
  }

  console.log("Ticket CLI - TypeScript port (work in progress)");
  console.log(`Command not yet implemented: ${command}`);
  return 1;
}

const exitCode = main();
process.exit(exitCode);
