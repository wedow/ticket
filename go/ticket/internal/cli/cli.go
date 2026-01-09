package cli

import (
	"bytes"
	"crypto/sha256"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"time"
)

const ticketsDir = ".tickets"

// Run executes the CLI with the given arguments and returns an exit code.
func Run(args []string) int {
	if len(args) == 0 || args[0] == "-h" || args[0] == "--help" || args[0] == "help" {
		printHelp()
		return 0
	}

	command := args[0]
	commandArgs := args[1:]

	switch command {
	case "create":
		return cmdCreate(commandArgs)
	case "query":
		return cmdQuery(commandArgs)
	case "add-note":
		return cmdAddNote(commandArgs)
	case "show":
		return cmdShow(commandArgs)
	case "status":
		return cmdStatus(commandArgs)
	case "start":
		return cmdStart(commandArgs)
	case "close":
		return cmdClose(commandArgs)
	case "reopen":
		return cmdReopen(commandArgs)
	case "edit":
		return cmdEdit(commandArgs)
	default:
		fmt.Println("Ticket CLI - Go port (work in progress)")
		fmt.Printf("Command not yet implemented: %s\n", command)
		return 1
	}
}

func printHelp() {
	fmt.Print(`tk - minimal ticket system with dependency tracking

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
  ready                    List open/in_progress tickets with deps resolved
  blocked                  List open/in_progress tickets with unresolved deps
  closed [--limit=N]       List recently closed tickets (default 20, by mtime)
  show <id>                Display ticket
  edit <id>                Open ticket in $EDITOR
  add-note <id> [text]     Append timestamped note (or pipe via stdin)
  query [jq-filter]        Output tickets as JSON, optionally filtered
  migrate-beads            Import tickets from .beads/issues.jsonl

Tickets stored as markdown files in .tickets/
Supports partial ID matching (e.g., 'tk show 5c4' matches 'nw-5c46')
`)
}

func generateID() string {
	cwd, err := os.Getwd()
	if err != nil {
		cwd = "ticket"
	}

	dirName := filepath.Base(cwd)

	// Extract first letter of each hyphenated/underscored segment
	segments := strings.FieldsFunc(dirName, func(r rune) bool {
		return r == '-' || r == '_'
	})

	var prefix string
	for _, s := range segments {
		if len(s) > 0 {
			prefix += string(s[0])
		}
	}

	// Fallback to first 3 chars if no segments
	if prefix == "" {
		if len(dirName) >= 3 {
			prefix = dirName[:3]
		} else {
			prefix = dirName
		}
	}

	// 4-char hash from timestamp + PID for entropy
	entropy := fmt.Sprintf("%d%d", os.Getpid(), time.Now().UTC().Unix())
	hash := sha256.Sum256([]byte(entropy))
	hashStr := fmt.Sprintf("%x", hash)[:4]

	return fmt.Sprintf("%s-%s", prefix, hashStr)
}

func isoDate() string {
	return time.Now().UTC().Format("2006-01-02T15:04:05Z")
}

func ensureDir() error {
	return os.MkdirAll(ticketsDir, 0755)
}

func getGitUserName() string {
	cmd := exec.Command("git", "config", "user.name")
	output, err := cmd.Output()
	if err != nil {
		return ""
	}
	return strings.TrimSpace(string(output))
}

func cmdCreate(args []string) int {
	if err := ensureDir(); err != nil {
		fmt.Fprintf(os.Stderr, "Error creating tickets directory: %v\n", err)
		return 1
	}

	title := ""
	description := ""
	design := ""
	acceptance := ""
	priority := 2
	issueType := "task"
	assignee := getGitUserName()
	externalRef := ""
	parent := ""

	// Parse args
	i := 0
	for i < len(args) {
		arg := args[i]

		if arg == "-d" || arg == "--description" {
			if i+1 >= len(args) {
				fmt.Fprintf(os.Stderr, "Error: %s requires an argument\n", arg)
				return 1
			}
			description = args[i+1]
			i += 2
		} else if arg == "--design" {
			if i+1 >= len(args) {
				fmt.Fprintf(os.Stderr, "Error: %s requires an argument\n", arg)
				return 1
			}
			design = args[i+1]
			i += 2
		} else if arg == "--acceptance" {
			if i+1 >= len(args) {
				fmt.Fprintf(os.Stderr, "Error: %s requires an argument\n", arg)
				return 1
			}
			acceptance = args[i+1]
			i += 2
		} else if arg == "-p" || arg == "--priority" {
			if i+1 >= len(args) {
				fmt.Fprintf(os.Stderr, "Error: %s requires an argument\n", arg)
				return 1
			}
			p, err := strconv.Atoi(args[i+1])
			if err != nil {
				fmt.Fprintf(os.Stderr, "Error: invalid priority value: %s\n", args[i+1])
				return 1
			}
			priority = p
			i += 2
		} else if arg == "-t" || arg == "--type" {
			if i+1 >= len(args) {
				fmt.Fprintf(os.Stderr, "Error: %s requires an argument\n", arg)
				return 1
			}
			issueType = args[i+1]
			i += 2
		} else if arg == "-a" || arg == "--assignee" {
			if i+1 >= len(args) {
				fmt.Fprintf(os.Stderr, "Error: %s requires an argument\n", arg)
				return 1
			}
			assignee = args[i+1]
			i += 2
		} else if arg == "--external-ref" {
			if i+1 >= len(args) {
				fmt.Fprintf(os.Stderr, "Error: %s requires an argument\n", arg)
				return 1
			}
			externalRef = args[i+1]
			i += 2
		} else if arg == "--parent" {
			if i+1 >= len(args) {
				fmt.Fprintf(os.Stderr, "Error: %s requires an argument\n", arg)
				return 1
			}
			parent = args[i+1]
			i += 2
		} else if strings.HasPrefix(arg, "-") {
			fmt.Fprintf(os.Stderr, "Unknown option: %s\n", arg)
			return 1
		} else {
			title = arg
			i++
		}
	}

	if title == "" {
		title = "Untitled"
	}

	ticketID := generateID()
	filePath := filepath.Join(ticketsDir, ticketID+".md")
	now := isoDate()

	// Build content
	var contentParts []string
	contentParts = append(contentParts, "---")
	contentParts = append(contentParts, fmt.Sprintf("id: %s", ticketID))
	contentParts = append(contentParts, "status: open")
	contentParts = append(contentParts, "deps: []")
	contentParts = append(contentParts, "links: []")
	contentParts = append(contentParts, fmt.Sprintf("created: %s", now))
	contentParts = append(contentParts, fmt.Sprintf("type: %s", issueType))
	contentParts = append(contentParts, fmt.Sprintf("priority: %d", priority))
	if assignee != "" {
		contentParts = append(contentParts, fmt.Sprintf("assignee: %s", assignee))
	}
	if externalRef != "" {
		contentParts = append(contentParts, fmt.Sprintf("external-ref: %s", externalRef))
	}
	if parent != "" {
		contentParts = append(contentParts, fmt.Sprintf("parent: %s", parent))
	}
	contentParts = append(contentParts, "---")
	contentParts = append(contentParts, fmt.Sprintf("# %s", title))
	contentParts = append(contentParts, "")

	if description != "" {
		contentParts = append(contentParts, description)
		contentParts = append(contentParts, "")
	}

	if design != "" {
		contentParts = append(contentParts, "## Design")
		contentParts = append(contentParts, "")
		contentParts = append(contentParts, design)
		contentParts = append(contentParts, "")
	}

	if acceptance != "" {
		contentParts = append(contentParts, "## Acceptance Criteria")
		contentParts = append(contentParts, "")
		contentParts = append(contentParts, acceptance)
		contentParts = append(contentParts, "")
	}

	content := strings.Join(contentParts, "\n")

	if err := os.WriteFile(filePath, []byte(content), 0644); err != nil {
		fmt.Fprintf(os.Stderr, "Error writing ticket file: %v\n", err)
		return 1
	}

	fmt.Println(ticketID)
	return 0
}

func cmdQuery(args []string) int {
	// Check if tickets directory exists
	if _, err := os.Stat(ticketsDir); os.IsNotExist(err) {
		return 0
	}

	var filter string
	if len(args) > 0 {
		filter = args[0]
	}

	// Read all ticket files
	files, err := filepath.Glob(filepath.Join(ticketsDir, "*.md"))
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading tickets directory: %v\n", err)
		return 1
	}

	var jsonLines []string

	for _, file := range files {
		ticket, err := parseTicket(file)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error parsing ticket %s: %v\n", file, err)
			continue
		}

		jsonData, err := json.Marshal(ticket)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error marshaling ticket %s: %v\n", file, err)
			continue
		}

		jsonLines = append(jsonLines, string(jsonData))
	}

	jsonOutput := strings.Join(jsonLines, "\n")
	if jsonOutput == "" {
		return 0
	}

	// Add newline at the end if there's content
	jsonOutput += "\n"

	// If filter is provided, pipe through jq
	if filter != "" {
		cmd := exec.Command("jq", "-c", "select("+filter+")")
		cmd.Stdin = strings.NewReader(jsonOutput)
		var stdout, stderr bytes.Buffer
		cmd.Stdout = &stdout
		cmd.Stderr = &stderr

		if err := cmd.Run(); err != nil {
			fmt.Fprintf(os.Stderr, "jq error: %s\n", stderr.String())
			return 1
		}

		output := stdout.String()
		if output != "" {
			fmt.Print(output)
		}
	} else {
		fmt.Print(jsonOutput)
	}

	return 0
}

func parseTicket(filePath string) (map[string]interface{}, error) {
	content, err := os.ReadFile(filePath)
	if err != nil {
		return nil, err
	}

	lines := strings.Split(string(content), "\n")

	// Find frontmatter boundaries
	var frontmatterStart, frontmatterEnd int
	frontmatterCount := 0
	for i, line := range lines {
		if strings.TrimSpace(line) == "---" {
			frontmatterCount++
			if frontmatterCount == 1 {
				frontmatterStart = i
			} else if frontmatterCount == 2 {
				frontmatterEnd = i
				break
			}
		}
	}

	if frontmatterCount < 2 {
		return nil, fmt.Errorf("invalid frontmatter in %s", filePath)
	}

	ticket := make(map[string]interface{})

	// Parse frontmatter
	for i := frontmatterStart + 1; i < frontmatterEnd; i++ {
		line := lines[i]
		if strings.TrimSpace(line) == "" {
			continue
		}

		parts := strings.SplitN(line, ":", 2)
		if len(parts) != 2 {
			continue
		}

		key := strings.TrimSpace(parts[0])
		value := strings.TrimSpace(parts[1])

		// Parse arrays
		if strings.HasPrefix(value, "[") && strings.HasSuffix(value, "]") {
			arrayContent := strings.TrimSpace(value[1 : len(value)-1])
			if arrayContent == "" {
				ticket[key] = []string{}
			} else {
				// Split by comma and trim whitespace
				items := strings.Split(arrayContent, ",")
				var trimmedItems []string
				for _, item := range items {
					trimmed := strings.TrimSpace(item)
					if trimmed != "" {
						trimmedItems = append(trimmedItems, trimmed)
					}
				}
				ticket[key] = trimmedItems
			}
		} else if key == "priority" {
			// Parse priority as integer
			priority, err := strconv.Atoi(value)
			if err == nil {
				ticket[key] = priority
			} else {
				ticket[key] = value
			}
		} else {
			ticket[key] = value
		}
	}

	return ticket, nil
}

func resolveTicketID(ticketID string) (string, error) {
	exactPath := filepath.Join(ticketsDir, ticketID+".md")
	if _, err := os.Stat(exactPath); err == nil {
		return exactPath, nil
	}

	pattern := filepath.Join(ticketsDir, "*"+ticketID+"*.md")
	matches, err := filepath.Glob(pattern)
	if err != nil {
		return "", err
	}

	if len(matches) == 0 {
		return "", fmt.Errorf("ticket '%s' not found", ticketID)
	}
	if len(matches) > 1 {
		return "", fmt.Errorf("ambiguous ID '%s' matches multiple tickets", ticketID)
	}

	return matches[0], nil
}

func cmdAddNote(args []string) int {
	if len(args) == 0 {
		fmt.Fprintln(os.Stderr, "Usage: ticket add-note <id> [note text]")
		return 1
	}

	ticketID := args[0]

	filePath, err := resolveTicketID(ticketID)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		return 1
	}

	targetID := strings.TrimSuffix(filepath.Base(filePath), ".md")

	var note string
	if len(args) > 1 {
		note = strings.Join(args[1:], " ")
	} else {
		stat, _ := os.Stdin.Stat()
		if (stat.Mode() & os.ModeCharDevice) == 0 {
			data, err := io.ReadAll(os.Stdin)
			if err != nil {
				fmt.Fprintf(os.Stderr, "Error reading stdin: %v\n", err)
				return 1
			}
			note = strings.TrimSpace(string(data))
		} else {
			fmt.Fprintln(os.Stderr, "Error: no note provided")
			return 1
		}
	}

	timestamp := isoDate()

	content, err := os.ReadFile(filePath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error reading ticket: %v\n", err)
		return 1
	}

	contentStr := string(content)

	if !strings.Contains(contentStr, "## Notes") {
		contentStr += "\n## Notes\n"
	}

	contentStr += fmt.Sprintf("\n**%s**\n\n%s\n", timestamp, note)

	if err := os.WriteFile(filePath, []byte(contentStr), 0644); err != nil {
		fmt.Fprintf(os.Stderr, "Error writing ticket: %v\n", err)
		return 1
	}

	fmt.Printf("Note added to %s\n", targetID)
	return 0
}

type TicketData struct {
	ID           string
	Frontmatter  map[string]interface{}
	Title        string
	FullContent  string
	FilePath     string
}

func parseTicketFull(filePath string) (*TicketData, error) {
	content, err := os.ReadFile(filePath)
	if err != nil {
		return nil, err
	}

	lines := strings.Split(string(content), "\n")

	var frontmatterStart, frontmatterEnd int
	frontmatterCount := 0
	for i, line := range lines {
		if strings.TrimSpace(line) == "---" {
			frontmatterCount++
			if frontmatterCount == 1 {
				frontmatterStart = i
			} else if frontmatterCount == 2 {
				frontmatterEnd = i
				break
			}
		}
	}

	if frontmatterCount < 2 {
		return nil, fmt.Errorf("invalid frontmatter in %s", filePath)
	}

	frontmatter := make(map[string]interface{})

	for i := frontmatterStart + 1; i < frontmatterEnd; i++ {
		line := lines[i]
		if strings.TrimSpace(line) == "" {
			continue
		}

		parts := strings.SplitN(line, ":", 2)
		if len(parts) != 2 {
			continue
		}

		key := strings.TrimSpace(parts[0])
		value := strings.TrimSpace(parts[1])

		if strings.HasPrefix(value, "[") && strings.HasSuffix(value, "]") {
			arrayContent := strings.TrimSpace(value[1 : len(value)-1])
			if arrayContent == "" {
				frontmatter[key] = []string{}
			} else {
				items := strings.Split(arrayContent, ",")
				var trimmedItems []string
				for _, item := range items {
					trimmed := strings.TrimSpace(item)
					if trimmed != "" {
						trimmedItems = append(trimmedItems, trimmed)
					}
				}
				frontmatter[key] = trimmedItems
			}
		} else if key == "priority" {
			priority, err := strconv.Atoi(value)
			if err == nil {
				frontmatter[key] = priority
			} else {
				frontmatter[key] = value
			}
		} else {
			frontmatter[key] = value
		}
	}

	title := ""
	for i := frontmatterEnd + 1; i < len(lines); i++ {
		line := lines[i]
		if strings.HasPrefix(line, "# ") {
			title = strings.TrimSpace(line[2:])
			break
		}
	}

	id := ""
	if idVal, ok := frontmatter["id"].(string); ok {
		id = idVal
	}

	return &TicketData{
		ID:          id,
		Frontmatter: frontmatter,
		Title:       title,
		FullContent: string(content),
		FilePath:    filePath,
	}, nil
}

func loadAllTickets() (map[string]*TicketData, error) {
	tickets := make(map[string]*TicketData)

	if _, err := os.Stat(ticketsDir); os.IsNotExist(err) {
		return tickets, nil
	}

	files, err := filepath.Glob(filepath.Join(ticketsDir, "*.md"))
	if err != nil {
		return nil, err
	}

	for _, file := range files {
		ticket, err := parseTicketFull(file)
		if err != nil {
			continue
		}
		if ticket.ID != "" {
			tickets[ticket.ID] = ticket
		}
	}

	return tickets, nil
}

func parseListField(value interface{}) []string {
	if value == nil {
		return []string{}
	}

	switch v := value.(type) {
	case []string:
		return v
	case []interface{}:
		var result []string
		for _, item := range v {
			if str, ok := item.(string); ok {
				result = append(result, str)
			}
		}
		return result
	case string:
		if v == "" || v == "[]" {
			return []string{}
		}
		v = strings.Trim(v, "[]")
		items := strings.Split(v, ",")
		var result []string
		for _, item := range items {
			trimmed := strings.TrimSpace(item)
			if trimmed != "" {
				result = append(result, trimmed)
			}
		}
		return result
	default:
		return []string{}
	}
}

func cmdShow(args []string) int {
	if len(args) == 0 {
		fmt.Fprintln(os.Stderr, "Usage: ticket show <id>")
		return 1
	}

	ticketID := args[0]

	filePath, err := resolveTicketID(ticketID)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		return 1
	}

	targetID := strings.TrimSuffix(filepath.Base(filePath), ".md")

	allTickets, err := loadAllTickets()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error loading tickets: %v\n", err)
		return 1
	}

	targetTicket, ok := allTickets[targetID]
	if !ok {
		fmt.Fprintf(os.Stderr, "Error: ticket '%s' not found\n", ticketID)
		return 1
	}

	targetFm := targetTicket.Frontmatter

	var blockers []struct {
		id     string
		status string
		title  string
	}
	var blocking []struct {
		id     string
		status string
		title  string
	}
	var children []struct {
		id     string
		status string
		title  string
	}
	var linked []struct {
		id     string
		status string
		title  string
	}

	targetDeps := parseListField(targetFm["deps"])
	targetLinks := parseListField(targetFm["links"])
	targetParent := ""
	if p, ok := targetFm["parent"].(string); ok {
		targetParent = p
	}

	for tid, ticket := range allTickets {
		fm := ticket.Frontmatter
		status := "open"
		if s, ok := fm["status"].(string); ok {
			status = s
		}

		deps := parseListField(fm["deps"])
		for _, dep := range deps {
			if dep == targetID && status != "closed" {
				blocking = append(blocking, struct {
					id     string
					status string
					title  string
				}{tid, status, ticket.Title})
			}
		}

		parent := ""
		if p, ok := fm["parent"].(string); ok {
			parent = p
		}
		if parent == targetID {
			children = append(children, struct {
				id     string
				status string
				title  string
			}{tid, status, ticket.Title})
		}
	}

	for _, dep := range targetDeps {
		if depTicket, ok := allTickets[dep]; ok {
			depStatus := "open"
			if s, ok := depTicket.Frontmatter["status"].(string); ok {
				depStatus = s
			}
			if depStatus != "closed" {
				blockers = append(blockers, struct {
					id     string
					status string
					title  string
				}{dep, depStatus, depTicket.Title})
			}
		}
	}

	for _, link := range targetLinks {
		if linkTicket, ok := allTickets[link]; ok {
			linkStatus := "open"
			if s, ok := linkTicket.Frontmatter["status"].(string); ok {
				linkStatus = s
			}
			linked = append(linked, struct {
				id     string
				status string
				title  string
			}{link, linkStatus, linkTicket.Title})
		}
	}

	lines := strings.Split(targetTicket.FullContent, "\n")
	inFrontmatter := false

	for _, line := range lines {
		if strings.TrimSpace(line) == "---" {
			if !inFrontmatter {
				inFrontmatter = true
				fmt.Println(line)
			} else {
				inFrontmatter = false
				fmt.Println(line)
			}
			continue
		}

		if inFrontmatter {
			if strings.HasPrefix(line, "parent:") && targetParent != "" {
				if parentTicket, ok := allTickets[targetParent]; ok {
					fmt.Printf("%s  # %s\n", line, parentTicket.Title)
				} else {
					fmt.Println(line)
				}
			} else {
				fmt.Println(line)
			}
		} else {
			fmt.Println(line)
		}
	}

	if len(blockers) > 0 {
		fmt.Println()
		fmt.Println("## Blockers")
		fmt.Println()
		for _, b := range blockers {
			fmt.Printf("- %s [%s] %s\n", b.id, b.status, b.title)
		}
	}

	if len(blocking) > 0 {
		fmt.Println()
		fmt.Println("## Blocking")
		fmt.Println()
		for _, b := range blocking {
			fmt.Printf("- %s [%s] %s\n", b.id, b.status, b.title)
		}
	}

	if len(children) > 0 {
		fmt.Println()
		fmt.Println("## Children")
		fmt.Println()
		for _, c := range children {
			fmt.Printf("- %s [%s] %s\n", c.id, c.status, c.title)
		}
	}

	if len(linked) > 0 {
		fmt.Println()
		fmt.Println("## Linked")
		fmt.Println()
		for _, l := range linked {
			fmt.Printf("- %s [%s] %s\n", l.id, l.status, l.title)
		}
	}

	return 0
}

func updateYAMLField(filePath, field, value string) error {
	content, err := os.ReadFile(filePath)
	if err != nil {
		return err
	}

	lines := strings.Split(string(content), "\n")
	updated := false
	inFrontmatter := false
	var resultLines []string

	for _, line := range lines {
		if strings.TrimSpace(line) == "---" {
			resultLines = append(resultLines, line)
			if !inFrontmatter {
				inFrontmatter = true
			} else {
				inFrontmatter = false
			}
			continue
		}

		if inFrontmatter && strings.HasPrefix(line, field+":") {
			resultLines = append(resultLines, fmt.Sprintf("%s: %s", field, value))
			updated = true
		} else {
			resultLines = append(resultLines, line)
		}
	}

	if !updated {
		var newLines []string
		firstMarkerFound := false
		for _, line := range resultLines {
			newLines = append(newLines, line)
			if !firstMarkerFound && strings.TrimSpace(line) == "---" {
				firstMarkerFound = true
				newLines = append(newLines, fmt.Sprintf("%s: %s", field, value))
			}
		}
		resultLines = newLines
	}

	return os.WriteFile(filePath, []byte(strings.Join(resultLines, "\n")), 0644)
}

func cmdStatus(args []string) int {
	validStatuses := []string{"open", "in_progress", "closed"}

	if len(args) < 2 {
		fmt.Fprintln(os.Stderr, "Usage: ticket status <id> <status>")
		fmt.Fprintf(os.Stderr, "Valid statuses: %s\n", strings.Join(validStatuses, " "))
		return 1
	}

	ticketID := args[0]
	status := args[1]

	isValid := false
	for _, validStatus := range validStatuses {
		if status == validStatus {
			isValid = true
			break
		}
	}

	if !isValid {
		fmt.Fprintf(os.Stderr, "Error: invalid status '%s'. Must be one of: %s\n", status, strings.Join(validStatuses, " "))
		return 1
	}

	filePath, err := resolveTicketID(ticketID)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		return 1
	}

	targetID := strings.TrimSuffix(filepath.Base(filePath), ".md")

	if err := updateYAMLField(filePath, "status", status); err != nil {
		fmt.Fprintf(os.Stderr, "Error updating ticket: %v\n", err)
		return 1
	}

	fmt.Printf("Updated %s -> %s\n", targetID, status)
	return 0
}

func cmdStart(args []string) int {
	if len(args) == 0 {
		fmt.Fprintln(os.Stderr, "Usage: ticket start <id>")
		return 1
	}

	return cmdStatus([]string{args[0], "in_progress"})
}

func cmdClose(args []string) int {
	if len(args) == 0 {
		fmt.Fprintln(os.Stderr, "Usage: ticket close <id>")
		return 1
	}

	return cmdStatus([]string{args[0], "closed"})
}

func cmdReopen(args []string) int {
	if len(args) == 0 {
		fmt.Fprintln(os.Stderr, "Usage: ticket reopen <id>")
		return 1
	}

	return cmdStatus([]string{args[0], "open"})
}

func cmdEdit(args []string) int {
	if len(args) == 0 {
		fmt.Fprintln(os.Stderr, "Usage: ticket edit <id>")
		return 1
	}

	ticketID := args[0]

	filePath, err := resolveTicketID(ticketID)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		return 1
	}

	// Check if stdin and stdout are both TTY
	stdinStat, _ := os.Stdin.Stat()
	stdoutStat, _ := os.Stdout.Stat()
	isTTY := (stdinStat.Mode()&os.ModeCharDevice) != 0 && (stdoutStat.Mode()&os.ModeCharDevice) != 0

	if isTTY {
		// Open in editor
		editor := os.Getenv("EDITOR")
		if editor == "" {
			editor = "vi"
		}
		cmd := exec.Command(editor, filePath)
		cmd.Stdin = os.Stdin
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			return 1
		}
	} else {
		// Non-TTY mode: just print the file path
		fmt.Printf("Edit ticket file: %s\n", filePath)
	}

	return 0
}
