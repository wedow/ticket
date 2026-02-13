#!/usr/bin/env bash
# Comprehensive test suite for tk ticket system
# Creates test tickets, exercises all features, then cleans up

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

PASS=0
FAIL=0
TEST_IDS=()

log_pass() {
    echo -e "${GREEN}✓${NC} $1"
    PASS=$((PASS + 1))
}

log_fail() {
    echo -e "${RED}✗${NC} $1"
    FAIL=$((FAIL + 1))
}

log_section() {
    echo -e "\n${YELLOW}=== $1 ===${NC}"
}

# Track test ticket IDs for cleanup
track_id() {
    TEST_IDS+=("$1")
}

# Extract ID from tk create output
extract_id() {
    grep "^id:" | awk '{print $2}'
}

# Assert command succeeds
assert_ok() {
    if eval "$1" > /dev/null 2>&1; then
        log_pass "$2"
    else
        log_fail "$2"
    fi
}

# Assert command fails
assert_fail() {
    if eval "$1" > /dev/null 2>&1; then
        log_fail "$2 (should have failed)"
    else
        log_pass "$2"
    fi
}

# Assert output contains string
assert_contains() {
    local output
    output=$(eval "$1" 2>&1) || true
    if echo "$output" | grep -q "$2"; then
        log_pass "$3"
    else
        log_fail "$3 (expected '$2' in output)"
        echo "  Got: $output"
    fi
}

# Assert output does NOT contain string
assert_not_contains() {
    local output
    output=$(eval "$1" 2>&1) || true
    if echo "$output" | grep -q "$2"; then
        log_fail "$3 (unexpected '$2' in output)"
    else
        log_pass "$3"
    fi
}

cleanup() {
    log_section "CLEANUP"
    for id in "${TEST_IDS[@]}"; do
        tk delete "$id" 2>/dev/null || true
    done
    echo "Deleted ${#TEST_IDS[@]} test tickets"
}

trap cleanup EXIT

# ============================================================================
log_section "TICKET CREATION"
# ============================================================================

# Basic create
ID1=$(tk create "Test Basic Ticket" | extract_id)
track_id "$ID1"
if [[ -n "$ID1" ]]; then
    log_pass "Create basic ticket: $ID1"
else
    log_fail "Create basic ticket"
fi

# Create with description
ID2=$(tk create "Test With Description" -d "This is a description" | extract_id)
track_id "$ID2"
assert_contains "tk show $ID2" "This is a description" "Create with description"

# Create with all options
ID3=$(tk create "Test Full Options" \
    -d "Full description" \
    --design "Design notes here" \
    --acceptance "AC: it works" \
    -t epic \
    -p 1 \
    -a "Tester" \
    --external-ref "GH-123" \
    --tags "test,automated" | extract_id)
track_id "$ID3"
assert_contains "tk show $ID3" "type: epic" "Create with type"
assert_contains "tk show $ID3" "priority: 1" "Create with priority"
assert_contains "tk show $ID3" "assignee: Tester" "Create with assignee"
assert_contains "tk show $ID3" "external-ref: GH-123" "Create with external-ref"
assert_contains "tk show $ID3" "tags:" "Create with tags"
assert_contains "tk show $ID3" "## Design" "Create with design section"
assert_contains "tk show $ID3" "## Acceptance Criteria" "Create with acceptance section"

# Create different types
for type in task feature bug chore; do
    ID=$(tk create "Test $type type" -t "$type" | extract_id)
    track_id "$ID"
    assert_contains "tk show $ID" "type: $type" "Create type: $type"
done

# Create with parent
ID_CHILD=$(tk create "Test Child Ticket" --parent "$ID3" | extract_id)
track_id "$ID_CHILD"
assert_contains "tk show $ID_CHILD" "parent: $ID3" "Create with parent"
assert_contains "tk show $ID3" "$ID_CHILD" "Parent shows child"

# ============================================================================
log_section "TICKET EDITING"
# ============================================================================

# Edit title
assert_ok "tk edit $ID1 --title 'Updated Title'" "Edit title"
assert_contains "tk show $ID1" "# Updated Title" "Title was updated"

# Edit description
assert_ok "tk edit $ID1 -d 'New description text'" "Edit description"
assert_contains "tk show $ID1" "New description text" "Description was updated"

# Edit design
assert_ok "tk edit $ID1 --design 'New design notes'" "Edit design"
assert_contains "tk show $ID1" "## Design" "Design section added"

# Edit acceptance
assert_ok "tk edit $ID1 --acceptance 'New AC'" "Edit acceptance"
assert_contains "tk show $ID1" "## Acceptance Criteria" "AC section added"

# Edit status
assert_ok "tk edit $ID1 -s in_progress" "Edit status to in_progress"
assert_contains "tk show $ID1" "status: in_progress" "Status is in_progress"

assert_ok "tk edit $ID1 -s needs_testing" "Edit status to needs_testing"
assert_contains "tk show $ID1" "status: needs_testing" "Status is needs_testing"

assert_ok "tk edit $ID1 -s closed" "Edit status to closed"
assert_contains "tk show $ID1" "status: closed" "Status is closed"

assert_ok "tk edit $ID1 -s open" "Edit status to open (reopen)"
assert_contains "tk show $ID1" "status: open" "Status is open"

# Edit type
assert_ok "tk edit $ID1 -t bug" "Edit type"
assert_contains "tk show $ID1" "type: bug" "Type was updated"

# Edit priority
assert_ok "tk edit $ID1 -p 0" "Edit priority"
assert_contains "tk show $ID1" "priority: 0" "Priority was updated"

# Edit assignee
assert_ok "tk edit $ID1 -a 'New Assignee'" "Edit assignee"
assert_contains "tk show $ID1" "assignee: New Assignee" "Assignee was updated"

# Edit tags
assert_ok "tk edit $ID1 --tags 'new,tags,here'" "Edit tags"
assert_contains "tk show $ID1" "tags:" "Tags were updated"

# Invalid status should fail
assert_fail "tk edit $ID1 -s invalid_status" "Reject invalid status"

# Invalid priority should fail
assert_fail "tk edit $ID1 -p 99" "Reject invalid priority"

# ============================================================================
log_section "DEPENDENCIES"
# ============================================================================

# Create tickets for dependency testing
DEP1=$(tk create "Dep Test 1" | extract_id)
track_id "$DEP1"
DEP2=$(tk create "Dep Test 2" | extract_id)
track_id "$DEP2"
DEP3=$(tk create "Dep Test 3" | extract_id)
track_id "$DEP3"

# Add dependency
assert_ok "tk dep $DEP2 $DEP1" "Add dependency"
assert_contains "tk show $DEP2" "deps: \[$DEP1\]" "Dependency recorded"

# Ticket with unresolved dep should be blocked
assert_contains "tk blocked" "$DEP2" "Blocked ticket appears in blocked list"
assert_not_contains "tk ready" "$DEP2" "Blocked ticket not in ready list"

# Close dependency
tk edit "$DEP1" -s closed
assert_contains "tk ready --open" "$DEP2" "Ticket ready after dep closed"

# Remove dependency
tk edit "$DEP1" -s open
assert_ok "tk undep $DEP2 $DEP1" "Remove dependency"
assert_contains "tk show $DEP2" "deps: \[\]" "Dependency removed"

# Dependency tree
tk dep "$DEP2" "$DEP1"
tk dep "$DEP3" "$DEP2"
assert_contains "tk dep tree $DEP3" "$DEP2" "Dep tree shows dependencies"
assert_contains "tk dep tree $DEP3" "$DEP1" "Dep tree shows transitive deps"

# Cycle detection - create a cycle
tk dep "$DEP1" "$DEP3"
assert_contains "tk dep cycle" "Cycle" "Cycle detection finds cycles"
tk undep "$DEP1" "$DEP3"  # Remove cycle

# ============================================================================
log_section "LINKS"
# ============================================================================

LINK1=$(tk create "Link Test 1" | extract_id)
track_id "$LINK1"
LINK2=$(tk create "Link Test 2" | extract_id)
track_id "$LINK2"

# Add link
assert_ok "tk link $LINK1 $LINK2" "Add link"
assert_contains "tk show $LINK1" "$LINK2" "Link appears in first ticket"
assert_contains "tk show $LINK2" "$LINK1" "Link is symmetric"

# Remove link
assert_ok "tk unlink $LINK1 $LINK2" "Remove link"
assert_not_contains "tk show $LINK1" "links: \[$LINK2\]" "Link removed from first"

# ============================================================================
log_section "LISTING AND FILTERING"
# ============================================================================

# Create tickets for filter testing
FILTER1=$(tk create "Filter Test Alpha" -t task -p 1 -a "Alice" --tags "frontend" | extract_id)
track_id "$FILTER1"
FILTER2=$(tk create "Filter Test Beta" -t bug -p 2 -a "Bob" --tags "backend" | extract_id)
track_id "$FILTER2"
tk edit "$FILTER2" -s closed

# Basic ls
assert_contains "tk ls" "$FILTER1" "ls shows open tickets"

# Filter by status
assert_contains "tk ls --status=open" "$FILTER1" "Filter by status=open"
assert_not_contains "tk ls --status=open" "$FILTER2" "Filter excludes closed"
assert_contains "tk ls --status=closed" "$FILTER2" "Filter by status=closed"

# Filter by assignee
assert_contains "tk ls -a Alice" "$FILTER1" "Filter by assignee"
assert_not_contains "tk ls -a Alice" "$FILTER2" "Assignee filter excludes others"

# Filter by tag
assert_contains "tk ls -T frontend" "$FILTER1" "Filter by tag"
assert_not_contains "tk ls -T frontend" "$FILTER2" "Tag filter excludes others"

# Filter by priority
assert_contains "tk ls -P 1" "$FILTER1" "Filter by priority"
assert_not_contains "tk ls -P 1" "$FILTER2" "Priority filter excludes others"

# Filter by type
assert_contains "tk ls -t task" "$FILTER1" "Filter by type"
assert_not_contains "tk ls -t task" "$FILTER2" "Type filter excludes others"

# Closed list
assert_contains "tk closed" "$FILTER2" "Closed list shows closed tickets"

# ============================================================================
log_section "HIERARCHY GATING"
# ============================================================================

# Create epic and children
EPIC=$(tk create "Test Epic for Gating" -t epic | extract_id)
track_id "$EPIC"
CHILD1=$(tk create "Epic Child 1" --parent "$EPIC" | extract_id)
track_id "$CHILD1"
CHILD2=$(tk create "Epic Child 2" --parent "$EPIC" | extract_id)
track_id "$CHILD2"

# Children should NOT appear in ready when epic is open
assert_not_contains "tk ready" "$CHILD1" "Child hidden when epic is open"

# Children SHOULD appear with --open flag
assert_contains "tk ready --open" "$CHILD1" "Child visible with --open"

# Start epic - children should now be ready
tk edit "$EPIC" -s in_progress
assert_contains "tk ready" "$CHILD1" "Child visible when epic in_progress"
assert_contains "tk ready" "$CHILD2" "Both children visible"

# ============================================================================
log_section "STATUS PROPAGATION"
# ============================================================================

# Use the epic and children from hierarchy gating
# Reset states
tk edit "$EPIC" -s in_progress
tk edit "$CHILD1" -s open
tk edit "$CHILD2" -s open

# Close first child - epic should stay in_progress
tk edit "$CHILD1" -s closed
assert_contains "tk show $EPIC" "status: in_progress" "Epic stays in_progress with open child"

# Set second child to needs_testing - epic should become needs_testing
tk edit "$CHILD2" -s needs_testing
assert_contains "tk show $EPIC" "status: needs_testing" "Epic propagates to needs_testing"

# Close second child - epic should become closed
tk edit "$CHILD2" -s closed
assert_contains "tk show $EPIC" "status: closed" "Epic propagates to closed"

# ============================================================================
log_section "NOTES"
# ============================================================================

NOTE_ID=$(tk create "Note Test Ticket" | extract_id)
track_id "$NOTE_ID"

# Add note
assert_ok "tk add-note $NOTE_ID 'This is a test note'" "Add note"
assert_contains "tk show $NOTE_ID" "This is a test note" "Note appears in ticket"
assert_contains "tk show $NOTE_ID" "## Notes" "Notes section created"

# Add another note
assert_ok "tk add-note $NOTE_ID 'Second note'" "Add second note"
assert_contains "tk show $NOTE_ID" "Second note" "Second note appears"

# ============================================================================
log_section "QUERY (JSON)"
# ============================================================================

# Basic query
assert_ok "tk query" "Query outputs JSON"
assert_contains "tk query" '"id":' "Query contains id field"
assert_contains "tk query" '"status":' "Query contains status field"

# Query with filter (if jq available)
if command -v jq &> /dev/null; then
    assert_contains "tk query 'select(.type==\"epic\")'" "epic" "Query with jq filter"
fi

# ============================================================================
log_section "WORKFLOW COMMAND"
# ============================================================================

assert_contains "tk workflow" "Ticket Workflow Guide" "Workflow outputs guide"
assert_contains "tk workflow" "Ticket Types" "Workflow has types section"
assert_contains "tk workflow" "Statuses" "Workflow has statuses section"
assert_contains "tk workflow" "Readiness Rules" "Workflow has readiness section"
assert_contains "tk workflow" "Status Propagation" "Workflow has propagation section"

# ============================================================================
log_section "HELP"
# ============================================================================

assert_contains "tk help" "Usage:" "Help shows usage"
assert_contains "tk help" "create" "Help shows create command"
assert_contains "tk help" "edit" "Help shows edit command"
assert_contains "tk help" "\-s, \-\-status" "Help shows status flag"
assert_contains "tk --help" "Usage:" "--help flag works"
assert_contains "tk -h" "Usage:" "-h flag works"

# ============================================================================
log_section "ERROR HANDLING"
# ============================================================================

# Unknown command
assert_fail "tk unknown_command" "Reject unknown command"

# Missing required args
assert_fail "tk edit" "Reject edit without id"
assert_fail "tk dep" "Reject dep without args"
assert_fail "tk delete" "Reject delete without id"

# Invalid ticket ID
assert_fail "tk show nonexistent_id_xyz" "Reject nonexistent ticket"

# ============================================================================
log_section "DELETE"
# ============================================================================

DELETE_ID=$(tk create "To Be Deleted" | extract_id)
assert_ok "tk delete $DELETE_ID" "Delete ticket"
assert_fail "tk show $DELETE_ID" "Deleted ticket not found"

# ============================================================================
# RESULTS
# ============================================================================

echo ""
echo "========================================"
echo -e "  ${GREEN}PASSED: $PASS${NC}"
echo -e "  ${RED}FAILED: $FAIL${NC}"
echo "========================================"

if [[ $FAIL -gt 0 ]]; then
    exit 1
fi
