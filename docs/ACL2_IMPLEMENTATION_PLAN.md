# ACL2 Implementation Plan - First Feature (Ticket Creation)

This document outlines the plan for implementing the ticket-cli `create` command in ACL2.

## Overview

ACL2 (A Computational Logic for Applicative Common Lisp) is both a programming language and a theorem prover. While ACL2 is primarily used for formal verification, its underlying language is a subset of Common Lisp, making it suitable for implementing practical applications.

## Implementation Strategy

Given ACL2's nature as a subset of Common Lisp, we have two approaches:

### Option A: Pure ACL2 (Recommended for first feature)
- Use ACL2's executable subset of Common Lisp
- Limited I/O capabilities but more aligned with ACL2's design
- Can leverage ACL2 books for additional functionality

### Option B: Common Lisp with ACL2 compatibility
- Use SBCL or another Common Lisp implementation
- Write code compatible with ACL2's subset
- More practical for CLI applications with file I/O

**Decision**: Start with Option B (Common Lisp), ensuring code stays within ACL2-compatible subset where possible. This allows practical file operations while maintaining ACL2 spirit.

## First Feature: `ticket create`

### Required Functions

1. **ID Generation** (`generate-id`)
   - Extract project prefix from current directory name
   - Generate 4-char SHA256 hash from PID + timestamp
   - Return formatted ID: `{prefix}-{hash}`

2. **Directory Utilities**
   - `ensure-tickets-dir` - Create `.tickets/` if missing
   - `get-current-dir-name` - Get working directory name

3. **Timestamp Generation** (`iso-date`)
   - Return UTC timestamp in ISO 8601 format: `YYYY-MM-DDTHH:MM:SSZ`

4. **Argument Parsing** (`parse-create-args`)
   - Handle positional title argument
   - Parse flags: `-d`, `--description`, `--design`, `--acceptance`, `-t`, `-p`, `-a`, `--external-ref`, `--parent`

5. **YAML Frontmatter Generation** (`generate-frontmatter`)
   - Build YAML frontmatter with required fields:
     - id, status, deps, links, created, type, priority
   - Add optional fields: assignee, external-ref, parent

6. **File Writing** (`write-ticket-file`)
   - Combine frontmatter and body content
   - Write to `.tickets/{id}.md`

7. **Main Command Handler** (`cmd-create`)
   - Orchestrate all above functions
   - Print generated ID to stdout

### Dependencies

- Common Lisp implementation: SBCL (preferred) or CCL
- SHA256: Either `ironclad` library or external command (`sha256sum`/`shasum`)
- File I/O: Standard Common Lisp functions

### File Structure

```
acl2/
├── ticket/
│   ├── ticket.asd          # ASDF system definition
│   ├── package.lisp        # Package definition
│   ├── utils.lisp          # Utility functions (hash, date, file ops)
│   ├── create.lisp         # Create command implementation
│   ├── cli.lisp            # Main CLI entry point
│   └── tests/
│       └── test-create.lisp
├── bdd.sh                  # BDD test runner
acl2_ticket.sh              # Wrapper script at project root
```

### Implementation Order

1. **Phase 1: Project Setup**
   - Create directory structure
   - Set up ASDF system definition
   - Create package definition
   - Create wrapper script

2. **Phase 2: Core Utilities**
   - Implement SHA256 hashing
   - Implement timestamp generation
   - Implement directory operations
   - Implement ID generation

3. **Phase 3: Create Command**
   - Implement argument parsing
   - Implement YAML frontmatter generation
   - Implement file writing
   - Implement main create handler

4. **Phase 4: CLI Integration**
   - Implement main entry point
   - Handle command dispatch
   - Build executable or script runner

5. **Phase 5: Testing**
   - Run BDD tests (ticket_creation.feature)
   - Fix any issues

## Code Samples

### ID Generation (Lisp)

```lisp
(defun get-directory-prefix ()
  "Extract prefix from current directory name."
  (let* ((dir-name (car (last (pathname-directory (truename ".")))))
         (segments (split-string dir-name #\-)))
    (if (and segments (> (length segments) 1))
        (coerce (mapcar #'(lambda (s) (char s 0)) segments) 'string)
        (subseq dir-name 0 (min 3 (length dir-name))))))

(defun generate-id ()
  "Generate ticket ID from directory prefix + timestamp hash."
  (let* ((prefix (get-directory-prefix))
         (pid (write-to-string (sb-posix:getpid)))
         (timestamp (write-to-string (get-universal-time)))
         (entropy (concatenate 'string pid timestamp))
         (hash (subseq (sha256-hex entropy) 0 4)))
    (format nil "~A-~A" prefix hash)))
```

### Ticket Creation (Lisp)

```lisp
(defun write-ticket (id title &key description design acceptance
                               (priority 2) (ticket-type "task")
                               assignee external-ref parent)
  "Create a new ticket file with the given parameters."
  (let ((path (format nil ".tickets/~A.md" id))
        (timestamp (iso-timestamp)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output
                             :if-exists :supersede)
      (format out "---~%")
      (format out "id: ~A~%" id)
      (format out "status: open~%")
      (format out "deps: []~%")
      (format out "links: []~%")
      (format out "created: ~A~%" timestamp)
      (format out "type: ~A~%" ticket-type)
      (format out "priority: ~A~%" priority)
      (when assignee (format out "assignee: ~A~%" assignee))
      (when external-ref (format out "external-ref: ~A~%" external-ref))
      (when parent (format out "parent: ~A~%" parent))
      (format out "---~%")
      (format out "# ~A~%~%" title)
      (when description (format out "~A~%~%" description))
      (when design
        (format out "## Design~%~%~A~%~%" design))
      (when acceptance
        (format out "## Acceptance Criteria~%~%~A~%~%" acceptance)))
    id))
```

## Risks and Mitigations

| Risk | Mitigation |
|------|------------|
| SHA256 not available in pure ACL2 | Use external command or ironclad library |
| File I/O complexity | Use standard CL file operations, well-tested |
| Argument parsing | Follow established patterns from other implementations |
| Time/date formatting | Use CL universal time and manual formatting |

## Success Criteria

1. `./acl2_ticket.sh create "Test ticket"` creates a valid ticket file
2. Generated ID matches expected format (`{prefix}-{4-char-hash}`)
3. YAML frontmatter contains all required fields with correct defaults
4. Passes all scenarios in `features/ticket_creation.feature`

## Next Steps After First Feature

After completing `create`, the next features to implement would be:
1. `show` - Display ticket contents
2. `status` - Update ticket status (open/in_progress/closed)
3. `ls` - List all tickets

These can be tracked as separate tickets using the ticket system itself.
