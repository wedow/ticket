---
id: tc-f25c
status: closed
deps: []
links: []
created: 2026-01-09T14:08:22Z
type: task
priority: 2
assignee: Ray Myers
---
# Document process for porting to a new language

Create docs/NEW_IMPLEMENTATION.md to capture the process for porting to a new language. Look at these tickets for and example. Make the doc self-contained based on that.

* tc-fa9d  [closed] - Setup project for python port
* tc-e639  [closed] - Lint / checks in python port <- [tc-fa9d]
* tc-9c9c  [closed] - Scope out BDD Python
* tc-32f5  [closed] - Spec review vs python implementation

The instructions should involve actually creating tickets (with the `./ticket` command) with the instructions relevant to the new language / platform.

The needed tickets are roughly as before except we need another similar to

* Add full BDD suite to Python build
  * Updates bdd.sh to run full suite and call it from root Makefile
  * This ticket should depend on all created feature tickets.

Also "Spec review vs python implementation" should depend on this full suite ticket, and a "Manual smoke test of Python port" to actually use it in a fresh dir on some basic use cases confirm we have a working app.

Python is the example here, the documented pattern should not be python specific.

## Notes

**2026-01-09T14:22:00Z**

Created docs/NEW_IMPLEMENTATION.md with comprehensive guide for porting ticket-cli to new languages. Document is self-contained and based on Python port experience (tc-fa9d, tc-e639, tc-9c9c, tc-32f5). Includes step-by-step process for: 1) Creating tracking tickets, 2) Setup & dev tools, 3) BDD feature ports, 4) Full suite integration, 5) Manual smoke testing, 6) Spec review. Provides language-agnostic instructions with Python port as reference example. Covers key files to create (wrapper script, bdd.sh, Makefile targets), best practices, common pitfalls, and quality checklist.
