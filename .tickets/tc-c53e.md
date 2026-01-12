---
id: tc-c53e
status: open
deps: [tc-1609]
links: []
created: 2026-01-12T03:22:26Z
type: task
priority: 2
assignee: Ray Myers
---
# Lint / checks in C port

Add linting, formatting, and testing infrastructure for C port.

Tools to configure:
  - Linting: clang-tidy with checks for memory leaks, buffer overflows, style violations
  - Formatting: clang-format with consistent style
  - Static analysis: cppcheck for additional checks
  - Testing: Configure test framework (Check/CUnit)

Makefile targets to add to root Makefile:
.PHONY: c c-lint c-format c-check-format c-static c-test c-check

c: c-check

c-check: c-lint c-static c-test

c-lint:
@echo "Running clang-tidy..."
cd c/ticket && clang-tidy src/*.c -- -Iinclude

c-format:
@echo "Running clang-format..."
cd c/ticket && clang-format -i src/*.c include/*.h

c-check-format:
@echo "Checking formatting..."
cd c/ticket && clang-format --dry-run --Werror src/*.c include/*.h

c-static:
@echo "Running static analysis..."
cd c/ticket && cppcheck --enable=all --error-exitcode=1 src/

c-test:
@echo "Running unit tests..."
cd c/ticket && make test

