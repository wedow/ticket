.PHONY: python python-lint python-format python-type python-test python-check
.PHONY: go go-lint go-type go-fmt go-test go-bdd go-check
.PHONY: zig zig-lint zig-type zig-test zig-bdd zig-check
.PHONY: typescript typescript-lint typescript-type typescript-test typescript-bdd typescript-check
.PHONY: c c-build c-test c-check c-lint c-format c-clean c-bdd
.PHONY: acl2 acl2-check acl2-lint acl2-test acl2-bdd

PYTHON_DIR := python/ticket
GO_DIR := go/ticket
ZIG_DIR := zig/ticket
TYPESCRIPT_DIR := typescript/ticket
C_DIR := c/ticket
ACL2_DIR := acl2/ticket

python: python-check

python-check: python-lint python-type python-test

python-lint:
	@echo "Running ruff linter..."
	cd $(PYTHON_DIR) && uv run ruff check src tests

python-format:
	@echo "Running ruff formatter..."
	cd $(PYTHON_DIR) && uv run ruff format src tests

python-format-check:
	@echo "Checking ruff formatting..."
	cd $(PYTHON_DIR) && uv run ruff format --check src tests

python-type:
	@echo "Running mypy type checker..."
	cd $(PYTHON_DIR) && uv run mypy src tests

python-test:
	@echo "Running pytest..."
	cd $(PYTHON_DIR) && uv run pytest

go: go-check

go-check: go-lint go-type go-test

go-lint:
	@echo "Running golangci-lint..."
	cd $(GO_DIR) && golangci-lint run ./...

go-type:
	@echo "Running go vet..."
	cd $(GO_DIR) && go vet ./...

go-fmt:
	@echo "Running gofmt and goimports..."
	cd $(GO_DIR) && gofmt -w . && goimports -w .

go-test:
	@echo "Running go test..."
	cd $(GO_DIR) && go test ./...

go-bdd:
	@echo "Running Go BDD tests..."
	./go/bdd.sh

zig: zig-check

zig-check: zig-lint zig-type zig-test

zig-lint:
	@echo "Running Zig formatter check..."
	cd $(ZIG_DIR) && zig fmt --check src/

zig-type:
	@echo "Running Zig type checker..."
	cd $(ZIG_DIR) && zig build

zig-test:
	@echo "Running Zig tests..."
	cd $(ZIG_DIR) && zig build test

zig-bdd:
	@echo "Running Zig BDD tests..."
	bash zig/bdd.sh

typescript: typescript-check

typescript-check: typescript-lint typescript-type typescript-test

typescript-lint:
	@echo "Running TypeScript linter..."
	cd $(TYPESCRIPT_DIR) && bun run lint

typescript-type:
	@echo "Running TypeScript type checker..."
	cd $(TYPESCRIPT_DIR) && bun run typecheck

typescript-test:
	@echo "Running TypeScript tests..."
	cd $(TYPESCRIPT_DIR) && bun test

typescript-bdd:
	@echo "Running TypeScript BDD tests..."
	bash typescript/bdd.sh

c: c-check

c-check: c-lint c-build c-test

c-build:
	@echo "Building C implementation..."
	cd $(C_DIR) && $(MAKE)

c-test:
	@echo "Running C tests..."
	cd $(C_DIR) && $(MAKE) test

c-lint:
	@echo "Checking C code formatting..."
	cd $(C_DIR) && $(MAKE) lint

c-format:
	@echo "Formatting C code..."
	cd $(C_DIR) && $(MAKE) format

c-clean:
	@echo "Cleaning C build artifacts..."
	cd $(C_DIR) && $(MAKE) clean

c-bdd:
	@echo "Running C BDD tests..."
	bash c/bdd.sh

acl2: acl2-check

acl2-check: acl2-lint acl2-test

acl2-lint:
	@echo "Checking ACL2/Lisp syntax..."
	@command -v sbcl >/dev/null 2>&1 || { echo "Error: sbcl not installed"; exit 1; }
	cd $(ACL2_DIR) && sbcl --noinform --non-interactive \
		--load src/package.lisp \
		--load src/utils.lisp \
		--load src/create.lisp \
		--load src/cli.lisp \
		--eval "(quit)"

acl2-test:
	@echo "Running ACL2/Lisp tests..."
	@echo "Note: No unit tests implemented yet"

acl2-bdd:
	@echo "Running ACL2 BDD tests..."
	bash acl2/bdd.sh
