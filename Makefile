.PHONY: python python-lint python-format python-type python-test python-check
.PHONY: go go-lint go-type go-fmt go-test go-bdd go-check
.PHONY: zig zig-lint zig-type zig-test zig-check

PYTHON_DIR := python/ticket
GO_DIR := go/ticket
ZIG_DIR := zig/ticket

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
