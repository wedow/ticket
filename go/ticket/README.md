# Ticket - Go Port

Go port of the git-backed issue tracker for AI agents.

## Installation

```bash
# Build the binary
go build ./cmd/ticket

# Install to $GOPATH/bin
go install ./cmd/ticket
```

## Usage

```bash
./bin/ticket --help
```

## Development

This project uses Go modules for dependency management.

```bash
# Run tests
go test ./...

# Format code
go fmt ./...

# Lint code (requires golangci-lint)
golangci-lint run

# Build the binary
go build -o bin/ticket ./cmd/ticket
```

## License

MIT
