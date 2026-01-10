# Ticket CLI - TypeScript Implementation

TypeScript implementation of the ticket CLI using Bun.

## Setup

Install Bun if you haven't already:
```bash
curl -fsSL https://bun.sh/install | bash
```

Install dependencies:
```bash
bun install
```

## Usage

Run the CLI directly:
```bash
bun run src/cli.ts --help
```

Or use the wrapper script from the project root:
```bash
./ts_ticket.sh --help
```

## Development

Run tests:
```bash
bun test
```

Type checking:
```bash
bun run lint
```

## Project Structure

- `src/` - Source code
  - `cli.ts` - Main CLI entry point
- `tests/` - Unit tests
  - `cli.test.ts` - CLI tests
- `package.json` - Package metadata and scripts
- `tsconfig.json` - TypeScript configuration

## Status

Work in progress. Currently implements basic help command. Full implementation coming soon.
