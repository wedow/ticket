import { describe, test, expect } from "bun:test";
import { spawnSync } from "child_process";
import { join } from "path";

const CLI_PATH = join(import.meta.dir, "..", "src", "cli.ts");

describe("CLI", () => {
  test("shows help with --help flag", () => {
    const result = spawnSync("bun", ["run", CLI_PATH, "--help"], {
      encoding: "utf-8",
    });

    expect(result.status).toBe(0);
    expect(result.stdout).toContain("tk - minimal ticket system");
    expect(result.stdout).toContain("Commands:");
  });

  test("shows help with no arguments", () => {
    const result = spawnSync("bun", ["run", CLI_PATH], {
      encoding: "utf-8",
    });

    expect(result.status).toBe(0);
    expect(result.stdout).toContain("tk - minimal ticket system");
  });

  test("shows work in progress message for unimplemented commands", () => {
    const result = spawnSync("bun", ["run", CLI_PATH, "create", "Test"], {
      encoding: "utf-8",
    });

    expect(result.status).toBe(1);
    expect(result.stdout).toContain("work in progress");
    expect(result.stdout).toContain("not yet implemented");
  });
});
