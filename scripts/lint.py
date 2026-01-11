#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.12"
# dependencies = [
#   "ruff>=0.14.11",
#   "mypy>=1.14.1",
# ]
# ///
import re
import subprocess
import sys
from pathlib import Path


def parse_script_metadata(script_path: Path) -> list[str]:
    """Parse the inline script metadata to extract dependencies."""
    content = script_path.read_text()
    
    # Look for the /// script ... /// block
    match = re.search(r'# /// script\n(.*?)# ///', content, re.DOTALL)
    if not match:
        return []
    
    metadata_block = match.group(1)
    
    # Extract dependencies list
    deps: list[str] = []
    in_deps = False
    for line in metadata_block.split('\n'):
        line = line.strip()
        if 'dependencies = [' in line:
            in_deps = True
            # Check if it's a one-liner
            if ']' in line:
                break
            continue
        if in_deps:
            if ']' in line:
                break
            # Extract dependency from line like '#   "package>=version",'
            dep_match = re.search(r'"([^"]+)"', line)
            if dep_match:
                deps.append(dep_match.group(1))
    
    return deps


def run_ruff(script_path: Path) -> bool:
    """Run ruff on the target script."""
    print(f"🔍 Running ruff on {script_path}")
    result = subprocess.run(
        ['ruff', 'check', str(script_path)],
        capture_output=False
    )
    return result.returncode == 0


def run_mypy(script_path: Path, dependencies: list[str]) -> bool:
    """Run mypy on the target script with its dependencies."""
    print(f"🔍 Running mypy on {script_path}")
    
    # Build the command with dependencies
    cmd = ['uv', 'run']
    for dep in dependencies:
        cmd.extend(['--with', dep])
    cmd.extend(['mypy', str(script_path)])
    
    result = subprocess.run(cmd, capture_output=False)
    return result.returncode == 0


def main() -> None:
    if len(sys.argv) != 2:
        print("Usage: ./scripts/lint.py <script_path>")
        sys.exit(1)
    
    script_path = Path(sys.argv[1])
    if not script_path.exists():
        print(f"Error: Script not found: {script_path}")
        sys.exit(1)
    
    print(f"📝 Linting {script_path}")
    
    # Parse dependencies from the target script
    dependencies = parse_script_metadata(script_path)
    if dependencies:
        print(f"📦 Found dependencies: {', '.join(dependencies)}")
    else:
        print("📦 No dependencies found")
    
    # Run ruff
    ruff_ok = run_ruff(script_path)
    
    # Run mypy
    mypy_ok = run_mypy(script_path, dependencies)
    
    # Report results
    print("\n" + "=" * 60)
    if ruff_ok and mypy_ok:
        print("✅ All checks passed!")
        sys.exit(0)
    else:
        if not ruff_ok:
            print("❌ Ruff found issues")
        if not mypy_ok:
            print("❌ Mypy found issues")
        sys.exit(1)


if __name__ == "__main__":
    main()
