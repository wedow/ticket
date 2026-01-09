---
id: tc-db24
status: open
deps: []
links: []
created: 2026-01-09T19:42:56Z
type: task
priority: 3
assignee: Ray Myers
---
# Improve Python implementation .gitignore

Enhance Python implementation .gitignore for better coverage of build artifacts and potential developer environment conflicts.

Tasks:
1. Update python/ticket/.gitignore to add:
   - .mypy_cache/
   - .ruff_cache/
   - *.pyc (if not already present)
   
2. Consider whether .python-version should remain tracked:
   - Pro: ensures consistent Python version across team
   - Con: may conflict if developers use different Python versions
   - Recommendation: Keep it tracked since pyproject.toml requires >=3.13
   
3. Ensure all cache directories are properly ignored

Verification:
- Run 'make python-check' to ensure everything still works
- Run 'python/bdd.sh' to verify BDD tests pass
- Run './py_ticket.sh help' to verify wrapper works
- Check 'git status' to confirm no cache files show as untracked

