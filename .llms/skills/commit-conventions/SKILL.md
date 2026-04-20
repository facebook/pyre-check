---
name: commit-conventions
description: Use when creating commits or diffs to apply the correct title prefix, reviewer, and test plan based on the area of the codebase being changed.
oncalls: [pyre, pysa]
---

# Commit Conventions

When creating commits:

- If the change is Pysa related, add `[pysa]` as a prefix in the commit title. Otherwise, add `[pyre]` as a prefix in the commit title.
- If the change is Pysa related, add `#pysa_swe` as a reviewer. Otherwise, add `#pyre` as a reviewer.
- By default, add `make test` in the test plan.
