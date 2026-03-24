# Claude Session Guide

## First Steps

1. **Read `docs/HAZEL.md`** — design, architecture, research foundations, implementation details
2. **Read `docs/KNOWN_ISSUES.md`** — pitfalls, gotchas, things that have bitten us before
3. **Check todo list** — active tasks and their status

## Where to Write Things

| Information type | Location |
|------------------|----------|
| Design, specs, architecture, vision, invariants | `docs/HAZEL.md` |
| Pitfalls, gotchas, debugging lessons | `docs/KNOWN_ISSUES.md` |
| Task tracking | `docs/TODO.md` |

## Session Discipline

**Design notes are living documents.** When discussion clarifies or modifies a design, update `docs/HAZEL.md` immediately. The design doc represents current consensus — it must stay accurate.

**Pitfalls accumulate.** When you discover something that could trip up a future session — a subtle bug pattern, a non-obvious invariant, a "this looks wrong but isn't" situation — add it to `docs/KNOWN_ISSUES.md`.

**Todos track work state.** Create todos in `docs/TODO.md` when tasks are identified. Mark them complete when done. The TodoWrite tool is session-scoped and does not persist — always mirror to the file.

**Context is perishable.** If you learn something that would help a future session — a decision rationale, a constraint discovered during implementation, why something is the way it is — write it in the appropriate file before the session ends. Your context will not persist; the files will.
