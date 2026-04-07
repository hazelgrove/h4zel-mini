# Claude Session Guide

## Situation

You are working on h4zel-mini, a structure editor with a CRDT-based collaborative editing backend. The core implementation exists and works — grove, blossom (type checking), controller (cursor/editing), and rendering are all functional with 98 passing tests.

## Current Task

**Read `docs/TODO.md` first.** It contains the implementation plan for the next feature: adding the forest decomposition layer and order maintenance intervals. This is a refactor across the Rust codebase. The TODO has the complete spec for what to build, how each file changes, and the execution order.

The spec in `docs/HAZEL.md` is the authoritative design document for the full system. Sections 4.6–4.8 cover forest decomposition and document-order intervals.

## What Exists

| File | Status | Notes |
|------|--------|-------|
| `docs/HAZEL.md` | Complete spec | Authoritative design document |
| `docs/TODO.md` | Implementation plan | **Read this for current task** |
| `rust/src/grove.rs` | Complete | CRDT graph layer |
| `rust/src/blossom.rs` | Complete, needs refactor | Type checking — currently uses `HashSet` dirty set, needs `PriorityQueue` |
| `rust/src/controller.rs` | Complete | Cursor and editing actions |
| `rust/src/render.rs` | Complete, needs refactor | Currently does ad-hoc cycle detection, should use forest |
| `rust/src/lang.rs` | Complete | Language constructors and sorts |
| `rust/src/types.rs` | Complete | Type representation |
| `rust/src/scenario.rs` | Complete | Single-user test harness |
| `rust/src/sync_scenario.rs` | Complete | Two-user sync test harness |
| `rust/src/lib.rs` | Complete, needs update | WASM entry point — needs Forest added to HazelState |
| `rust/tests/single_user.rs` | 55 tests | Single-user integration tests |
| `rust/tests/conflict.rs` | 25 tests | Multi-user conflict tests |
| `rust/src/order.rs` | **Needs creation** | Order maintenance wrapper |
| `rust/src/forest.rs` | **Needs creation** | Forest decomposition layer |
| `ts/src/App.tsx` | Complete | React UI — canvas projector, structural rendering |
| `ts/src/Automerge.tsx` | Complete | Automerge ↔ Grove patch bridge |
| `rust/Cargo.toml` | Complete | Dependencies include `order-maintenance` and `priority-queue` |

## Architecture

```
Grove (CRDT graph) ← patches from controller
  ↓ dirty Sites
Forest (tree decomposition + intervals) ← parallel state, reads grove
  ↓ dirty Sites with interval priorities
Blossom (incremental type checking) ← priority queue driven by intervals
  ↓ type attributes
Render (tree → JSON) ← uses forest traversal, blossom attributes
```

All four (grove, forest, blossom, controller) are peers owned by `HazelState`. Patches flow: controller → grove → forest → blossom. Rendering reads grove + forest + blossom.

## Rules

- **Never push to git unless explicitly asked.** Commit freely, push only on request.
- **When referencing old code (git history, previous implementations), extract the behavioral requirement, not the structure.** Old code's module boundaries, type wrappers, and file decomposition were often suboptimal. Re-derive the minimal implementation from what the behavior actually needs. Ask: "what does this accomplish?" not "how was it organized?"
- **Plans and TODO docs are starting points, not contracts.** If during implementation you notice a piece is unnecessary, skip or simplify it. Re-evaluate at each step.
- **When you notice a recurring mistake pattern, add a rule here to prevent it in future sessions.** This file is the durable context for all future Claudes on this project.

## Where to Write Things

| Information type | Location |
|------------------|----------|
| Design changes, spec updates | `docs/HAZEL.md` |
| Task tracking | `docs/TODO.md` |
