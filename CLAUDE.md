# Claude Session Guide

## Situation

You are implementing h4zel-mini from a specification. Most of the implementation code has been intentionally deleted. The specification in `docs/HAZEL.md` is the authoritative design document — implement from it.

## First Steps

1. **Read `docs/HAZEL.md`** — the complete specification: vision, formal foundations, grove CRDT, forest decomposition, type system with binding resolution, incremental type checking, cursor, projectors, navigation, editing actions, Automerge collaboration, rendering. Appendix A contains reusable Automerge integration code.
2. **Check what already exists** — infrastructure files (Automerge.tsx, main.tsx, RustTypes.tsx, build configs) are preserved. All core implementation (grove, forest, blossom, types, lang, controller, render) needs to be written.

## What Exists

| File | Status | Notes |
|------|--------|-------|
| `docs/HAZEL.md` | Complete spec | Read this first |
| `docs/*.pdf` | Research papers | Referenced in spec |
| `ts/src/Automerge.tsx` | Reuse as-is | Automerge ↔ Grove patch bridge |
| `ts/src/main.tsx` | Reuse as-is | Bootstrap, imports App (which you create) |
| `ts/src/RustTypes.tsx` | Reuse as-is | TypeScript types mirroring Rust serde format |
| `ts/package.json` | Reuse | Dependencies for Automerge, React, Vite |
| `ts/vite.config.ts` | Reuse | Vite + WASM plugin config |
| `ts/*.config.*` | Reuse | TypeScript, ESLint, Vitest configs |
| `ts/index.html` | Reuse | Entry point |
| `rust/Cargo.toml` | Reuse | Rust/WASM dependencies |
| Everything else | Needs implementation | Create from spec |

## Architecture Decision

The spec describes a two-runtime design (Rust/WASM + TypeScript). You may choose to restructure the boundary between them. The spec notes that TypeScript currently creates all patches while Rust only applies them — you may move patch creation logic to Rust if that produces a cleaner design.

## Where to Write Things

| Information type | Location |
|------------------|----------|
| Design changes, spec updates | `docs/HAZEL.md` |
| Task tracking | `docs/TODO.md` (create as needed) |
