# Known Issues and Technical Debt

Issues identified during code audit. Severity ratings: **Critical**, **Moderate**, **Minor**.

---

## Critical

### 1. ~~Arity Duplication (DRY Violation)~~ FIXED
**Files**: `rust/src/lang.rs:28-49`, `ts/src/Controller.ts:543-568`

~~Constructor arity is defined in both Rust and TypeScript.~~

**Fixed**: Arity is now exposed from Rust via `arity_of_constructor()` WASM function. TypeScript calls into Rust.

### 2. ~~`Ord` Implementation in `order.rs` is Mathematically Wrong~~ FIXED
**File**: `rust/src/order.rs:12-19`

~~When `partial_cmp` returns `None`, the code returned `Equal`, which is wrong.~~

**Fixed**: Now panics with a descriptive message if `partial_cmp` returns `None` (which should never happen if comparing Orders from the same structure).

### 3. ~~Interval Splitting Bug in `forest.rs`~~ FIXED
**File**: `rust/src/forest.rs:320-330`

~~The code split `i_outer.start` twice independently, giving `p1 = p3` and inverted inner intervals.~~

**Fixed**: Now correctly chains splits: `p1 < p2 < p3 < p4`, giving proper nested intervals.

---

## Moderate

### 4. ~~JSON.stringify for Equality~~ ADDRESSED
**File**: `ts/src/Controller.ts:157-171`

~~Equality checks use `JSON.stringify(a) === JSON.stringify(b)`. This is fragile (key ordering), slow (O(n) serialization), and semantically questionable.~~

**Resolution**: Added documentation explaining why JSON.stringify is acceptable here:
- Rust's serde produces deterministic key ordering
- The structures are small value types (UUIDs, positions, paths)
- Not in hot inner loops - called O(visible nodes) per render
- Added proper types to remove the unsafe `any`-typed `nodesEqual` function

### 5. ~~`any` Types Throughout TypeScript~~ ADDRESSED
**File**: `ts/src/Controller.ts:5-13`, `ts/src/RustTypes.tsx`

~~Core types are `any`: `TermEdge`, `TermNode`, `Patch`, etc.~~

**Resolution**: Added proper TypeScript types mirroring the Rust serde serialization:
- `Node`, `NodeId`, `Edge`, `Location` from grove.rs
- `TermNode`, `TermEdge`, `TermLocation` from forest.rs
- Patch types remain `unknown` (opaque handles created by Rust)

### 6. ~~`.unwrap()` in WASM Boundary~~ ADDRESSED
**File**: `rust/src/wasm-rust.rs:32-37`

~~Malformed JS data causes unhelpful WASM panics.~~

**Resolution**: Added documentation explaining the design decision:
- TypeScript is the authoritative source; malformed data indicates a TS bug
- `console_error_panic_hook` provides clear error messages
- For production, consider returning `Result<JsValue, JsError>` instead

### 7. ~~Projector Navigation is Bolted On~~ ADDRESSED
**File**: `ts/src/Controller.ts:369-476`

~~Extensive special-casing for projectors suggests they don't fit naturally into the navigation model.~~

**Resolution**: Added design documentation explaining the rationale:
- Projectors wrap terms with view metadata (position 0: type, position 1: content)
- Navigation treats projectors as "transparent" - users navigate to content directly
- Alternative designs (polymorphic navigation, metadata on edges) considered but deferred
- Current explicit special-casing is pragmatic for a prototype

### 8. ~~Mutation Hidden in "compute" Methods~~ ADDRESSED
**File**: `ts/src/Controller.ts:251-357`

~~Methods named `computeWrapLeft`, `computeDelete`, etc. mutate `this.cursor` as a side effect.~~

**Resolution**: Added documentation clarifying the intentional design:
- Methods compute patches AND update cursor to reflect post-action position
- Cursor update happens immediately in TypeScript
- Patches are returned to be applied through WASM/Rust layer
- This separation allows cursor handling in TS while patches go through Grove

### 9. ~~Clipboard Holds Stale References~~ NOT AN ISSUE
**File**: `ts/src/Controller.ts:327-357`

**Note from review**: This is actually handled by Grove semantics - the cursor/clipboard remains in the "severed" subterm, which still exists but isn't visible. User can click to return cursor to visible terms.

---

## Minor

### 10. ~~"Blossom" Naming Unexplained~~ DOCUMENTED
**File**: `rust/src/blossom.rs`

**Resolution**: Added module-level documentation explaining the botanical metaphor:
- Grove: The CRDT graph data structure
- Forest: Tree decomposition layer
- Blossom: Typing layer that "blooms" on top of the forest

### 11. ~~Forest vs Term Naming Confusion~~ DOCUMENTED
**File**: `rust/src/forest.rs`

**Resolution**: Added module-level documentation explaining:
- Grove types (`Node`, `Edge`, `Location`): Raw graph primitives
- Term types (`TermNode`, `TermEdge`, etc.): Tree-view wrappers with PathHash
- "Forest" is the decomposition from the Grove formalism

### 12. ~~No Left Direction~~ DOCUMENTED
**File**: `ts/src/RustTypes.tsx:36-39`

**Resolution**: Added comment explaining the navigation model:
- Up: Move toward root
- Down: Move toward leaves (first child)
- Right: Next sibling (wraps around cyclically)
- No "Left" needed - Right wraps around to cover all siblings

### 13. ~~16-byte Path Hash Truncation~~ DOCUMENTED
**File**: `rust/src/forest.rs:23`

**Resolution**: Added collision analysis comment:
- 16 bytes = 128 bits = ~2^-64 birthday bound
- For N term occurrences, collision probability ≈ N²/2^128
- Even with 10^9 terms, probability is ~10^-20 (negligible)
- 50% space savings vs full 32-byte hash

### 14. ~~Clone-Heavy Code~~ DOCUMENTED
**File**: `rust/src/types.rs`

**Resolution**: Added module-level note acknowledging the clone-heavy style:
- Types are small enums, cloning is unlikely to be a bottleneck
- If profiling shows issues, consider: `Rc<T>`, references, or interning

### 15. ~~`Rc<RefCell<bool>>` for `is_in_unicycle`~~ NOT AN ISSUE
**File**: `rust/src/grove.rs:125`

**Note from review**: This is intentional - when a unicycle breaks, all constituents can update their bit simultaneously via the shared reference, without iterating.

---

## Clarifications from Review

**Q: What happens when two users both have cursors and one deletes the term the other's cursor is on?**
A: Handled by Grove logic. Deletion is severance, not destruction. The cursor remains in the severed subterm (invisible but functional). User can click to return to visible terms.

**Q: Are patches truly idempotent?**
A: Yes. If the ID is different, it's a different patch. Same ID = same patch = idempotent.

**Q: What's the story for undo/redo?**
A: TBD. Each patch can be "undone" by a new patch with same data but flipped sign (insert ↔ delete). This can be exploited for undo later.

**Q: Why `Rc<RefCell<bool>>` for `is_in_unicycle`?**
A: So that when a unicycle breaks, all constituents can flip their bit in one go via the shared reference, without iterating.
