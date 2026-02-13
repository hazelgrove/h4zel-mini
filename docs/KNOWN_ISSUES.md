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

### 7a. CanvasProjector Depth Variable Shadowing (FIXED)
**File**: `ts/src/CanvasProjector.tsx`

In `collectNodesAndEdges`, a `let depth = 0` for cursor unwrapping shadowed the recursion `depth` parameter. The recursive call passed the cursor-unwrap counter instead of the actual recursion depth, so the depth guard (max 100) never triggered. Combined with path-hash-based visited tracking, this could cause infinite recursion when nesting projectors (e.g., Structural inside Canvas).

**Fixed**: Renamed the cursor-unwrap counter to `unwrapCount`.

### 7b. Fragile Multi-Step Projector Creation (FIXED)
**Files**: `ts/src/App.tsx`, `ts/src/Controller.ts`

Creating a projector required multiple sequential actions (WrapRight Proj, MoveToLocation, Insert type, move back). Between actions the tree state changed, making the process fragile — stale TermNode references, cursor in wrong position, etc.

**Fixed**: Added atomic `WrapWithProjector` action that creates Proj node, type node, and any sub-structure (Canvas PosNil, Labeled default label) in one patch set.

### 7d. CanvasProjector Cursor Patches Not Emitted (FIXED)
**File**: `ts/src/CanvasProjector.tsx`

`handleNodeClick`, `handleSlotClick`, and `handleWireDrop` all called `controller.move_to_term()` / `controller.move_to_location()` directly. These methods apply patches locally but return `void`, so cursor movement patches were never emitted to Automerge sync. This caused divergent state between clients when interacting with Canvas projectors.

**Fixed**: Added `applyAction` prop to CanvasProjector. All cursor movement now goes through `applyAction({ MoveToTerm: ... })` / `applyAction({ MoveToLocation: ... })`, which returns patches for emission.

### 7c. ~~Projector Navigation is Bolted On~~ ADDRESSED
**File**: `ts/src/Controller.ts:369-476`

~~Extensive special-casing for projectors suggests they don't fit naturally into the navigation model.~~

**Resolution**: Added design documentation explaining the rationale:
- Projectors wrap terms with view metadata (position 0: type, position 1: content)
- Navigation treats projectors as "transparent" - users navigate to content directly
- Alternative designs (polymorphic navigation, metadata on edges) considered but deferred
- Current explicit special-casing is pragmatic for a prototype

### 8. ~~Mutation Hidden in "compute" Methods~~ FIXED
**File**: `ts/src/Controller.ts`

~~Methods named `computeWrapLeft`, `computeDelete`, etc. mutate `this.cursor` as a side effect.~~

**Fixed**: The `this.cursor` field was removed entirely. Methods now:
- Compute patches that modify the Grove cursor directly
- Derive all cursor state from the Grove via `myIdentityNode`
- No local cursor state to mutate

This follows the correct architecture: there is no local cursor, only the Grove cursor.

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

**Q: Are patches commutative?**
A: Yes, patches are **intrinsically commutative** by Grove's design. This is a property of the CRDT, not something application code needs to ensure. If patches appear non-commutative, the bug is in patch generation, not in Grove.

**Q: What's the story for undo/redo?**
A: TBD. Each patch can be "undone" by a new patch with same data but flipped sign (insert ↔ delete). This can be exploited for undo later.

**Q: Why `Rc<RefCell<bool>>` for `is_in_unicycle`?**
A: So that when a unicycle breaks, all constituents can flip their bit in one go via the shared reference, without iterating.

---

## Cursor Pitfalls

See `docs/DESIGN.md` for cursor specification.

### Common Pitfalls

1. **Creating a "local cursor" abstraction**: There is no local cursor. The Grove cursor is the only cursor. All state is derived from the identity node. If you find yourself writing `this.cursor = ...`, you're doing it wrong.

2. **Tree traversal for finding cursor**: NEVER call `findAllCursors()` except at initialization. Use the stable identity node reference instead. Tree traversal is O(n) and can infinite loop if there's any structural issue.

3. **Using edge IDs instead of node IDs**: Edges can change when structure changes. Always reference nodes by their stable IDs.

4. **Computing target before unwrap**: If you compute "where to move" using edges inside the cursor, then unwrap the cursor, those edges may no longer exist. Use node references instead.

5. **Creating new cursor nodes on move**: This creates multiple cursor nodes with the same identity. Always reuse the existing cursor node.

6. **Patch ordering for node creation**: When creating a node and referencing its locations, the patch that creates the node (connects it to a parent) must come BEFORE patches that reference the node's child locations. Otherwise the node doesn't exist when its locations are referenced.

7. **Initialization code running on every render**: In React function components, code outside hooks runs on every render. Initialization code (loading patches, creating cursor, syncing) MUST be guarded by a ref flag or placed in a useEffect with empty deps.

8. **Assuming children_of_term array indices match positions**: The `children_of_term()` function returns ONLY non-empty positions. If a node has arity 2 but position 0 is empty, `children_of_term()` returns a 1-element array where `[0]` is the child at position 1. To get a specific position, construct the TermLocation directly: `{ node: termNode, position: 0 }`.
