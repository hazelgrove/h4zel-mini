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

### 4. JSON.stringify for Equality
**File**: `ts/src/Controller.ts:157-171`

Equality checks use `JSON.stringify(a) === JSON.stringify(b)`. This is fragile (key ordering), slow (O(n) serialization), and semantically questionable. For frequent cursor comparisons during rendering, this is a performance concern.

**Fix**: Implement proper deep equality, or compare by ID if structures have unique identifiers.

### 5. `any` Types Throughout TypeScript
**File**: `ts/src/Controller.ts:5-13`, `ts/src/RustTypes.tsx`

Core types are `any`: `TermEdge`, `TermNode`, `Patch`, etc. No type safety for the main data structures. `RustTypes.tsx` manually mirrors Rust types with no verification.

**Fix**: Generate TypeScript types from Rust definitions, or use a schema/validation layer.

### 6. `.unwrap()` in WASM Boundary
**File**: `rust/src/wasm-rust.rs:32-37`

```rust
fn from_js<T: DeserializeOwned>(v: JsValue) -> T {
    serde_wasm_bindgen::from_value(v).unwrap()  // Panics on bad data
}
```

Malformed JS data causes unhelpful WASM panics.

**Fix**: Return `Result` types and handle errors gracefully in TypeScript.

### 7. Projector Navigation is Bolted On
**File**: `ts/src/Controller.ts:369-476`

Extensive special-casing for projectors: `isProjectorNode()`, `isInsideProjector()`, etc. The `computeMove` function is riddled with projector checks. Suggests projectors don't fit naturally into the navigation model.

**Fix**: Consider making navigation polymorphic per constructor, or factor out a cleaner abstraction.

### 8. Mutation Hidden in "compute" Methods
**File**: `ts/src/Controller.ts:251-357`

Methods named `computeWrapLeft`, `computeDelete`, etc. mutate `this.cursor` as a side effect while returning patches. "Compute" suggests pure computation.

**Fix**: Either rename to `applyWrapLeft` etc., or refactor to separate cursor mutation from patch computation.

### 9. Clipboard Holds Stale References
**File**: `ts/src/Controller.ts:327-357`

If you cut a term and incoming patches restructure that area, clipboard holds a stale reference.

**Note from review**: This is actually handled by Grove semantics - the cursor/clipboard remains in the "severed" subterm, which still exists but isn't visible. User can click to return cursor to visible terms.

---

## Minor

### 10. "Blossom" Naming Unexplained
**File**: `rust/src/blossom.rs`

Why is the typing layer called "blossom"? Not documented. Confusing for newcomers.

**Fix**: Add a comment explaining the metaphor, or rename to something self-explanatory.

### 11. Forest vs Term Naming Confusion
**File**: `rust/src/forest.rs`

File is "forest" but defines `TermNode`, `TermEdge`, `TermLocation`, `TermSite`. Is the abstraction "forest" or "term"?

**Note**: "Forest" refers to the decomposition layer from the Grove paper - presenting the graph as a tree/forest of terms.

### 12. No Left Direction
**File**: `ts/src/RustTypes.tsx:36-39`

```typescript
export type Direction = "Up" | "Down" | "Right"
```

No "Left" direction. Presumably "Right" wraps around, but this is implicit.

**Fix**: Document the navigation model, or add "Left" for symmetry.

### 13. 16-byte Path Hash Truncation
**File**: `rust/src/forest.rs:23`

```rust
type PathHash = [u8; 16];  // Only 16 bytes of SHA256
```

SHA256 produces 32 bytes. Collision probability of 16 bytes (~2^64 birthday bound) may be acceptable but isn't analyzed.

**Fix**: Document the collision analysis, or use full 32 bytes.

### 14. Clone-Heavy Code
**File**: `rust/src/types.rs`

Many `.clone()` calls throughout. For incremental efficiency goals, excessive cloning could matter.

**Fix**: Profile and optimize if needed. Consider references where possible.

### 15. `Rc<RefCell<bool>>` for `is_in_unicycle`
**File**: `rust/src/grove.rs:125`

Uses runtime borrow checking for shared mutation of unicycle status.

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
