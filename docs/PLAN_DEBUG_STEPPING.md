# Plan: Step-by-step attribute propagation debug view

## Feature

A toggle in the UI that switches between:
- **Normal mode** (current): `update_all()` runs automatically after every action/patch. The user never sees intermediate states.
- **Debug mode**: `update_all()` is suppressed. The user clicks a "Step" button to execute one worklist item at a time. All sites currently in the worklist are highlighted orange in the render tree.

## What already exists

- `HazelState::update_all()` — drains the worklist (WASM, exposed to JS)
- `Action::BlossomAction(UpdateStep)` — processes one worklist item (already wired through `dispatch_action` and `perform_action`)
- `HazelState::has_dirty()` — returns whether the worklist is non-empty (WASM, exposed to JS)
- The render tree already carries `marks` for type errors (rendered with red highlight via `.has-marks` CSS class)

## Rust changes

### 1. Expose the dirty set to the render tree

The render tree needs to know which nodes are dirty so the UI can highlight them. Two options:

**Option A: Add a `dirty` boolean field to `RenderNode::Term`.**
The renderer checks whether each node's `TreeSite` is in the worklist and sets the flag. Simple, but requires blossom to expose "is this site dirty?" which means scanning the `BinaryHeap` (no efficient lookup — heaps don't support membership queries).

**Option B: Return the dirty set separately as a `HashSet<String>` of node IDs.**
Add a new WASM method `dirty_node_ids()` that returns the set of grove node UUIDs currently in the worklist. The TS side applies highlights by checking membership. No render tree changes needed.

**Option C: Maintain a `HashSet<TreeSite>` shadow of the worklist.**
Blossom maintains a `dirty_set: HashSet<TreeSite>` alongside the `BinaryHeap`. Items are added to both in `mark_dirty` and removed from both in `update_step`. This provides O(1) membership queries. The renderer can then check `blossom.is_dirty(site)` and set a flag on the render node.

**Recommendation: Option C.** It's the cleanest — the dirty flag flows through the same render path as everything else, no extra WASM call from JS, and the shadow set is cheap (it's just a HashSet alongside the heap we already have).

### 2. Add `dirty` field to `RenderNode::Term`

```rust
// In render.rs, RenderNode::Term:
dirty: bool,  // true if this site is in the blossom worklist
```

In `render_term`, after looking up `blossom.get_attr(...)`:
```rust
let dirty = blossom.is_site_dirty(&Site::Term(node_id), forest);
```

Add similar for `RenderNode::Hole` and locations — a location being dirty means the slot itself needs recomputation. But highlighting holes might be noisy. Start with just term nodes.

### 3. Add `is_site_dirty` to Blossom

```rust
pub fn is_site_dirty(&self, site: &Site, forest: &Forest) -> bool {
    let ts = forest.tree_site_of(site);
    self.dirty_set.contains(&ts)
}
```

### 4. Suppress `update_all` in `perform_action` when in debug mode

Currently `perform_action` always calls `self.blossom.update_all(...)` at the end. In debug mode, it should skip this. Two ways to handle this:

**Option A: Add a `debug_stepping: bool` field to `HazelState`.**
`perform_action` checks it and skips `update_all` when true. A WASM method `set_debug_stepping(bool)` toggles it.

**Option B: Let the TS side control when `update_all` is called.**
`perform_action` never calls `update_all` — the TS side calls it explicitly after the action (in normal mode) or not (in debug mode). This is cleaner separation but means the TS side has to remember to call `update_all`.

**Recommendation: Option A.** The Rust side already calls `update_all` in `perform_action` and `apply_patch` — having the TS side manage this is error-prone (there are multiple code paths). A boolean flag in `HazelState` is simpler.

But there's a subtlety: `perform_action` also calls `update_all` **before** auto-advance (line 226). This is needed for auto-advance to work correctly (it needs up-to-date type info to decide where to advance). In debug mode, should auto-advance still work?

**Decision needed: Should auto-advance work in debug mode?** If yes, we need `update_all` before auto-advance even in debug mode (otherwise WrapLeft/WrapRight won't auto-advance, which makes editing broken). If no, debug mode only works for observation, not editing.

**Recommendation:** Auto-advance should still work. Only suppress the *final* `update_all` at the end of `perform_action`. The mid-action `update_all` for auto-advance stays. This means: after a WrapLeft in debug mode, the cursor moves correctly, but the type information shown is stale until the user steps through.

Actually, a cleaner approach: in debug mode, `perform_action` applies patches and updates the forest (structural changes) but does NOT drain the blossom worklist. The worklist accumulates dirty sites. The user steps through them manually. Auto-advance needs types? Actually, auto-advance only needs grove structure (it checks arity), not types. Let me verify:

```rust
// controller.rs
pub fn auto_advance_down(&self, grove: &Grove) -> Vec<Patch> {
```

Yes — auto-advance only reads grove structure, not blossom. So suppressing `update_all` in debug mode is safe for all code paths.

### 5. New WASM methods

```rust
pub fn set_debug_stepping(&mut self, enabled: bool)
pub fn worklist_size(&self) -> usize  // for the UI to show count
```

`update_step` already exists as an action. But dispatching it through `perform_action` is roundabout — it goes through action parsing and patch application. A direct WASM method is cleaner:

```rust
pub fn step_once(&mut self) -> bool {
    self.blossom.update_step(&self.grove, &self.forest)
}
```

## TypeScript changes

### 6. Add `dirty` to the RenderNode type

```typescript
// In App.tsx, the TermNode type:
dirty?: boolean;
```

### 7. Add debug stepping state and UI controls

```typescript
const [debugStepping, setDebugStepping] = useState(false);
```

Toggle checkbox next to the existing sync toggle. When toggled:
- Call `state.set_debug_stepping(true/false)`
- When turning OFF debug mode, call `state.update_all()` to flush the worklist and re-render.

Step button (visible only in debug mode):
- Calls `state.step_once()`
- Re-renders

Show worklist size: `state.worklist_size()` displayed next to the step button.

### 8. Suppress `update_all` calls in TS when debug mode is on

Currently the TS side calls `state.update_all()` in several places:
- After `init_cursor` (line 718)
- After initialization (line 703)

These should still run (they're part of setup). The Rust side handles suppression in `perform_action` via the flag.

But the `onChange` handler for remote patches (line 757) calls `state.update_all()` — this should also be suppressed in debug mode. Either check `debugStepping` before calling, or let the Rust flag handle it (but `update_all` is called directly, not through `perform_action`, so the flag wouldn't help here).

**Recommendation:** Have the TS `onChange` handler check `debugStepping` and skip `update_all()` when true. Same for any other direct calls to `update_all()`.

### 9. CSS for dirty highlight

```css
.dirty {
  outline: 2px solid rgba(255, 165, 0, 0.7);
  outline-offset: 1px;
  background: rgba(255, 165, 0, 0.15);
}
```

In `RenderNodeView`, add `dirty` to the class list:
```typescript
const dirtyClass = t.dirty ? "dirty" : "";
```

### 10. Keyboard shortcut for stepping

Optional: bind a key (e.g., `Ctrl+U` — already mapped to `UpdateStep` action) to step. This already exists in `keyToAction`:
```typescript
case "u": return { BlossomAction: "UpdateStep" };
```

But this goes through `perform_action` which in debug mode won't call `update_all`. It will call `dispatch_action` → `update_step`. That works — one item is popped, types update for that site, and we re-render. But `perform_action` also calls `apply_patches_to_state` on the empty patches list (no-op) and then skips `update_all` (debug mode). This is fine.

## Decisions

1. **Auto-advance works in debug mode.** `auto_advance_down` only reads grove structure (arity, parent edges), not types. Both `update_all` calls in `perform_action` are suppressed. The cursor lands correctly but type info is stale until stepped through.

2. **Remote patches accumulate dirty sites in debug mode.** Correct behavior — the user sees all pending propagation work from both local and remote changes.

3. **Highlight everything** — term nodes AND locations (holes/slots). If a location is dirty, the slot highlight shows it needs recomputation.

4. **Toggle off → flush.** Toggling debug mode off immediately calls `update_all` and re-renders.

5. **Deduplicate on push.** `mark_dirty` checks the `dirty_set` before pushing to the heap. If the site is already in the set, skip the push. No lazy deletion needed.

## Execution order

1. Add `dirty_set: HashSet<TreeSite>` to `Blossom`, update `mark_dirty` and `update_step` with lazy deletion.
2. Add `is_site_dirty` method to `Blossom`.
3. Add `dirty: bool` field to `RenderNode::Term`. Set it in `render_term`.
4. Add `debug_stepping: bool` to `HazelState`. Add `set_debug_stepping` and `step_once` WASM methods. Guard `update_all` in `perform_action` and `apply_patch`.
5. TS: add `dirty` to render node type, add CSS, add dirty class to `RenderNodeView`.
6. TS: add debug stepping toggle, step button, worklist size display.
7. TS: suppress `update_all` in `onChange` when debug stepping is on.
