# TODO: Forest Layer, Order Maintenance, and OpenReference

**Read this whole document before writing any code.** The changes are interdependent.

---

## 1. What and Why

The grove is a CRDT graph where nodes can have multiple parents (sharing) or participate in cycles. Type checking, rendering, and cursor navigation all need a **tree**. Currently each consumer does its own ad-hoc decomposition:

- `render.rs` traverses the grove with a `visited: HashSet<Uuid>` to break cycles
- `blossom.rs` keys type attributes by `Site::Term(Uuid)` — raw grove node IDs
- `blossom.rs` picks the next dirty site by O(n) scanning all dirty sites and computing depth via parent-chain walks

This refactor adds:
1. **`forest.rs`** — the single source of truth for graph→tree decomposition
2. **`order.rs`** — order maintenance intervals on tree sites, encoding document order
3. **Priority-queue-driven type checking** — replaces the O(n) dirty-set scanning in blossom
4. **Path-hashed tree site keys** — every map (intervals, type attributes, worklist) is keyed by `TreeSite`, which uniquely identifies a position in the decomposed tree

### Why path hashing

A grove node with 2+ parents appears at multiple positions in the decomposed tree when references are opened. In an unrolled unicycle, the same node (and the same edge) appears at multiple depths. Only the full path from root disambiguates these. The path hash (`SHA256(parent_hash || edge_uuid)[..16]`) provides:
- O(1) equality comparison
- Fixed 16-byte key (collision probability ~N²/2^128, negligible)
- Incremental computation (each edge extends the parent's hash)

By using `TreeSite` from the start, `OpenReference` is a local change in forest traversal — no key migrations in blossom, render, or intervals.

---

## 2. New Types

Defined in `forest.rs`:

```rust
/// First 16 bytes of SHA256(parent_path || edge_uuid).
/// Identifies a unique path from root in the decomposed tree.
pub type PathHash = [u8; 16];

/// A site in the decomposed tree: a grove Site at a specific tree path.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct TreeSite {
    pub path: PathHash,
    pub site: Site,   // Site::Term(Uuid) or Site::Loc(Location)
}

/// Order maintenance interval encoding document position.
#[derive(Clone)]
pub struct Interval {
    pub start: Order,
    pub end: Order,
}
```

Path hash computation:
```rust
fn extend_path(parent_path: PathHash, edge_id: Uuid) -> PathHash {
    let mut hasher = Sha256::new();
    hasher.update(parent_path);
    hasher.update(edge_id.as_bytes());
    hasher.finalize()[..16].try_into().unwrap()
}
```

The root path is `[0u8; 16]`.

---

## 3. New File: `rust/src/order.rs`

Complete module — wraps the `order-maintenance` crate (already in Cargo.toml):

```rust
use order_maintenance::Priority;
use std::cmp::Ordering;

#[derive(Clone, PartialEq, Eq, PartialOrd)]
pub struct Order {
    priority: Priority,
}

impl Ord for Order {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap_or(Ordering::Equal)
    }
}

impl Order {
    pub fn new() -> Self {
        Order { priority: Priority::new() }
    }

    /// Insert a new point immediately after this one.
    /// Returns (self, new_point) where self < new_point.
    pub fn split(self) -> (Order, Order) {
        let other = Order { priority: self.priority.insert() };
        (self, other)
    }
}
```

---

## 4. New File: `rust/src/forest.rs`

The forest is **parallel state** alongside the grove — both owned by `HazelState`. The forest reads the grove for structural queries and maintains its own data: intervals and path mappings.

### State

```rust
pub struct Forest {
    /// Order maintenance intervals for each tree site.
    intervals: HashMap<TreeSite, Interval>,

    /// Cache: grove node UUID → path hash in the decomposed tree.
    /// In the 1:1 case (no opened references), each node has one entry.
    /// With OpenReference, a node may have multiple path hashes — upgrade
    /// to HashMap<Uuid, Vec<PathHash>> at that point.
    node_paths: HashMap<Uuid, PathHash>,

    /// Reverse lookup: path hash → (parent path hash, edge UUID).
    /// Needed for upward navigation (finding a tree site's parent tree site).
    /// When OpenReference unrolls cycles, this maps each expanded occurrence
    /// back to its parent in the decomposed tree.
    path_parents: HashMap<PathHash, (PathHash, Uuid)>,

    /// Which reference paths have been opened (expanded).
    /// Initially empty — all references are collapsed.
    /// OpenReference adds entries here.
    open_paths: HashSet<PathHash>,
}
```

### Initialization

```rust
impl Forest {
    pub fn init(root_id: Uuid) -> Self {
        let root_path: PathHash = [0u8; 16];

        let o = Order::new();
        let (o1, o2) = o.split();
        let (o2, o3) = o2.split();
        let (o3, o4) = o3.split();

        let root_term = TreeSite { path: root_path, site: Site::Term(root_id) };
        let root_loc = TreeSite {
            path: root_path,
            site: Site::Loc(Location { node: root_id, position: 0 }),
        };

        let mut intervals = HashMap::new();
        intervals.insert(root_term, Interval { start: o1, end: o4 });
        intervals.insert(root_loc, Interval { start: o2, end: o3 });

        let mut node_paths = HashMap::new();
        node_paths.insert(root_id, root_path);

        Forest {
            intervals,
            node_paths,
            path_parents: HashMap::new(),
            open_paths: HashSet::new(),
        }
    }
}
```

### Traversal API (the decomposition)

These methods are the centralized decomposition logic. Render and blossom call these instead of querying the grove directly for tree structure.

```rust
pub enum ChildResult {
    Node(Uuid),
    Reference(Uuid),
}

impl Forest {
    /// Is this node a reference (collapsed) in the decomposed tree?
    pub fn is_reference(&self, node_id: Uuid, grove: &Grove) -> bool {
        Some(node_id) != grove.root_id
            && (grove.is_grove_root(node_id) || grove.is_in_unicycle(node_id))
    }

    /// Child locations of a node (0..arity).
    /// References have no children (they're leaves until opened).
    pub fn children_of_node(&self, node_id: Uuid, grove: &Grove) -> Vec<Location> {
        if self.is_reference(node_id, grove) {
            return vec![];
        }
        // same as current grove-based enumeration
        ...
    }

    /// Live children at a location, classified as Node or Reference.
    pub fn children_at_location(
        &self, loc: &Location, grove: &Grove,
    ) -> Vec<ChildResult> {
        grove.live_children_at(loc).iter().map(|&child_id| {
            if self.is_reference(child_id, grove) {
                ChildResult::Reference(child_id)
            } else {
                ChildResult::Node(child_id)
            }
        }).collect()
    }

    /// Unique parent location of a node in the decomposed tree.
    pub fn parent_of(&self, node_id: Uuid, grove: &Grove) -> Option<Location> {
        if Some(node_id) == grove.root_id {
            return None;
        }
        grove.parent_location(node_id)
    }

    /// Look up a tree site's interval.
    pub fn interval_of(&self, site: &TreeSite) -> Option<&Interval> {
        self.intervals.get(site)
    }

    /// Convert a grove Site to a TreeSite using the cached path hash.
    pub fn tree_site_of(&self, site: &Site) -> TreeSite {
        let node_id = match site {
            Site::Term(id) => *id,
            Site::Loc(loc) => loc.node,
        };
        let path = self.node_paths.get(&node_id).copied().unwrap_or([0u8; 16]);
        TreeSite { path, site: site.clone() }
    }
}
```

### Interval Maintenance

Called after `grove.apply_patch()` returns dirty sites:

```rust
impl Forest {
    /// Update path caches and intervals for dirty sites.
    pub fn update(&mut self, dirty_sites: &[Site], grove: &Grove) {
        for site in dirty_sites {
            self.update_node_path(site, grove);
            let tree_site = self.tree_site_of(site);
            self.ensure_interval(&tree_site, grove);
        }
    }

    /// Ensure a node's path hash is cached. For birth patches, compute
    /// from the parent's cached path + the edge UUID.
    fn update_node_path(&mut self, site: &Site, grove: &Grove) {
        let node_id = match site {
            Site::Term(id) => *id,
            Site::Loc(loc) => loc.node,
        };
        if self.node_paths.contains_key(&node_id) {
            return;
        }
        // New node: compute path from parent
        if let Some(edge) = grove.unique_parent_edge(node_id) {
            let parent_path = self.node_paths
                .get(&edge.source.node)
                .copied()
                .unwrap_or([0u8; 16]);
            let path = extend_path(parent_path, edge.id);
            self.node_paths.insert(node_id, path);
            self.path_parents.insert(path, (parent_path, edge.id));
        }
    }

    /// Ensure a tree site has an interval. If not, allocate by splitting
    /// the parent's interval. Then recursively ensure children are nested.
    fn ensure_interval(&mut self, site: &TreeSite, grove: &Grove) {
        if self.intervals.contains_key(site) {
            // Already has an interval — just verify children are nested
            self.nest_children(site, grove);
            return;
        }
        // Allocate interval within parent's interval
        if let Some(parent_site) = self.parent_tree_site(site, grove) {
            if let Some(parent_interval) = self.intervals.get(&parent_site).cloned() {
                let (s1, s2) = parent_interval.start.split();
                let (s2, s3) = s2.split();
                let (s3, s4) = s3.split();
                // Parent gets [s1, s4], this site gets [s2, s3]
                self.intervals.insert(parent_site, Interval { start: s1, end: s4 });
                self.intervals.insert(*site, Interval { start: s2, end: s3 });
                self.nest_children(site, grove);
            }
        }
    }

    /// Ensure all children's intervals are strictly within this site's interval.
    fn nest_children(&mut self, site: &TreeSite, grove: &Grove) {
        // Implementation: for Term sites, ensure each child Location is nested.
        // For Loc sites, ensure each child Term is nested.
        // Skip if the child's interval is already properly contained.
        // If not, split endpoints to make room.
        ...
    }

    /// Find the parent TreeSite of a given TreeSite.
    fn parent_tree_site(&self, site: &TreeSite, grove: &Grove) -> Option<TreeSite> {
        match &site.site {
            // A Location's parent is the Term at the same node
            Site::Loc(loc) => Some(TreeSite {
                path: site.path,
                site: Site::Term(loc.node),
            }),
            // A Term's parent is the Location that contains it
            Site::Term(node_id) => {
                let parent_loc = grove.parent_location(*node_id)?;
                let parent_path = self.node_paths.get(&parent_loc.node).copied()?;
                Some(TreeSite {
                    path: parent_path,
                    site: Site::Loc(parent_loc),
                })
            }
        }
    }
}
```

### OpenReference

This is the mechanism for expanding collapsed references. Not implemented in phase 1, but the data structures support it.

**Action**: `OpenReference(edge_id: Uuid)` — expand the reference at this edge.

**Implementation** (for future):

```rust
impl Forest {
    pub fn open_reference(
        &mut self, edge_id: Uuid, grove: &Grove,
    ) -> Vec<TreeSite> {
        let edge = grove.edge(edge_id);  // need grove to expose edge lookup
        let dest_id = edge.destination;

        // Compute the path hash for the parent of this edge
        let source_path = self.node_paths
            .get(&edge.source.node)
            .copied()
            .unwrap_or([0u8; 16]);
        let new_path = extend_path(source_path, edge_id);

        // Mark this path as opened
        self.open_paths.insert(new_path);

        // Register the new occurrence of the destination node
        // (upgrade node_paths to Vec<PathHash> when this is implemented)
        self.node_paths.insert(dest_id, new_path);
        self.path_parents.insert(new_path, (source_path, edge_id));

        // Recursively discover all descendants of the opened node.
        // Each descendant gets a new TreeSite with a path hash extending new_path.
        // If a descendant is itself a grove-root, it appears as a Reference
        // (which can itself be opened later, unrolling further).
        let mut new_sites = vec![];
        self.expand_subtree(dest_id, new_path, grove, &mut new_sites);

        // Allocate intervals for all new sites
        for site in &new_sites {
            self.ensure_interval(site, grove);
        }

        new_sites
    }

    fn expand_subtree(
        &mut self,
        node_id: Uuid,
        path: PathHash,
        grove: &Grove,
        out: &mut Vec<TreeSite>,
    ) {
        // Add this node's Term site
        out.push(TreeSite { path, site: Site::Term(node_id) });

        let node = match grove.node(node_id) {
            Some(n) => n,
            None => return,
        };

        for pos in 0..node.arity {
            let loc = Location { node: node_id, position: pos };
            // Add this Location site
            out.push(TreeSite { path, site: Site::Loc(loc) });

            for &child_id in &grove.live_children_at(&loc) {
                if self.is_reference(child_id, grove) {
                    // Child is a reference — don't recurse, but do register it.
                    // (The user can open it separately later.)
                } else {
                    // Compute child's path hash via the edge
                    if let Some(edge) = grove.edge_from_loc_to(&loc, child_id) {
                        let child_path = extend_path(path, edge.id);
                        self.node_paths.insert(child_id, child_path);
                        self.path_parents.insert(child_path, (path, edge.id));
                        self.expand_subtree(child_id, child_path, grove, out);
                    }
                }
            }
        }
    }
}
```

**Effect on traversal**: `is_reference` checks `open_paths`:
```rust
pub fn is_reference(&self, node_id: Uuid, parent_path: PathHash,
                     edge_id: Uuid, grove: &Grove) -> bool {
    if Some(node_id) == grove.root_id { return false; }
    if !grove.is_grove_root(node_id) && !grove.is_in_unicycle(node_id) {
        return false;
    }
    // It's a grove-root — check if this specific path is opened
    let path = extend_path(parent_path, edge_id);
    !self.open_paths.contains(&path)
}
```

Note: when `OpenReference` is implemented, `is_reference` needs the parent path and edge to compute the specific path hash. This means the traversal API signatures evolve slightly — `children_at_location` needs to know the current path context. For phase 1 (no opened references), the simpler signature works.

**Effect on node_paths**: with opened references, a single grove node can have multiple path hashes. `node_paths: HashMap<Uuid, PathHash>` becomes `HashMap<Uuid, Vec<PathHash>>`, or more precisely, the forest tracks all expanded occurrences. The `tree_site_of` helper would need to know which occurrence to use — this is resolved by the traversal context (the path hash flows down during traversal).

**Effect on dirty propagation**: when a grove patch dirties a site, the forest looks up all path hashes for that node (via `node_paths`) and creates a `TreeSite` for each. All are enqueued in blossom's worklist.

---

## 5. Changes to `rust/src/blossom.rs`

### Key type: `Site` → `TreeSite`

Every `HashMap<Site, ...>` becomes `HashMap<TreeSite, ...>`. Every method that takes `&Site`, `Uuid` (as node ID), or `Location` for attribute storage/lookup switches to `TreeSite`.

Methods that call grove for *structural* queries (children, parents, constructors, arity) extract the grove `Site`/`Uuid`/`Location` from the `TreeSite` and pass it to grove. Methods that read/write *type state* (attrs, dirty, bindings) use the full `TreeSite`.

### Dirty set → priority queue

```rust
// Before
dirty: HashSet<Site>,

// After
worklist: PriorityQueue<TreeSite, Reverse<Order>>,
```

`PriorityQueue` is from the `priority-queue` crate (already in Cargo.toml). `Reverse` makes it a min-heap: smallest interval start (shallowest node) is popped first.

### `mark_dirty`

```rust
// Before
pub fn mark_dirty(&mut self, site: Site) {
    self.dirty.insert(site);
}

// After
pub fn mark_dirty(&mut self, site: TreeSite, forest: &Forest) {
    if let Some(interval) = forest.interval_of(&site) {
        self.worklist.push(site, Reverse(interval.start.clone()));
    }
}
```

### `update_step` / `update_all`

```rust
// After
pub fn update_step(&mut self, grove: &Grove, forest: &Forest) -> bool {
    let (site, _) = match self.worklist.pop() {
        Some(x) => x,
        None => return false,
    };
    self.recompute(&site, grove, forest);
    true
}

pub fn update_all(&mut self, grove: &Grove, forest: &Forest) {
    for _ in 0..10000 {
        if !self.update_step(grove, forest) { break; }
    }
}
```

### Delete entirely

- `pick_next()` — replaced by `worklist.pop()`
- `depth_of()` — replaced by interval ordering

### Thread `&Forest` through internal methods

`recompute`, `dirty_dependents`, and any method that calls `mark_dirty` need `&Forest` in their signature. The changes are mechanical — add the parameter and pass it through.

### `get_attr`

```rust
// Before
pub fn get_attr(&self, site: &Site) -> TypeAttribute

// After
pub fn get_attr(&self, site: &TreeSite) -> TypeAttribute
```

---

## 6. Changes to `rust/src/render.rs`

### Add `&Forest` parameter to all render functions

```rust
pub fn render_tree(grove: &Grove, forest: &Forest, blossom: &Blossom, controller: &Controller) -> RenderNode
```

### Remove `visited: HashSet<Uuid>`

The forest handles cycle/reference detection. Remove the `visited` parameter from `render_term`, `render_location`, and all call sites.

### `render_term` — use forest for reference detection

Before:
```rust
if !visited.insert(node_id) { return Reference }
if grove.is_grove_root(node_id) && ... { return Reference }
if grove.is_in_unicycle(node_id) { return Reference }
```

After:
```rust
if forest.is_reference(node_id, grove) {
    return RenderNode::Reference { id: node_id.to_string() };
}
```

### `render_location` — use forest for child enumeration

Before: calls `grove.live_children_at(loc)` and has inline logic for deciding node vs reference vs conflict.

After: calls `forest.children_at_location(loc, grove)` which returns `Vec<ChildResult>`. Map `ChildResult::Node` → recurse, `ChildResult::Reference` → `RenderNode::Reference`. Multiple children at same location → `RenderNode::Conflict`.

### Type attribute lookups use `TreeSite`

```rust
// Before
let attr = blossom.get_attr(&Site::Term(node_id));

// After
let tree_site = forest.tree_site_of(&Site::Term(node_id));
let attr = blossom.get_attr(&tree_site);
```

### `cursor_info` also takes `&Forest`

---

## 7. Changes to `rust/src/lib.rs`

### Add modules

```rust
pub mod forest;
pub mod order;
```

### Update `HazelState`

```rust
struct HazelState {
    grove: Grove,
    forest: Forest,
    blossom: Blossom,
    controller: Controller,
}
```

### Update `genesis`

After creating the root node, initialize the forest:
```rust
self.forest = Forest::init(root_id);
```

### Update patch application flow

The current `apply_patches_to_state` helper applies patches to grove and marks blossom dirty. Replace with:

```rust
fn apply_patch(
    grove: &mut Grove,
    forest: &mut Forest,
    blossom: &mut Blossom,
    patch: &Patch,
) {
    let dirty = grove.apply_patch(patch);
    forest.update(&dirty, grove);
    for site in &dirty {
        blossom.mark_dirty(forest.tree_site_of(site), forest);
    }
}
```

Update all callers: `perform_action`, the WASM `apply_patch` entry point, etc.

### Update `render` call

Pass `&self.forest` to `render_tree` and `cursor_info`.

---

## 8. Changes to test harnesses

### `rust/src/scenario.rs`

Add `forest: Forest` field to `Scenario`. Update `new()` to call `Forest::init(root_id)`. Update `apply_patches` to call `forest.update(...)`. Update `act` to pass `&forest` to blossom methods.

### `rust/src/sync_scenario.rs`

Each user's `Scenario` has its own `Forest` (the forest is local state, not synced). The rest follows from the `Scenario` changes.

---

## 9. Complexity Improvement

| Operation | Before | After |
|-----------|--------|-------|
| Pick next dirty site | O(n) scan × O(depth) parent walk | O(log n) heap pop |
| Mark dirty | O(1) | O(1) amortized |
| Priority comparison | O(depth) | O(1) |

---

## 10. Execution Order

1. **Add `order.rs`** — standalone module, no dependencies on other changes.
2. **Add `forest.rs`** — depends on `order.rs`. Implement `Forest`, `TreeSite`, `PathHash`, `Interval`, interval allocation, traversal API, `tree_site_of`. Write unit tests for path hash computation and interval nesting.
3. **Update `lib.rs`** — add modules, add `Forest` to `HazelState`, update `genesis`, replace `apply_patches_to_state` with the new patch flow.
4. **Update `blossom.rs`** — switch all keys to `TreeSite`, replace `HashSet` with `PriorityQueue`, thread `&Forest` through, delete `depth_of`/`pick_next`.
5. **Update `render.rs`** — add `&Forest` parameter, use forest traversal, remove `visited` set.
6. **Update `scenario.rs` / `sync_scenario.rs`** — add `Forest` to harnesses, thread through.
7. **Run all 98 tests.** They should all pass — behavior is unchanged, only the internal priority mechanism and key types changed.

Steps 1–2 are additive. Steps 3–6 are the migration — do them together.
