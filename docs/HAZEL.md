# Hazel: Design and Specification

> Single source of truth for the h4zel-mini project — vision, formal foundations, and design specification.
> Appendix A contains reusable Automerge integration code.

---

## 1. Vision and Motivation

Hazel is a live functional programming environment whose central thesis is that **every editor state should be semantically meaningful** — syntactically, statically, and dynamically — without exception. This is the *continuity invariant*.

### 1.1 The Gap Problem

Programming languages assign formal meaning only to complete programs, but programmers spend substantial time with *incomplete* programs. Conventional tools disable editor services precisely when the programmer needs them most. Three classes of gaps:

1. **Syntactic gaps**: The edit state is not well-formed according to the grammar.
2. **Static gaps**: The program is syntactically well-formed but ill-typed.
3. **Dynamic gaps**: Conventional dynamic semantics cannot evaluate a program with holes or type errors.

Hazel attacks all three: every reachable editor state has a well-defined syntax tree, a well-defined type, and can be evaluated to produce a (possibly indeterminate) result.

### 1.2 Planetary Computing Vision (Fairground)

The long-term vision extends Hazel to a collaborative planetary compute engine:

- **Live**: Continuous feedback about dynamic behavior during editing — no batch model.
- **Rich**: Programs embed domain-specific visualizations and direct-manipulation GUIs (via *livelits*).
- **Composable**: Pure functional core enables fusion, parallelization, and distribution. Every notebook is a library; the system forms a single live program.
- **Collaborative**: Multiple stakeholders work in a shared environment. One user's error cannot break others.
- **FAIR by default**: Findability, Accessibility, Interoperability, Reproducibility are structural properties.

### 1.3 Key Papers

| Paper | Venue | Topic |
|-------|-------|-------|
| [SNAPL 2017](https://arxiv.org/pdf/1703.08694) | SNAPL | The gap problem, vision |
| [PROPL 2024](https://hazel.org/papers/propl24.pdf) | PROPL | Totally live programming vision |
| [PROPL 2025](https://hazel.org/papers/fairground-propl25.pdf) | PROPL | Fairground planetary computing |
| [POPL 2024](https://hazel.org/papers/marking-popl24.pdf) | POPL | Marked lambda calculus (type error localization) |
| [POPL 2019](https://arxiv.org/pdf/1805.00155) | POPL | Live evaluation with typed holes |
| [POPL 2025](https://hazel.org/papers/grove-popl25.pdf) | POPL | Grove collaborative editing |
| [PLDI 2021](https://hazel.org/papers/livelits-pldi2021.pdf) | PLDI | Livelits |
| [arXiv 2025](https://arxiv.org/pdf/2504.08946) | arXiv | Incremental bidirectional typing |
| [arXiv 2025](https://arxiv.org/pdf/2508.16848) | arXiv | Tile-based syntax (tall tylr) |

---

## 2. Formal Foundations

### 2.1 The Marked Lambda Calculus (POPL 2024)

The theoretical backbone of Hazel's static semantics — total type error localization and recovery.

**Two expression languages:**
- **Unmarked expressions** (`e`): The user's program, including empty holes (`⟨⟩`).
- **Marked expressions** (`ě`): Extended with *error marks* — syntactic membranes around erroneous sub-expressions ("red squiggles").

**The marking judgment** is a total, deterministic function transforming any unmarked expression into a marked expression with errors localized. Defined bidirectionally:
- Synthetic: `Γ ⊢ e ↬ ě ⇒ τ` (infer type, insert marks)
- Analytic: `Γ ⊢ e ↬ ě ⇐ τ` (check against type, insert marks)

**Key metatheorems** (all mechanized in Agda): totality, well-formedness, soundness, unicity.

**Gradual typing as recovery:** When a mark is inserted, the unknown type `?` allows type checking to proceed optimistically past errors. Type consistency replaces type equality.

**Type hole inference:** A constraint-based unification layer attempts to fill type holes. Conflicts are localized to the hole itself using **PotentialTypeSets** — recursive sets of potential types with an `etc` constructor preventing premature polymorphic generalization.

### 2.2 Live Evaluation with Typed Holes (POPL 2019)

**Elaboration** transforms external expressions into internal expressions with **hole closures** (`⟨⟩ᵘ_σ`) and **casts** (`d⟨τ₁ ⇒ τ₂⟩`).

**Evaluation around holes:** Instead of aborting, evaluation continues. Three irreducible forms:
- **Values**: constants, lambdas
- **Indeterminate forms**: rooted at a hole closure or failed cast

**Fill-and-resume:** When the programmer fills a hole, evaluation need not restart. A **commutativity theorem** ensures correctness (for pure functional languages).

**The continuity invariant** composes: sensibility → totality of marking → typed elaboration → progress + preservation.

### 2.3 Tile-Based Parsing (tall tylr, 2025)

**Syntactic obligations** generalize holes to cover missing operands, operators, delimiters, and sort transitions. A **molder** turns tokens into tiles; a **melder** parses tiles into terms. Every edit state maps to a syntactically well-formed program sketch.

### 2.4 Livelits: Live Literals (PLDI 2021)

Custom GUIs that fill typed holes via a **model-view-update-expand** architecture. Compositionality via **splices** — sub-expression editors with lexical scoping. Liveness via **closure collection** and **resumption**.

### 2.5 Incremental Bidirectional Typing (2025)

The **Marked and Annotated Lambda Calculus (MALC)** stores error marks as boolean flags plus analyzed/synthesized types on every node. **Incremental update propagation**: dirty bits at edit locations propagate along a frontier, achieving **275.96× speedup** over from-scratch reanalysis.

---

## 3. System Architecture

### 3.1 Theoretical Pipeline

h4zel-mini implements the Grove, Forest decomposition, and incremental typing layers. Tile-based parsing, type hole inference, elaboration, live evaluation, and livelits are **not yet implemented**.

```
User Edits (keystrokes, structure edits)
    │
    ▼
[Structure Editor]  →  Direct structure editing (tile-based parsing NOT IMPLEMENTED)
    │
    ▼
[Grove CmRDT]  →  Convergent collaborative graph state
    │
    ▼
[Forest Decomposition]  →  Tree view of the graph
    │
    ▼
[Incremental Marking]  →  Marked & annotated program (every expression typed, errors localized)
    │
    ▼
[Rendering]  →  Visual presentation with cursor highlighting, projector views
```

### 3.2 Two-Runtime Design

The system is split into Rust (compiled to WASM) and TypeScript (browser):

- **Rust/WASM**: A pure state machine. Receives patches, maintains grove/forest/type state, answers queries. Never initiates actions.
- **TypeScript**: All user-facing logic — keyboard handling, patch construction, cursor movement, rendering, Automerge sync.

The WASM boundary serializes all data as JSON values. TypeScript creates patches using Rust-provided helper functions, applies them via the WASM API, then queries the resulting state for rendering.

### 3.3 Terminology

- **Grove**: The CRDT graph data structure with commutative operations.
- **Forest**: The decomposition layer presenting the grove as a tree, with path hashing for cycle/sharing handling.
- **World Tree**: The specific instance of the grove — the database of code that everyone collaborates on.
- **Blossom**: The incremental type checking layer — h4zel-mini's name for the MALC implementation.
- **Patch**: An atomic edge insertion or deletion. The only way to mutate the grove.
- **Site**: Either a Term or a Location — the unit at which type attributes are tracked.

### 3.4 Node Identity Stability

Grove nodes are identified by UUID and are **never deleted**. Once a node is created, its UUID persists for the lifetime of the grove. This means references to nodes (by UUID) survive all subsequent grove mutations. This property is critical — cursor movement specs, for example, capture a reference to a node and then mutate the grove; the reference remains valid.

---

## 4. Grove: Collaborative CRDT

### 4.1 Core Data Structure

The edit state is a **directed labeled multi-graph**:
- **Node**: Identified by UUID, labeled with a Constructor.
- **Edge**: Identified by UUID, connects a Location to a Node, has a Sign (Live or Dead).
- **Location**: A `(Node, Position)` pair — a specific child slot on a node. Position is a small integer (u8).
- **Constructor**: Either `Root` (the grove root, arity 1) or `Lang(constructor)` (a language constructor from section 6).

### 4.2 Patch Semantics

The only mutation is applying a Patch: `(edge_id, source_location, destination_node, sign)`.

The patch also carries the constructors of the source and destination nodes, so that newly created nodes can be initialized.

Four cases based on `(current_sign_of_edge, patch_sign)`:

| Current | Patch | Effect |
|---------|-------|--------|
| None | Live | **Birth**: Connect edge, create nodes if new (with their constructors and all arity-many child positions) |
| None | Dead | **Skip-life**: Record as Dead, no structural effect |
| Live | Live | **Idempotent**: No-op |
| Live | Dead | **Death**: Disconnect edge |

Edges never resurrect (`Dead → *` cannot occur). Vertices are never deleted.

**Commutativity**: All patches commute — applying them in any order yields the same state. This is guaranteed because each edge has a unique UUID, insertion/deletion operate on sets, and deletion is permanent.

### 4.3 Dirty Sites from Patches

Each patch returns a set of **dirty sites** that need type recomputation:

- **Birth**: source location + destination node + all child locations of any newly created nodes.
- **Death**: source location + destination node.
- **Skip-life / Idempotent**: nothing.

### 4.4 Derived Properties

The grove maintains derived properties that must be updated after each patch:
- **is_root(node)**: True iff node has 0 or 2+ live parent edges (shared, orphaned, or the top root).
- **is_in_unicycle(node)**: True iff node is in a cycle where every node has exactly one parent. Detection: follow parent pointers from the destination node; if we revisit the destination, it's a unicycle.

### 4.5 Arity and Position Validity

Each constructor defines an arity. The grove itself does **not** enforce arity — it is a free graph. Patch construction in the editor must only create edges to valid positions (0..arity-1). The renderer and type checker should handle unexpected positions gracefully (ignore or render as a conflict).

### 4.6 Decomposition into Trees

The graph decomposes into a forest of trees with special cases:
- **Holes**: Locations with no live out-edges (rendered as empty placeholders).
- **Local conflicts**: Locations with multiple live out-edges (multiple children at one position).
- **Relocation conflict references**: Nodes with multiple live in-edges (multiple parents).
- **Unicycle conflict references**: Cycles in single-parent chains, broken at a deterministic point (e.g., the edge with the lexicographically smallest UUID).

### 4.7 Forest Layer

The forest presents the grove as a navigable tree. Because the same grove node can appear at multiple points (via sharing or cycles), the forest disambiguates by augmenting each node/edge/location with a **path hash** — a deterministic hash of the path from the root to that occurrence. The path hash is computed incrementally: each edge's hash combines the parent's path hash with the edge's UUID (e.g., via SHA-256 or similar). The root has a fixed path hash (e.g., all zeros).

A node that is a "root" in the grove (2+ parents, or in a unicycle) appears as a **reference** when encountered during traversal. The grove's top root is NOT a reference — it is the traversal starting point. References can be "opened" (expanded) or "closed" (rendered as a placeholder). The `OpenReference` action toggles this. **This state is local** — it is not shared via Automerge, since it is a view preference.

### 4.8 Document-Order Intervals

Each site in the forest is assigned an interval `[start, end]` from an order maintenance data structure. Intervals nest: a parent's interval strictly contains all children's intervals.

**Allocation**: When a new site is created (via Birth patch), its interval is allocated within its parent's interval using the order maintenance structure's split operation. If the site has children, their intervals are allocated recursively within the new interval.

**Cleanup**: Intervals for disconnected sites (orphaned by Death patches) become stale but do not need explicit cleanup — they simply won't appear in forest traversal. The order maintenance structure tolerates orphaned entries.

### 4.9 Orphaned Subtrees

Deleting an edge (Death) disconnects the destination node from the tree. The node and all its descendants remain in the grove permanently — the grove never garbage-collects. Over time, orphaned subtrees accumulate. This is accepted as a tradeoff for CRDT simplicity and is bounded by user editing activity.

---

## 5. Core Design: Uniformity Principle

**No special cases.** Holes are just absence of children. Projectors are just nodes. Cursors are just nodes. Recursive rendering handles nesting naturally. The world tree representation is intentionally free/unconstrained — constraints live in editor behavior, not data structure.

---

## 6. Language Constructors

Every node in the grove has a constructor that determines its arity (number of child positions) and semantic role.

### 6.1 Constructor Table

| Constructor | Arity | Sort | Positions | Synthesized Type |
|---|---|---|---|---|
| `Root` | 1 | — | 0: program root | (not typed — structural) |
| `Typ` | 0 | Type | — | Typ (the type of types) |
| `Num` | 0 | Type | — | Typ |
| `Zero` | 0 | Expression | — | Num |
| `Plus` | 2 | Expression | 0: left, 1: right | Num |
| `Prod` | 2 | Type | 0: left, 1: right | Typ |
| `Pair` | 2 | Expression, Pattern | 0: first, 1: second | Prod(syn[0], syn[1]) |
| `Arrow` | 2 | Type | 0: domain, 1: codomain | Typ |
| `Fun` | 2 | Expression | 0: param (Pattern), 1: body (Expression) | Arrow(syn[0], syn[1]) |
| `Ap` | 2 | Expression | 0: function, 1: argument | codomain of function's syn Arrow |
| `Asc` | 2 | Expression, Pattern | 0: term, 1: type annotation (Type) | Surface type from position 1 |
| `Let` | 3 | Expression | 0: pattern (Pattern), 1: binding, 2: body | syn[2] (body's type) |
| `Identifier(String)` | 0 | Expression, Pattern | — | Unknown if Pattern; binding type if bound in Expression; Unknown if unbound in Expression |
| `Proj` | 2 | (transparent) | 0: projector type, 1: content | syn[1] |
| `Cursor` | 2 | (transparent) | 0: identity, 1: content | syn[1] |
| `Structural` | 0 | — | — | — |
| `Collapsed` | 0 | — | — | — |
| `Labeled` | 1 | — | 0: label term | — |
| `Canvas` | 1 | — | 0: position list | — |
| `PosNil` | 0 | — | — | — |
| `PosCons` | 4 | — | 0: nodeIdent, 1: x, 2: y, 3: tail | — |

**Identifier(String)**: The string value is part of the constructor itself. Identifiers are immutable — "editing" an identifier means deleting the old node and creating a new one with the modified string. Since grove nodes are never deleted, the old Identifier becomes an orphan.

**Multi-sort constructors** (Pair, Asc, Identifier): These are valid in multiple sort contexts. A `SortInconsistent` mark is generated only when a constructor appears in a sort it does NOT allow.

### 6.2 Sort System

Every position in the tree has an expected **sort**: `Type`, `Pattern`, or `Expression`. Sorts flow downward from parent to child:

- `Fun` position 0 is Pattern, position 1 is Expression.
- `Let` position 0 is Pattern, positions 1 and 2 are Expression.
- `Asc` position 1 is Type.
- `Arrow`, `Prod` children are Type.
- `Plus` children are Expression.
- `Pair` children inherit the parent's sort (Expression or Pattern).
- `Proj` and `Cursor` position 1 inherits the parent's sort (transparent).
- Projector metadata positions (Proj[0], Cursor[0], Labeled[0], Canvas[0], PosCons[*]) have no sort.
- `Root` position 0 is Expression.

### 6.3 Transparent Wrappers

`Proj` and `Cursor` are **type-transparent**: the type system treats them as if they aren't there.

- **Ana**: Position 1 inherits the parent's analyzed type and sort. Position 0 has no type constraints.
- **Syn**: The wrapper's synthesized type IS position 1's synthesized type. It is not cached independently — it must always be re-derived from the content.
- **Marks**: No marks are generated on the wrapper itself.

**Critical invariant**: Because transparent wrappers derive their type attributes entirely from their content, any change to the content's type MUST propagate through the wrapper to the wrapper's parent. An implementation that caches the wrapper's type and short-circuits propagation when the cache "looks unchanged" will fail when the underlying content has changed but the reference to it has not. The correct approach is to always propagate through transparent wrappers, or to ensure the equivalence check genuinely resolves the content's current type rather than comparing stale references.

### 6.4 Variable Binding

`Fun` and `Let` are the two binding forms. Each binds a pattern (position 0) over a body:

- **Fun(pattern, body)**: The pattern names the parameter. The body (position 1) is in scope of the binding.
- **Let(pattern, expr, body)**: The pattern names the bound variable. The body (position 2) is in scope. The binding expression (position 1) is NOT in scope of the pattern (no recursion).

**Scope rule**: An `Identifier(name)` in Expression sort is **bound** if there is an enclosing `Fun` or `Let` whose pattern contains an `Identifier` with the same name string. The **nearest** (innermost) such binder wins. Transparent wrappers (Proj, Cursor) do not affect scoping — they are invisible to binding resolution.

**Synthesized type of a bound Identifier**: A bound Identifier in Expression position synthesizes the type of its binding:

| Binder | Binding type |
|--------|-------------|
| `Fun(pat, body)` | The analytic type pushed to position 0 (i.e., the domain of the Arrow that the Fun is checked against). If the Fun's ana is not an Arrow, this is Unknown. |
| `Let(pat, expr, body)` | The synthesized type of position 1 (the binding expression). |

An unbound Identifier in Expression position synthesizes Unknown.

An Identifier in Pattern position always synthesizes Unknown (patterns introduce bindings, they don't look them up).

**Examples**:
- `Fun x ↦ x` checked against `Num → Num`: the body `x` synthesizes Num (domain of the Arrow).
- `Let x = (0 : Num) in x + 1`: the body `x` synthesizes Num (syn of the binding expression `0 : Num`).
- `Fun x ↦ Fun x ↦ x`: the inner `x` is bound to the inner Fun's parameter (shadowing).

---

## 7. Type System

### 7.1 Type Representation

Types are represented using the same grove terms (e.g., `Arrow`, `Num`, `Prod` nodes). A type reference can be:

- **Surface(term)**: Points to a term in the forest that IS a type. For example, if the user writes a type annotation `Num → Num`, the Arrow node in the forest is the type.
- **Synthetic(constructor, children)**: A type computed by the type checker, not corresponding to any forest node. Represented as a recursive structure: a constructor (e.g., `Arrow`) and a list of child type references (which can themselves be Surface, Synthetic, or Unknown). Used when the type checker infers types that the user hasn't written.
- **Unknown**: The gradual type `?`, consistent with everything. Represents missing type information.

These three forms are **exhaustive** — every type reference is one of these. `Unknown` is the default when no type information is available.

### 7.2 TypeAttribute

Each site (term or location) has a `TypeAttribute`:
- **sort**: Option — Type, Pattern, Expression, or absent (for metadata positions)
- **ana**: Option — the analyzed (expected) type flowing DOWN from parent, or absent
- **syn**: Option — the synthesized (inferred) type flowing UP from structure, or absent
- **marks**: list of error marks (`SortInconsistent`, `TypeInconsistent`)

`ana` and `syn` are absent (not Unknown) for sites that don't participate in the type system (e.g., Root, projector metadata). They are `Unknown` when the type system participates but can't determine a type. The distinction matters: absent means "not applicable," Unknown means "could be anything."

### 7.3 Bidirectional Type Rules

Type checking for a site computes its TypeAttribute from its context (parent's type expectations) and its structure (children's synthesized types).

**For a Location** (child slot): Compute the analyzed type the parent expects at this position, and read the synthesized type from the child (if present).

**For a Term** (node): Compute the synthesized type from the constructor and children, then check consistency against the analyzed type from the parent.

### 7.4 Analytic Type Flow

How the expected type flows down from parent to each child position:

| Parent Constructor | Position 0 ana | Position 1 ana | Position 2 ana |
|---|---|---|---|
| `Root` | Unknown (Expression) | — | — |
| `Plus` | Num | Num | — |
| `Prod` | Typ | Typ | — |
| `Pair` | left of matched Prod | right of matched Prod | — |
| `Arrow` | Typ | Typ | — |
| `Fun` | left of matched Arrow (Pattern sort) | right of matched Arrow (Expression sort) | — |
| `Ap` | Synthetic Arrow(Unknown, parent's ana) | domain of position 0's syn Arrow | — |
| `Asc` | parent's ana (inherits sort) | Typ | — |
| `Let` | Unknown (Pattern sort) | syn of position 0 | parent's ana (inherits) |
| `Proj` | (absent — metadata) | parent's ana (transparent) | — |
| `Cursor` | (absent — metadata) | parent's ana (transparent) | — |

"Matched Arrow/Prod" means: decompose the analyzed type into its components. If the type is not the expected shape (e.g., trying to match a Num as an Arrow), return `(Unknown, Unknown)`.

**Sibling dependencies**: Note that `Ap` position 1's ana depends on position 0's **syn**, and `Let` position 1's ana depends on position 0's **syn**. These are cross-sibling dependencies: one child's type information flows through the parent to another child. The worklist handles this through re-dirtying (see section 8.5).

### 7.5 Consistency Checking

After computing ana (expected) and syn (actual) for a term:
- If both are present, recursively compare constructors and children.
- If constructors differ → `TypeInconsistent` mark.
- `Unknown` is consistent with everything (gradual typing).
- If either is absent → no consistency check.

---

## 8. Incremental Type Checking

### 8.1 Overview

Type attributes are maintained incrementally via a worklist algorithm. When patches modify the grove, affected sites are **dirtied** (added to a priority queue ordered by document-order interval). The worklist is then drained, recomputing each site's type attributes and dirtying dependents if the result changed.

### 8.2 Deferred Recomputation

Applying patches does NOT automatically recompute types. Patches only dirty sites. Type recomputation is triggered explicitly — either one step at a time (for debugging/visualization) or all at once. **Type recomputation must complete before rendering** to ensure the UI shows consistent type information.

### 8.3 Propagation Rules

When a site is recomputed:
1. Compute the new TypeAttribute from the site's context and structure.
2. If the new attribute differs from the cached attribute, update the cache and dirty all dependent sites.
3. If the attribute is unchanged, stop — no further propagation needed.

**Dependents of a location**: the parent term + all child terms at that location.
**Dependents of a term**: the parent location + all child locations of that term + all use-site Identifiers connected via binding pointers (see section 8.6).

This means a change propagates: child term → parent location → parent term → sibling locations → sibling terms. Additionally, changes to a binder's type propagate directly to all use-sites via binding pointers, without traversing the intervening tree. The upward/downward/binding flows converge through re-dirtying.

### 8.4 Transparent Wrapper Warning

When comparing old and new TypeAttributes for Proj/Cursor nodes, the comparison must resolve any type references to their current values. A reference-level comparison (e.g., "both point to the same forest term") is insufficient because the referred term's structure may have changed. Failing to detect such changes will halt propagation prematurely.

### 8.5 Sibling Dependency Convergence

Some constructors have cross-sibling dependencies (e.g., `Ap` position 1's ana depends on position 0's syn). The worklist processes sites in interval order (roughly parents before children). When position 0 changes:

1. Position 0's syn changes → dirty position 0.
2. Position 0 is recomputed → no further change (it was just recomputed).
3. But position 0's change dirtied the parent term (Ap).
4. Ap is recomputed → notices position 0's syn changed → recomputes position 1's ana → dirtied position 1.
5. Position 1 is recomputed with correct ana.

This convergence takes multiple worklist items, not a single pass. The worklist may need to process the same site more than once within a single drain. This is correct and terminates because each recomputation either changes the attribute (making progress) or doesn't (stopping propagation).

### 8.6 Binding Pointers and Incremental Scope Resolution

Variable binding creates a non-local dependency: an Identifier's synthesized type depends on a binder that may be arbitrarily far up the tree. Naive resolution (walking up to find the binder on every recomputation) is too expensive. Instead, the system maintains **binding pointers** — direct links from use-site Identifiers to their binding-site patterns.

**Binding pointer maintenance**:
- When an Identifier in Expression position is recomputed, resolve its binding by walking up the tree through parent pointers, skipping transparent wrappers, looking for the nearest enclosing Fun/Let whose pattern has a matching name.
- Cache the result as a binding pointer from the use-site to the binding-site pattern.
- When the tree structure changes (patches add/remove nodes), binding pointers for affected subtrees must be revalidated.

**Dirty propagation through bindings**: When a binding site's type changes (e.g., the Fun's Arrow decomposition changes, or a Let's binding expression gets a new syn), all use-site Identifiers connected via binding pointers are dirtied. This is in addition to the standard parent/child dirty propagation.

The order maintenance data structure enables efficient binding resolution: a binder's interval contains its body's interval. To verify that a candidate binder actually encloses a use-site, check that the use-site's interval is contained within the binder's body interval. This is an O(1) comparison.

**Shadowing**: When multiple binders match the same name, the innermost one (whose interval most tightly contains the use-site) wins. The order maintenance intervals make this comparison efficient.

### 8.7 Unicycle Safety

Sites that are part of a unicycle (single-parent cycle) are skipped during worklist processing. Without this, type propagation through a cycle would diverge.

---

## 9. Cursor

```
Cursor(identity, content)  // arity 2
├─ position 0: Identifier (user UUID) — NEVER cursor-selectable
└─ position 1: content (the selected term, or empty for hole)
```

### 9.1 Invariants

- One cursor node per client session — never deleted/recreated, only moved.
- The identity node at position 0 is **stable** — it never moves. All cursor state is derived by walking parent pointers from the identity node.
- Cursor on `Proj` node ≠ cursor inside `Proj`'s content — these are distinct valid states.
- The cursor identity UUID is persisted per browser tab (e.g., in sessionStorage) to survive page reloads.

### 9.2 Cursor State Derivation

All cursor state is derived from the stable identity node:

```
identityNode                        // Identifier at Cursor[0] — stable reference
    → parent edge → source          // Cursor node, position 0
        → .node                     // The Cursor node itself
            → (node, position: 1)   // Cursor content location
            → parent edge → source  // Where cursor is attached in the tree
```

There is no local cursor position state. The grove IS the cursor state.

### 9.3 Three Primitive Patch Operations

All cursor movement is composed from three atomic primitives:

1. **Unwrap**: Move content from Cursor[1] to cursor's parent location.
   - If Cursor[1] is empty: no-op (no patches generated).
   - Delete edge: Cursor[1] → content.
   - Create edge: cursor's parent location → content.

2. **Move**: Relocate cursor node to a new location.
   - Delete edge: old parent location → Cursor.
   - Create edge: new location → Cursor.

3. **Wrap**: Move content at a location into Cursor[1].
   - Delete edge: location → content.
   - Create edge: Cursor[1] → content.

### 9.4 Movement Atomicity

**All patches generated by a single action MUST be applied to the grove before any re-rendering or type recomputation occurs.** Movements generate multiple patches (Unwrap + Move + Wrap). If intermediate states are visible (e.g., after Unwrap but before Wrap), the grove is in a valid but semantically incorrect state — content appears duplicated at the old location, or the cursor is detached. The implementation must apply all patches from a single action as a batch.

**Patch ordering within an action**: When an action creates a new node and then references that node's child locations, the patch that creates the node (connects it to a parent via a Live edge) must be applied BEFORE patches that reference the node's child locations. Otherwise the node doesn't exist when its locations are accessed.

### 9.5 Movement Specifications

**CRITICAL**: Every movement that arrives at a non-empty location MUST wrap the content there. Failing to wrap creates a conflict (cursor node and content as siblings at the same location), which causes content to appear duplicated or "reappear" after deletion.

**Up** (exit current node, wrap parent):
1. Let `target` = the node at cursor's parent location (i.e., the node the cursor is inside).
2. Let `grandparent_location` = the parent location of `target`.
3. If `grandparent_location` doesn't exist (at root): no-op.
4. Unwrap cursor content → goes to cursor's parent location.
5. Move cursor to `grandparent_location`.
6. Wrap `target` (which is at `grandparent_location`) into cursor.

**Down** (enter selected term's first child):
1. If cursor content is empty or a Reference: no-op.
2. Let `content` = cursor's content node.
3. If `content` has arity 0: no-op (nowhere to enter).
4. Determine target position: 1 for Proj/Cursor (skip metadata at 0), 0 otherwise.
5. Let `target_location` = `(content, target_position)`.
6. Unwrap cursor content → `content` goes to cursor's parent location.
7. Move cursor to `target_location`.
8. **If `target_location` has a child**: Wrap that child into cursor.

Note: Down through stacked transparent wrappers (e.g., `Proj(A, Proj(B, term))`) requires one Down press per layer. This is intentional — each Down descends exactly one constructor level.

**Right** (next sibling, cyclic):
1. Let `current_location` = cursor's parent location.
2. Compute `next_location` = next sibling of `current_location`, wrapping cyclically.
3. Skip position 0 of Proj/Cursor nodes (protected metadata).
4. If `next_location` = `current_location`: no-op.
5. Unwrap cursor content → goes to `current_location`.
6. Move cursor to `next_location`.
7. **If `next_location` has a child**: Wrap that child into cursor.

Note: Right inside a Proj is always a no-op (Proj has positions 0 and 1; position 0 is protected; Right from 1 skips 0 and returns to 1).

**MoveToTerm** (click on a term):
1. If the term is inside another user's cursor content: target the outer cursor's parent location instead, to avoid nesting cursors inside each other.
2. Unwrap cursor content.
3. Let `location` = the term's parent location.
4. Move cursor to `location`.
5. Wrap the term into cursor.

**MoveToLocation** (click on a hole or location):
1. If the location is inside another user's cursor content: do not enter it.
2. Unwrap cursor content.
3. Move cursor to the target location.
4. **If the location has a child**: Wrap that child into cursor.

### 9.6 Multi-User Cursors

- Each user has their own cursor with a unique identity UUID.
- All cursors are visible in the shared world tree.
- Cursors at the same location become siblings (Grove supports multiple children at one position).
- Own cursor vs. other cursors are visually distinguished by color.
- **Cursor nesting prohibition**: A user's cursor should never be placed inside another user's cursor content. MoveToTerm/MoveToLocation must guard against this (see 9.5).

### 9.7 Initialization

1. On startup, search the forest for all Cursor nodes (structural traversal — type information not needed).
2. Check if this session's cursor exists (by matching identity UUID from session storage).
3. If found: recover reference to identity node.
4. If not found: create new Cursor node with fresh identity at Root[0], emit patches. If Root[0] already has content, wrap it into the new cursor's content.

---

## 10. Projector

```
Proj(projector_type, content)  // arity 2
├─ position 0: metadata — NEVER cursor-selectable
└─ position 1: content (rendered recursively)
```

### 10.1 Projector Types

| Type       | Arity | Position 0 content      | Rendering |
|------------|-------|-------------------------|-----------|
| Structural | 0     | —                       | Default expanded view with badge |
| Collapsed  | 0     | —                       | Constructor name of content's root + "(...)", or just "(...)" if content is empty/conflict |
| Labeled    | 1     | label term (Identifier) | Label badge + expanded content |
| Canvas     | 1     | position list (PosCons) | Visual node-and-wire graph |

### 10.2 Invariants

- Arbitrary nesting: `Proj(A, Proj(B, term))` — outer adds frame, inner renders contents.
- Rendering is uniformly recursive.
- Cursors can rest on a Proj OR on its content — these are distinct valid states.
- Type transparency: Proj derives its type entirely from position 1 (see section 6.3).

### 10.3 Unrecognized Projector Types

If Proj[0] contains a constructor that is not a recognized projector type (Structural, Collapsed, Labeled, Canvas), the renderer should fall through to rendering the content at Proj[1] transparently — as if the Proj wrapper were not there. This handles edge cases where unexpected nodes end up at Proj[0] (e.g., from conflicts or user error).

### 10.4 Projector Creation

Creating a projector is an **atomic** operation — all patches must be generated and applied together:
1. Create the Proj node.
2. Create the projector type node at Proj[0].
3. For Canvas: also create PosNil at Canvas[0].
4. For Labeled: also create a default label Identifier at Labeled[0].
5. Move cursor's old content to Proj[1].

Atomicity prevents partial projector state from being visible to collaborators or to the type system.

### 10.5 Canvas Projector

Renders content as an interactive node-and-wire graph.

**Position map** stored in Grove as a linked list at Canvas[0]:
```
PosNil                                      // empty list (arity 0)
PosCons(nodeIdent, x, y, tail)             // list cell (arity 4)
  where nodeIdent, x, y are Identifier nodes with string values
```

**Graph collection** traverses the forest from the Proj[1] content location. Each direct child of the content becomes a top-level graph node. Recursion continues into each child:
- Cursor nodes: transparent (traverse into content).
- Proj nodes with non-Canvas projector: rendered inline as embedded, not traversed further.
- Leaf nodes and normal nodes: added as graph nodes with wires to their children.

Position persistence: positions are read from the PosCons chain on render, updated by rebuilding the entire chain when nodes are dragged.

---

## 11. Navigation

### 11.1 Protected Positions

Position 0 of Proj and Cursor is unreachable by keyboard cursor movement (Up/Down/Right). Only direct `MoveToLocation` (e.g., click) can access these positions.

### 11.2 Directions

- **Up**: toward root (exit current node, wrap parent)
- **Down**: toward leaves (enter first child; position 1 for Proj/Cursor, position 0 otherwise; no-op for arity-0 nodes)
- **Right**: next sibling (wraps cyclically, skipping protected position 0)

There is no **Left** direction — Right wraps cyclically through all sibling positions.

### 11.3 Auto-Advance After Wrapping

After `WrapLeft` and `WrapRight` actions, the cursor automatically moves Down into the newly created node's first available child. For zero-arity constructors, the Down is a no-op (cursor remains wrapping the new node).

---

## 12. Editing Actions

### 12.1 Action Types

- **WrapLeft(constructor)**: Create node in Cursor[1]. If cursor had content, move it to position 0 of the new node. Auto-advance Down.
- **WrapRight(constructor)**: Create node in Cursor[1]. If cursor had content, move it to position 1 of the new node. Auto-advance Down.
- **Insert(constructor)**: Create node at Cursor[1]. Only works when cursor content is empty.
- **Delete**: Delete all edges from Cursor[1] (content becomes orphaned, along with its entire subtree).
- **Cut**: Store cursor content's node UUID in local clipboard, delete edge from Cursor[1].
- **Paste**: Reconnect clipboard node to Cursor[1]. Only works when cursor content is empty.
- **Move(direction)**: See section 9.5.
- **MoveToTerm(term)**: See section 9.5.
- **MoveToLocation(location)**: See section 9.5.
- **WrapWithProjector(constructor)**: See section 10.4. This is DIFFERENT from WrapLeft/WrapRight Proj — it atomically creates the full projector structure.
- **TextInsert(char)**: If cursor content is an Identifier, delete it and create a new Identifier with the character appended. If cursor content is empty, create a new single-character Identifier. (This is a delete-and-recreate — the old Identifier becomes an orphan.)
- **TextBackspace**: If cursor content is an Identifier with length > 1, delete it and create a new Identifier with the last character removed. If length = 1, just delete.
- **UpdateStep**: Process one item from the type-checking worklist.
- **AllUpdateSteps**: Drain the entire worklist.
- **OpenReference(edge)**: Expand a collapsed reference in the forest.

### 12.2 Cut/Paste

The clipboard is **local-only** — it is not shared via Automerge. It stores a reference to a grove node UUID (which still exists in the grove as an orphan after being cut). Paste reconnects it.

### 12.3 Conflict Resolution

Conflicts (multiple children at one location) can be resolved by the user through Delete: navigate the cursor to one of the conflicting terms and delete it. There is no dedicated conflict resolution UI — the standard editing operations suffice.

---

## 13. Collaboration and Automerge

### 13.1 Automerge Document

The Automerge document stores grove patches as a flat map of patch IDs to serialized patch values. See **Appendix A** for the exact code.

**Patch ID format**: `"${sign}-${edge.id}"` where `sign` is the Sign enum value (`"Live"` or `"Dead"`) and `edge.id` is the edge UUID string. This deterministic ID ensures idempotency — the same patch always produces the same key.

**Patch value**: The grove patch object, JSON-serialized and wrapped in Automerge's `ImmutableString` type (which prevents Automerge from treating the string's characters as individually mergeable).

### 13.2 Patch Flow

```
User action
    → Controller generates patches
    → All patches applied to local WASM state as a batch (section 9.4)
    → Type recomputation triggered (AllUpdateSteps)
    → Re-render
    → Patches serialized and written to Automerge document
    → Automerge syncs to peers via WebSocket
    → Peers receive Automerge changes
    → Automerge emits change events containing "put" actions
    → Each "put" is converted back to a grove patch
    → Grove patches applied to peer's WASM state (idempotent for local patches)
    → Type recomputation + re-render on peer
```

### 13.3 Idempotency

Local patches are applied to WASM state immediately AND written to Automerge. When Automerge delivers them back (as a change event), they are applied again. This is safe because grove patch application is idempotent — applying a Live patch to an already-Live edge is a no-op. The worklist may see redundant dirty sites from double-application, but this only causes extra (harmless) recomputation.

### 13.4 Sync Toggle

A sync toggle allows pausing real-time sync. When paused, outgoing and incoming patches are queued. Resuming flushes both queues atomically.

### 13.5 Document Sharing

The Automerge document URL is stored in the browser's URL hash. Sharing the URL allows another browser to join the same collaborative session.

### 13.6 Bootstrap

1. Initialize WASM module.
2. Create Automerge repo with persistence (IndexedDB) and sync (WebSocket).
3. Load existing document from URL hash, or create new empty document.
4. Extract all existing patches from Automerge document, apply to WASM state. (Order doesn't matter — grove is commutative.)
5. Run AllUpdateSteps to compute initial types.
6. Initialize cursor (find existing or create new — see section 9.7).
7. Render.

---

## 14. Rendering

### 14.1 Render Trigger

Rendering occurs after every action that modifies the grove, AFTER type recomputation completes. The sequence is always: apply patches → run type updates → render.

### 14.2 Recursive Traversal

Rendering traverses the forest from the root location, producing a visual tree:

- **Holes** (0 children): Empty box placeholder.
- **Single child**: Render the term.
- **Multiple children** (conflict): Render all terms, visually bracketed.
- **Depth limit**: Rendering stops at a maximum depth to prevent infinite loops from cyclic references.

### 14.3 Constructor Rendering

| Constructor | Display |
|---|---|
| Typ | □ |
| Num | ℕ |
| Zero | 0 |
| Plus | (left + right) |
| Prod | (left × right) |
| Pair | (left, right) |
| Arrow | (left → right) |
| Fun | (fun left ↦ right) |
| Ap | (left ◁ right) |
| Asc | (left : right) |
| Let | let left = middle in right |
| Identifier | the string itself |
| Reference | 🌀 (clickable to open) |

### 14.4 Cursor Rendering

When the renderer encounters a Cursor node:
- Skip position 0 (identity — never rendered).
- Render position 1's content with cursor highlighting.
- If the cursor belongs to the local user: use primary cursor color.
- If the cursor belongs to another user: use secondary cursor color.
- If position 1 is empty: render a highlighted empty hole.

### 14.5 Highlighting

Every rendered node and hole is visually highlighted based on cursor, clipboard, and dirty state. Own cursor, other cursors, clipboard, and dirty sites each have distinct colors.

Every rendered node has a click handler for direct cursor navigation (MoveToTerm or MoveToLocation).

### 14.6 Inspector

During rendering, the type attributes (sort, ana, syn, marks) of the cursor's current site are captured and displayed in a status bar.

---

## 15. Open Problems

### 15.1 Not Yet Implemented in h4zel-mini

- **Live evaluation**: No elaboration or dynamic semantics.
- **Type hole inference**: No unification/constraint solving. Unknown types remain unknown.
- **Tile-based parsing**: Direct structure editing only.
- **Livelits**: No extensible literal/GUI system.
- **Selective patch withholding**: All patches sync unconditionally.

### 15.2 Research Open Problems

- **Side effects and fill-and-resume**: Commutativity theorem holds only for pure functional languages.
- **Scaling live evaluation**: Incremental evaluation, distributed scheduling, streaming datasets.
- **Syntax error recovery completeness**: Total recovery in tall tylr still underway.
- **Incremental marking + type inference integration**: Incrementalizing constraint gathering/unification.
- **Constraint solving under collaboration**: Interaction between conflict resolution, incremental propagation, and inference.
- **Polymorphic generalization with holes**: PotentialTypeSets + full System F.
- **Pattern matching with holes**: Exhaustiveness/redundancy checking in marked lambda calculus.
- **Dynamic semantics for groves**: Evaluating programs with unresolved conflicts.

---

## 16. Quick Reference

### Running the Prototype

```bash
# Build Rust/WASM
cd rust && wasm-pack build --target web

# Run TypeScript
cd ts && npm install && npm run dev
```

### Keyboard Shortcuts

| Key | Action | Effect |
|-----|--------|--------|
| a-z, A-Z | TextInsert | Append character to identifier (or create new) |
| 0 | Insert Zero | Create zero literal |
| + | WrapLeft Plus | Wrap with addition |
| * | WrapLeft Prod | Wrap with product type |
| , | WrapLeft Pair | Wrap with pair |
| - | WrapLeft Arrow | Wrap with arrow type |
| Space | WrapLeft Ap | Wrap with application |
| : | WrapLeft Asc | Wrap with type ascription |
| [ | WrapWithProjector Structural | Wrap with structural projector |
| Backspace | Delete | Delete cursor content |
| Shift+Backspace | TextBackspace | Delete last character of identifier |
| ↑ | Move Up | Exit current node |
| ↓ | Move Down | Enter first child |
| → | Move Right | Next sibling (cyclic) |
| Ctrl+X | Cut | Cut to local clipboard |
| Ctrl+V | Paste | Paste from local clipboard |
| Ctrl+T | Insert Typ | Insert type-of-types |
| Ctrl+N | Insert Num | Insert number type |
| Ctrl+P | WrapLeft Prod | Alt: wrap with product |
| Ctrl+F | WrapLeft Fun | Wrap with function (lambda) |
| Ctrl+L | WrapLeft Let | Wrap with let binding |
| Ctrl+U | UpdateStep | Run single type update step |
| Ctrl+Shift+S | Insert Structural | Insert structural projector type |
| Ctrl+Shift+C | Insert Collapsed | Insert collapsed projector type |
| Ctrl+Shift+G | Insert Canvas | Insert canvas/graph projector type |

---

## Appendix A: Automerge Integration (Reusable Code)

This appendix contains the exact Automerge integration code and dependency specification. This code is well-tested and should be reused rather than reimplemented — the Automerge API surface has non-obvious requirements (ImmutableString, patch format, change events) that are easy to get wrong.

### A.1 Dependencies

```json
{
  "dependencies": {
    "@automerge/react": "^2.3.1",
    "vite-plugin-top-level-await": "^1.6.0",
    "vite-plugin-wasm": "^3.5.0"
  }
}
```

The `vite-plugin-wasm` and `vite-plugin-top-level-await` plugins are required because the WASM module uses top-level await for initialization and Vite needs explicit WASM support.

### A.2 Document Schema and Patch Conversion (`Automerge.tsx`)

```typescript
import {
  type Patch as AmPatch,
  isImmutableString,
  ImmutableString,
  DocHandle,
} from "@automerge/react";

export { type Patch as AmPatch } from "@automerge/react";

/**
 * The Automerge document schema. A flat map from unique patch ID
 * to JSON-serialized grove patch wrapped in ImmutableString.
 *
 * ImmutableString is critical — without it, Automerge treats the
 * JSON string's characters as individually mergeable, which corrupts
 * the serialized patch data.
 */
export type GroveDoc = {
  grovePatches: {
    [patchId: string]: ImmutableString;
  };
};

/**
 * Deterministic patch ID. Since each edge has exactly two possible
 * patches (Live and Dead), the combination of sign + edge UUID is
 * globally unique.
 */
export function id_of_patch(patch: any) {
  return `${patch.sign}-${patch.edge.id}`;
}

/**
 * Write grove patches into an Automerge document.
 * Must be called inside handle.change() for atomicity.
 */
export function emitPatches(
  d: GroveDoc,
  patches: any[],
) {
  for (const patch of patches) {
    const patchId = id_of_patch(patch);
    d.grovePatches[patchId] = new ImmutableString(JSON.stringify(patch));
  }
}

/**
 * Extract all grove patches from an Automerge document.
 * Used during initialization to replay the full patch history.
 */
export function grovePatchesFromDocHandle(doc: DocHandle<GroveDoc>): any[] {
  const result = [];
  for (const [_patchId, serializedPatch] of Object.entries(
    doc.doc().grovePatches,
  )) {
    try {
      const patch = JSON.parse(serializedPatch.toString());
      result.push(patch);
    } catch (error) {
      console.error(`Error parsing patch ${_patchId}: ${error}`);
    }
  }
  return result;
}

/**
 * Convert an Automerge change event patch to a grove patch.
 *
 * Automerge emits "put" actions when entries are added to a map:
 * {
 *   action: "put",
 *   path: ["grovePatches", "<patchId>"],
 *   value: ImmutableString("<JSON serialized grove patch>")
 * }
 *
 * Returns the deserialized grove patch, or undefined if the
 * Automerge patch is not a grove patch (e.g., structural changes).
 */
export function amPatchToGrovePatch(amPatch: AmPatch): any | undefined {
  if (
    amPatch.action === "put" &&
    amPatch.path[0] === "grovePatches" &&
    amPatch.path.length === 2 &&
    typeof amPatch.path[1] === "string"
  ) {
    if (!isImmutableString(amPatch.value)) {
      throw new Error("patches should be an ImmutableString");
    }
    const patch = JSON.parse(amPatch.value.val);
    return patch;
  }
  return;
}
```

### A.3 Bootstrap (`main.tsx`)

```typescript
import { StrictMode } from "react";
import { createRoot } from "react-dom/client";
import App from "./App.tsx";
import {
  DocHandle,
  ImmutableString,
  IndexedDBStorageAdapter,
  isValidAutomergeUrl,
  Repo,
  RepoContext,
  WebSocketClientAdapter,
} from "@automerge/react";

// Create Automerge repo with browser persistence and WebSocket sync
const repo = new Repo({
  storage: new IndexedDBStorageAdapter(),
  network: [new WebSocketClientAdapter("wss://sync3.automerge.org")],
});

let handle: DocHandle<{ grovePatches: Record<string, ImmutableString> }>;

// Load existing document from URL hash, or create new
const locationHash = document.location.hash.substring(1);
if (isValidAutomergeUrl(locationHash)) {
  handle = await repo.find(locationHash);
} else {
  handle = repo.create({ grovePatches: {} });
  document.location.hash = handle.url;
}

// The App component receives the handle and registers for change events
createRoot(document.getElementById("root")!).render(
  <RepoContext.Provider value={repo}>
    <StrictMode>
      <App handle={handle} />
    </StrictMode>
  </RepoContext.Provider>,
);
```

### A.4 App-Level Patch Handling Pattern

The App component should:

1. **On initialization**: Call `grovePatchesFromDocHandle(handle)` to get all existing patches, apply them to WASM state.

2. **On user action**: Generate patches via Controller, apply to WASM state, then write to Automerge:
   ```typescript
   handle.change((d) => emitPatches(d, patches));
   ```

3. **On Automerge change event**: Register a callback on `handle` that fires when remote changes arrive:
   ```typescript
   handle.on("change", ({ patches: amPatches }) => {
     for (const amPatch of amPatches) {
       const grovePatch = amPatchToGrovePatch(amPatch);
       if (grovePatch) {
         controller.applyPatch(grovePatch);  // Idempotent
       }
     }
     controller.runAllUpdates();
     rerender();
   });
   ```

4. **Sync toggle**: When sync is paused, queue outgoing patches instead of calling `handle.change()`, and queue incoming patches instead of applying them. On resume, flush both queues in a single `handle.change()` call.

### A.5 TypeScript Type Definitions (`RustTypes.tsx`)

These types mirror the Rust enum serialization format (serde) used across the WASM boundary:

```typescript
export type Constructor =
    | "Typ" | "Num" | "Zero" | "Plus" | "Prod" | "Pair"
    | "Arrow" | "Fun" | "Asc" | "Ap" | "Let"
    | { Identifier: string }
    | "Proj" | "Structural" | "Collapsed" | "Labeled" | "Canvas"
    | "PosNil" | "PosCons"
    | "Cursor"

export type GroveConstructor =
    | "Root"
    | { Lang: Constructor }

export type TermConstructor =
    | { Constructor: GroveConstructor }
    | { Reference: any }  // TermEdge

export type ForestAction =
    | { OpenReference: any }  // TermEdge

export type BlossomAction =
    | { ForestAction: ForestAction }
    | "AllUpdateSteps"
    | "UpdateStep"

export type Direction = "Up" | "Down" | "Right"

export type Action =
    | { BlossomAction: BlossomAction }
    | { WrapLeft: Constructor }
    | { WrapRight: Constructor }
    | { Insert: Constructor }
    | "Delete"
    | { Move: Direction }
    | "Cut" | "Paste"
    | { MoveToLocation: any }  // TermLocation
    | { MoveToTerm: any }      // Term
    | { TextInsert: string }
    | "TextBackspace"
    | { WrapWithProjector: Constructor }

export type Sort = "Type" | "Pattern" | "Expression"

export type Mark =
    | { SortInconsistent: [Sort, Sort] }
    | { TypeInconsistent: [any, any] }  // [TypeLocation, TypeLocation]
```

These type definitions are dictated by Rust's `serde` serialization of enums: unit variants become strings, tuple/struct variants become `{ VariantName: data }` objects. This format is not negotiable — it must match whatever the Rust side produces.
