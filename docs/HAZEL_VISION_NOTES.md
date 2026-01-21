# Hazel Vision and Architecture Notes

Notes for Claude to get up to speed on the Hazel project vision and current prototype.

## Overview

Hazel is a live functional programming environment where every incomplete program is both statically and dynamically well-defined. The key innovation is **typed holes** - every editor state has meaning, enabling continuous feedback during development.

## Key Papers

### 1. Vision Paper (propl24.pdf)
The PROPL 2024 vision paper outlines the goal of "totally live programming" - a system where:
- There are no meaningless editor states
- Every incomplete program has a type (possibly incomplete)
- Every incomplete program can run (producing possibly incomplete results)
- Typed holes serve as placeholders for missing code and containers for erroneous code

### 2. Grove Paper (POPL 2025)
Grove is a **bidirectionally typed collaborative structure editor calculus**. Key contributions:

- **Graph-based representation**: Code is represented as a labeled directed multi-graph with uniquely identified vertices (nodes) and edges
- **Commutative edits**: All edits are edge insertions/deletions that commute, forming a CmRDT (Commutative Replicated Data Type)
- **No patch synthesis or 3-way merge**: Patches come directly from the edit log, eliminating heuristic diff/merge algorithms
- **Conflict representation**: Conflicts (e.g., from code relocation) appear as locations with multiple edges, explicitly represented in the graph
- **Total type system**: Even conflicted states have a well-defined type via a marking system with gradual typing

### 3. Incremental Bidirectional Typing (arxiv 2504.08946)
This paper addresses efficient type checking in live environments:

- **Small-step dynamics**: Type updates propagate incrementally through the marked/annotated program
- **Order maintenance**: Data structures maintain pointers connecting bound variables to binding locations
- **Dramatic speedups**: 275.96x faster than from-scratch reanalysis via prioritized update propagation
- **Agda verification**: Correctness proven equivalent to naive re-analysis

## Current Prototype Architecture (h4zel-mini)

### Layer Stack

```
                    +------------------+
                    |     App.tsx      |  React UI, keyboard handling
                    +------------------+
                            |
                    +------------------+
                    |   Controller.ts  |  Cursor, clipboard, actions (cursor to move to world tree)
                    +------------------+
                            |
                    +------------------+
                    |   Render.tsx     |  Rendering, projector views
                    +------------------+
                            |
                    +------------------+
                    |   blossom.rs     |  Type checking, worklist updates
                    +------------------+
                            |
                    +------------------+
                    |   forest.rs      |  Forest: decomposition layer (tree view of grove)
                    +------------------+
                            |
                    +------------------+
                    |   grove.rs       |  Grove: CRDT graph data structure
                    +------------------+

                    The world tree = this instance of the grove
```

## Terminology

- **Grove**: The CRDT - the graph data structure with commutative operations
- **Forest**: The "decomposition" layer that presents the grove as a tree (handles unfolding references, path hashes)
- **World Tree**: The specific instance of the grove in this prototype and in the Hazel vision - the particular database of code that everyone collaborates on

### grove.rs - Graph Foundation (Grove Layer)

The core Grove data structure:

- **Node**: Identified by UUID, has a constructor (Root or Lang)
- **Edge**: Connects a Location (node + position) to a Node, has sign (Live/Dead)
- **Patch**: Atomic edit operation with edge, source, destination, sign
- **State**: Tracks parents, children, constructors, is_root, is_in_unicycle

Key insight: Patches are commutative - applying them in any order yields the same state.

### forest.rs - Decomposition Layer (Forest Layer)

Wraps grove, presenting a tree interface (the "decomposition" from the paper):

- **TermNode**: Node + path hash (for handling references/cycles)
- **TermEdge**: Edge + path hash
- **TermLocation**: Location within a term
- **Path handling**: Tracks which references are "opened" to present cyclic graphs as trees

The path hash mechanism handles the key challenge: the underlying graph may have cycles or multi-parent nodes, but we want to present it as a tree for navigation and display.

### blossom.rs - Type Checking Layer

Incremental type attribute updates:

- **TypeAttribute**: sort (Type/Pattern/Expression), ana (expected type), syn (synthesized type), marks
- **Worklist**: Priority queue ordered by interval position for correct update propagation
- **Dirty mechanism**: When patches arrive, dirty sites are added to worklist

### types.rs - Type System

Bidirectional type checking implementation:

- **Sort**: Type, Pattern, or Expression
- **Type/TypeLocation**: Either Surface (from term tree) or Synthetic (computed)
- **Marks**: SortInconsistent, TypeInconsistent
- **compute_ana/compute_syn**: The bidirectional type checking rules

### lang.rs - Language Constructors

The object language:
- Types: Typ, Num
- Expressions: Zero, Plus, Prod, Pair, Arrow, Fun, Ap, Asc, Let, Identifier
- **Projectors**: Proj (wrapper), Structural, Collapsed, Labeled

## Projectors - Key Current Work

Projectors enable different views of subterms:

### Architecture

A `Proj` node has two children:
1. Position 0: Projector type (Structural, Collapsed, or Labeled)
2. Position 1: The wrapped term

### Current Projector Types

1. **Structural**: Default expanded view, shows child normally
2. **Collapsed**: Shows only constructor name with "(...)" for children
3. **Labeled**: Shows a label badge + the expanded content
4. **Canvas**: Visual graph view with draggable nodes and wires

### Canvas Projector

The Canvas projector renders its content as a visual graph:

- **Nodes**: Rendered as boxes that can be dragged
- **Wires**: Connect related terms (e.g., function applications to their arguments)
- **Position map**: Stored structurally in the world tree as a linked list

#### Position Map Structure

Positions are stored as a linked list using `PosNil` and `PosCons` constructors:

```
Canvas(content, positionMap)
  where positionMap = PosCons(nodeId, x, y, tail) | PosNil
```

- `PosNil`: Empty position list (arity 0)
- `PosCons`: Position entry (arity 4): nodeId (Identifier), x (Identifier), y (Identifier), tail
- X and Y are stored as Identifier nodes with string representations of numbers

#### Implementation

- `ts/src/CanvasProjector.tsx`: React component with ReactFlow
- Position reading: `readPositionsFromGrove()` parses the linked list
- Position writing: `createPositionMapPatches()` in Controller.ts
- Auto-layout: Dagre algorithm for initial positions
- Drag state: Local overlay during drag, written to Grove on drop

### Implementation (Render.tsx)

- `render_proj()`: Dispatches based on projector type
- `get_projector_info()`: Extracts projector type from Proj node
- `change_projector_type()`: Toggles between Structural/Collapsed
- `edit_label()`: Navigates cursor to label for editing

### Collaboration Model

**Key principle**: Everything that could potentially be shared between collaborators must live in the world tree (the Grove data structure). This includes:
- The program itself (terms, types, etc.)
- Cursor positions
- Projector state (which projectors exist, their types, labels, etc.)

### Cursor Implementation (Implemented)

The cursor is now a binary node in the Grove: `Cursor(identity, content)` with arity 2:
- **Position 0**: Identity - an Identifier node containing a UUID string unique to each client
- **Position 1**: Content - the selected term/subtree (or empty for hole selection)

#### Key Design Principles

1. **Stable cursor node**: Each client creates ONE cursor node on startup. This node's ID never changes. Movement operations unwrap/rewrap content, they don't delete/recreate the cursor.

2. **Local vs Grove cursor**:
   - `this.cursor` (local): Tracks navigation position WITHIN the cursor's content
   - Grove Cursor node: Represents what the user is "selecting" in the shared world tree
   - Local navigation within cursor content doesn't emit patches

3. **Three cursor operations** (all use stable node IDs):
   - `createUnwrapPatches()`: Move content from cursor-content to cursor's parent
   - `createMoveCursorNodePatches(location)`: Move cursor node to new location
   - `createWrapPatches(node)`: Move target node into cursor-content

4. **Transparent wrapper**: Like Proj nodes, Cursor nodes are "transparent" for:
   - Navigation: Users see through the cursor to its content
   - Type checking: Cursor passes through types unchanged
   - Rendering: Content is rendered with cursor highlighting

#### Multi-User Cursors

- Each user has their own cursor with a unique identity
- All cursors are visible in the shared world tree
- Own cursor: highlighted in cyan (`cursor_color`)
- Other cursors: highlighted in orange (`other_cursor_color`)
- Cursors at the same location become siblings (Grove supports multiple children)

#### Implementation Files

- `rust/src/lang.rs`: Cursor constructor (arity 2)
- `rust/src/types.rs`: Cursor type rules (transparent like Proj)
- `ts/src/Controller.ts`: Cursor patch creation, local navigation
- `ts/src/Render.tsx`: `render_cursor()`, color differentiation
- `ts/src/App.tsx`: Initial cursor creation, sync

The Grove architecture supports **selective patch withholding**: collaborators can choose not to share certain patches to enable differing views. For example:
- User A might collapse a function while User B keeps it expanded
- Both states exist in each user's local world tree
- Patches for projector type changes can be withheld from sync

This is different from having "shared" vs "non-shared" data. All data is *shareable* by virtue of living in the world tree; the choice of what to actually sync is a separate concern handled at the collaboration layer.

## Automerge Integration

The prototype uses Automerge for CRDT-based collaboration:

- `Automerge.tsx`: Converts between Automerge patches and Grove patches
- `App.tsx`: Handles sync (autoSync flag), queuing when disabled
- **ImmutableString**: Grove patches stored as JSON strings in Automerge doc

The `id_of_patch` function creates deterministic IDs so patches are idempotent.

## Key Technical Concepts

### Commutativity

All Grove edits commute because:
- Edges have unique IDs
- Insertion is adding to a set
- Deletion is permanent (Dead state)
- Edge lifecycle: None -> Live -> Dead (skip-life: None -> Dead also valid)

### Path Hashes

When navigating through references (cyclic structures), path hashes track:
- Which "unfolding" of the cycle we're in
- Allows presenting infinite structures as finite views
- `OpenReference` action extends the visible unfolding

### Interval-Based Update Ordering

From the incremental typing paper:
- Each term site has an interval (start, end orders)
- Updates processed in interval order ensure correct information flow
- Order maintenance data structures enable efficient splitting

## Future Directions

Based on the vision and current state:

1. ~~**Cursor in the world tree**~~: DONE - Cursor is now a `Cursor(identity, content)` node in Grove
2. **Selective sync layer**: Implement patch filtering at the collaboration layer for differing views
3. **More projector types**: Color coding, different syntax views, slider projectors, etc.
4. **Canvas projector improvements**: Position map is structural (PosCons linked list), drag-and-drop working
5. **Evaluation**: The "live" part - running incomplete programs with holes
6. **Type inference**: Currently marks errors, could infer more via unification
7. **Pattern matching**: Full pattern support in the language

## Quick Reference

### Running the Prototype

```bash
# Build Rust/WASM
cd rust && wasm-pack build --target web

# Run TypeScript
cd ts && npm install && npm run dev
```

### Key Keyboard Shortcuts

- Arrow keys: Navigate cursor
- Alphabet: Insert identifier
- 0: Insert zero
- +, *, space, :, etc.: Wrap with operators
- Ctrl+f: Wrap with fun (lambda)
- Ctrl+l: Wrap with let
- [: Wrap with Proj
- Ctrl+Shift+S: Insert Structural
- Ctrl+Shift+C: Insert Collapsed
- Ctrl+x/v: Cut/paste
- Ctrl+u: Single update step

### Important Files for Getting Started

1. `rust/src/grove.rs` - Core data structure
2. `rust/src/types.rs` - Type checking rules
3. `ts/src/Render.tsx` - How things display
4. `ts/src/App.tsx` - Main application logic
