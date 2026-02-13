# Design Specifications

Consensus specifications for subsystems. Update this document when designs are clarified or modified.

---

## Core Principle: UNIFORMITY

No special cases. Holes are just absence of children. Projectors are just nodes. Cursors are just nodes. Recursive rendering handles nesting naturally. The world tree representation is intentionally free/unconstrained — constraints live in editor behavior, not data structure.

---

## Cursor

```
Cursor(identity, content)  // arity 2
├─ position 0: Identifier (user UUID) — NEVER cursor-selectable
└─ position 1: content location
```

**Invariants:**
- Cursor on `Proj` node ≠ cursor inside `Proj`'s content (distinct valid states)
- No local cursor state — derive all from stable `myIdentityNode` reference
- Identity node captured once at init; cursor node = `identityNode.parent`
- One cursor node per client session — never delete/recreate, only move

**Operations:**
1. Unwrap: move content from cursor to cursor's parent
2. Move: relocate cursor node
3. Wrap: move target into cursor's content

**Type transparency:** Cursor passes parent type constraints through to position 1. Position 0 has no type constraints.

---

## Projector

```
Proj(projector_type, content)  // arity 2
├─ position 0: metadata — NEVER cursor-selectable
└─ position 1: content (rendered recursively)
```

**Projector types:**

| Type       | Arity | Position 0 content      |
|------------|-------|-------------------------|
| Structural | 0     | —                       |
| Collapsed  | 0     | —                       |
| Labeled    | 1     | label term              |
| Canvas     | 1     | position list (PosCons) |

**Invariants:**
- Arbitrary nesting: `Proj(A, Proj(B, term))` — outer adds frame, inner renders contents
- Rendering is uniformly recursive
- Cursors can rest on a Proj OR on its content — these are distinct

**Type transparency:** Proj passes parent type constraints through to position 1. Position 0 has no type constraints.

---

## Navigation

**Protected positions:** Position 0 of Proj and Cursor is unreachable by cursor movement.

**Enforcement:** Navigation logic explicitly skips position 0 for Proj/Cursor constructors. No structural enforcement — the world tree stays free.

**Directions:**
- Up: toward root
- Down: toward leaves (first child, skipping protected position 0)
- Right: next sibling (wraps cyclically, skipping protected position 0)

---

## Canvas Position Map

```
Canvas(positionList)  // arity 1
  where positionList = PosCons(nodeIdent, x, y, tail) | PosNil

PosNil   // arity 0 — empty list
PosCons  // arity 4 — (nodeIdent, x, y, tail)
```

- nodeIdent, x, y are Identifier nodes with string values
- Position stored structurally in world tree, not external state
