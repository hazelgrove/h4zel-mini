# Hazel: A Comprehensive Technical Report

## System Architecture, Data Structures, and Open Problems

---

## 0. Authorship

This summary was written by Claude Opus 4.6 based on these papers:

Hazel vision:

https://arxiv.org/pdf/1703.08694

https://hazel.org/papers/propl24.pdf

https://hazel.org/papers/fairground-propl25.pdf

Hazel core: 

https://hazel.org/papers/marking-popl24.pdf

https://arxiv.org/pdf/1805.00155

Hazel system:

https://hazel.org/papers/grove-popl25.pdf

https://hazel.org/papers/livelits-pldi2021.pdf

https://arxiv.org/pdf/2508.16848

https://arxiv.org/pdf/2504.08946

## 1. Executive Summary

Hazel is a research programming environment whose central thesis is that **every editor state should be semantically meaningful**—syntactically, statically, and dynamically—without exception. This thesis, called the *continuity invariant*, eliminates the "gap problem" whereby editor services degrade or vanish when the programmer's code is incomplete or ill-typed. Hazel achieves this through a tightly co-designed stack of formal calculi, each verified in the Agda proof assistant, spanning syntax recovery, type error localization, live evaluation, collaborative editing, extensible literal notation, and incremental analysis.

The system is implemented as a browser-based live notebook environment (OCaml compiled to JavaScript via js_of_ocaml) for a typed functional language in the Elm/ML family, and is evolving toward a planetary-scale collaborative scientific computing platform called *Fairground*.

---

## 2. The Vision: Why Hazel Exists

### 2.1 The Gap Problem (SNAPL 2017)

Programming languages assign formal meaning only to complete programs. But programmers spend substantial time interacting with *incomplete* programs—programs with missing expressions, type errors, and binding inconsistencies. When a program is incomplete, conventional tools disable editor services precisely when the programmer needs them most. This creates three classes of gaps:

1. **Syntactic gaps**: The edit state is not well-formed according to the grammar.
2. **Static gaps**: The program is syntactically well-formed but ill-typed (formally meaningless under a standard type system).
3. **Dynamic gaps**: Even if types are recoverable, a conventional dynamic semantics cannot evaluate a program with holes or type errors.

Hazel's research program attacks all three, aiming for a system where *every reachable editor state* has a well-defined syntax tree, a well-defined type, and can be evaluated to produce a (possibly indeterminate) result.

### 2.2 The Planetary Computing Vision (PROPL 2024, PROPL 2025)

The long-term vision extends Hazel from a single-user research tool to a collaborative planetary compute engine, initially called "Planet Hazel" and more recently *Fairground*. The key properties are:

- **Live**: Continuous feedback about dynamic behavior during editing; no "batch" model. The entire program re-evaluates incrementally as edits occur.
- **Rich**: Programs embed domain-specific visualizations and direct-manipulation GUIs (via *livelits*), rather than being restricted to plain text.
- **Composable**: A pure functional core (products, sums, functions) enables fusion, parallelization, and distribution. Every notebook is a library that exports top-level definitions; the entire system forms a single live program.
- **Collaborative**: Multiple stakeholders—scientists, policymakers, journalists—work within a shared environment. One user's localized error cannot break the system for others.
- **FAIR by default**: Findability, Accessibility, Interoperability, and Reproducibility are structural properties of the platform, not extra effort.

Fairground introduces *Fair Python*, a purely functional dataflow subset of Python, with a sandboxed foreign function interface (FFI) for interoperating with existing code in arbitrary languages (configured via Nix for reproducibility). A distributed parallelizing scheduler executes the planetary-scale program as it is collaboratively edited.

---

## 3. Core Formal Foundations

### 3.1 The Marked Lambda Calculus (POPL 2024)

The marked lambda calculus is the theoretical backbone of Hazel's static semantics. It provides the first comprehensive formal account of **total type error localization and recovery**.

#### 3.1.1 Two Expression Languages

The calculus defines two parallel expression languages:

- **Unmarked expressions** (`e`): The "user's program"—ordinary expressions including empty holes (`⟨⟩`).
- **Marked expressions** (`ě`): Mirrors the unmarked language but extended with *error marks*—syntactic membranes around erroneous sub-expressions that formalize "red squiggles."

Each error mark corresponds to a distinct error class:
- `⟨x⟩□` — free variable
- `⟨ě⟩≁` — inconsistent types (analytic subsumption failure)
- `⟨λx:τ.ě⟩:` — inconsistent type annotation on a lambda parameter
- `⟨λx:τ.ě⟩⇐▶̸→` — lambda analyzed against a non-arrow type
- `⟨ě⟩⇒▶̸→ ě` — application of a non-function
- `⟨if ě then ě else ě⟩̸⊓` — inconsistent branch types
- (analogous marks for product mismatches, polymorphism mismatches, etc.)

#### 3.1.2 The Marking Judgment

The central operation is **marking**: a total, deterministic function that transforms any unmarked expression into a marked expression with errors localized. It is defined bidirectionally:

- **Synthetic marking**: `Γ ⊢ e ↬ ě ⇒ τ` (infer a type, insert marks)
- **Analytic marking**: `Γ ⊢ e ↬ ě ⇐ τ` (check against a type, insert marks)

The key metatheorems, all mechanized in Agda:

| Property | Statement |
|---|---|
| **Totality** | Every syntactically well-formed expression can be marked, yielding a well-typed marked expression. |
| **Well-formedness** | Mark erasure recovers the original expression; the marked expression is well-typed in the marked type system. |
| **Soundness** | Well-typed expressions are marked without any error marks. Ill-typed expressions receive at least one mark. |
| **Unicity** | Marking is deterministic (a total function). |

#### 3.1.3 Gradual Typing as Recovery Mechanism

When a mark is inserted (e.g., a free variable), the surrounding type information is lost. Recovery uses the **unknown type** `?` from gradual type theory (identified with "type holes"). The unknown type is consistent with every type, allowing type checking to proceed optimistically past errors. Type consistency replaces type equality:

```
? ~ τ    τ ~ ?    τ ~ τ    (τ₁→τ₂) ~ (τ₁'→τ₂')  if  τ₁~τ₁' and τ₂~τ₂'
```

Matched arrow types (extracting arrow structure from `?`) allow applications and lambdas to type-check even when the function type is unknown.

#### 3.1.4 Type Hole Inference

On top of the bidirectional core, a **constraint-based unification layer** attempts to fill type holes. Each unknown type carries a *provenance* linking it to its origin (a type hole or expression hole). After bidirectional marking, constraints are gathered and unified. When constraints conflict, the system does not guess—instead, the conflict is **localized to the hole itself**, and the user interactively selects from *partially consistent fillings*. Hovering over a filling temporarily fills the type hole and returns control to the bidirectional system, which marks errors accordingly.

This "neutral" approach avoids the notorious problem of ML-family languages where constraint solvers arbitrarily blame one expression among several conflicting uses. The data structure for tracking partial solutions is a **PotentialTypeSet**: a recursive set of potential types that survive unification merging, extended with an `etc` constructor representing latent constraints from unfilled expression holes (which prevents premature polymorphic generalization).

### 3.2 Live Evaluation with Typed Holes (POPL 2019)

The *Hazelnut Live* calculus defines a dynamic semantics for incomplete programs—the theoretical basis for Hazel's live programming.

#### 3.2.1 External vs. Internal Expressions

Programs pass through an **elaboration** phase that transforms external expressions (as entered by the user) into internal expressions suitable for evaluation. Elaboration:

1. Initializes **hole closures**: each hole `⟨⟩ᵘ` becomes `⟨⟩ᵘ_σ` where `σ = id(Γ)` is the identity substitution for the local typing context. Non-empty holes `⟨e⟩ᵘ` similarly become `⟨d⟩ᵘ_σ`.
2. Inserts **casts** `d⟨τ₁ ⇒ τ₂⟩` wherever type holes require deferred structural checks, following the gradually typed lambda calculus.

The **hole context** `Δ` records each hole's type and typing context, borrowing notation from contextual modal type theory (CMTT).

#### 3.2.2 Evaluation Around Holes

Instead of aborting at holes (the "exceptional approach"), evaluation **continues around them**. The dynamic semantics distinguishes three classes of irreducible forms:

- **Values**: constants, lambdas (and boxed variants under casts).
- **Indeterminate forms**: rooted at a hole closure or failed cast. An empty hole closure `⟨⟩ᵘ_σ` is always indeterminate. Applications `d₁(d₂)` where `d₁` is indeterminate become indeterminate. Failed casts `d⟨τ₁ ⇒ ⟨⟩ ⇏ τ₂⟩` (where a ground type cast failed) are indeterminate rather than exceptional.

The progress theorem is generalized: for any well-typed internal expression, either it steps, it is a (boxed) value, or it is indeterminate.

#### 3.2.3 Hole Closures and the Live Context Inspector

As evaluation proceeds, substitutions around holes are recorded in hole closure environments. When `(λx:b. ⟨⟩ᵘ_{x/x, y/y})(c)` steps, the result is `⟨⟩ᵘ_{c/x, y/y}`. Multiple closures for the same hole arise from multiple calls (e.g., mapping an incomplete function over a list). The **live context inspector** UI presents the names, types, and *runtime values* of variables in scope at a selected hole instance.

#### 3.2.4 Fill-and-Resume

When the programmer fills a hole, evaluation need not restart from scratch. The **hole filling operation** `⟦d/u⟧d'`, based on CMTT's contextual substitution, replays the recorded substitutions:

```
⟦d/u⟧⟨⟩ᵘ_σ = [⟦d/u⟧σ]d     (apply delayed substitution to fill expression)
```

A **commutativity theorem** ensures correctness: if `d₁` steps to `d₂`, then filling at either end yields equivalent results (`⟦d'/u⟧d₁ →* ⟦d'/u⟧d₂`). This holds for pure functional languages. Non-commutative effects break this property—handling them is an identified open problem.

#### 3.2.5 The Continuity Invariant

Composing the structure editor's *Sensibility* property (every edit produces a well-typed program sketch) with elaboration, typed elaboration, preservation, and progress yields:

> **Corollary (Continuity):** After every edit action, the editor state can be elaborated to a well-typed internal expression that either steps (preserving its type), is a boxed value, or is indeterminate.

This formally solves the gap problem: every reachable editor state has both static and dynamic meaning.

---

## 4. The Hazel System

### 4.1 Syntax: Tile-Based Parsing (tall tylr, 2025)

Hazel's editor uses a **tile-based** approach to syntax. Rather than traditional parsing from a text buffer, the system uses an error-handling parser/editor generator called *tall tylr*. It introduces **syntactic obligations**—generalizations of holes that cover missing operands, operators, delimiters, and sort transitions. The key ideas:

- A **molder** turns tokens into *tiles* (typed pieces of syntax).
- A **melder** completes and parses tiles into terms using an error-handling generalization of operator precedence parsing.
- Obligations abstract over many possible completions, avoiding the proliferation of multi-option repairs that plagues traditional error recovery.

This ensures that **every edit state maps to a syntactically well-formed program sketch**, the prerequisite for the marking and evaluation pipeline.

### 4.2 Collaborative Editing: Grove (POPL 2025)

Grove is a collaborative structure editor calculus that eliminates patch synthesis (diff) and three-way merge entirely.

#### 4.2.1 Core Data Structure: Labeled Directed Multi-Graph

The edit state is not a single tree but a **directed labeled multi-graph** where:
- **Vertices** are labeled with a unique identifier (UID) and a constructor (e.g., `Plus`, `Var(x)`).
- **Edges** are labeled with a UID and establish parent-child relationships at a labeled position (e.g., `L`, `R` for `Plus`).

#### 4.2.2 Commutative Patch Language (CmRDT)

The patch language has only two commands: **edge insertion** and **edge deletion** (permanent—a 2P-Set CRDT). All edits commute, eliminating the need for operational transforms. A convergence theorem ensures branches converge to the same state regardless of patch application order.

Relocation is simply edge deletion + edge insertion. Crucially, vertices are never deleted, so heuristics for identifying relocated code are unnecessary.

#### 4.2.3 Decomposition into Groves

Since tree-based editors and type systems expect trees, the graph is **decomposed into a grove**—a set of trees with:
- **Holes**: locations with no out-edges.
- **Local conflicts**: locations with multiple out-edges (multiple children competing for the same position).
- **Relocation conflict references**: vertices with multiple in-edges (multiple parents claiming the same node).
- **Unicycle conflict references**: cycles broken at a deterministically chosen edge.

Resolving conflicts amounts to manipulating these constructs as ordinary syntax (e.g., deleting all but one relocation conflict reference).

#### 4.2.4 Typing Groves

A bidirectional marking system is extended for groves. Conflicts and holes are handled using the same gradual typing machinery—when a conflict prevents determining a single type, the unknown type `?` is used, and downstream typing proceeds. A unification-based inference layer opportunistically fills type holes or suggests partial fillings when types conflict due to conflicting syntax.

**Total type error localization for groves** is proven, meaning developers have full access to semantic services (type hints, navigation, completion) even while resolving merge conflicts.

### 4.3 Livelits: Live Literals (PLDI 2021)

Livelits allow library providers to define custom GUIs that fill typed holes in the program. They bridge the gap between textual programming and direct manipulation.

#### 4.3.1 Architecture

A livelit follows a **model-view-update-expand** architecture:
- **Model**: Persistent state (serialized into the syntax tree).
- **View**: Renders the GUI.
- **Update**: Handles user interactions, producing new model states.
- **Expand**: A macro expansion that generates a typed expression from the model.

The expansion type is declared in the livelit definition (e.g., `livelit $color at RGBA {...}`).

#### 4.3.2 Compositionality via Splices

Livelit GUIs can embed **splices**—sub-expression editors of a specified type. Splices are lexically scoped to the livelit invocation site and passed into the expansion as function arguments. This ensures:
- **Capture avoidance**: Variables in splices refer to bindings at the invocation site, not internal expansion bindings.
- **Context independence**: Expansions are valid in any lexical context; dependencies are resolved at the livelit definition site.

Livelits can also take **parameters** (applied using function application syntax) and support partial application to define livelit abbreviations.

#### 4.3.3 Liveness via Closure Collection

Livelits can evaluate splices live. This works in two phases:
1. **Proto-closure collection**: Replace each livelit with a uniquely numbered hole, evaluate using Hazelnut Live semantics, collecting hole closures.
2. **Closure resumption**: Fill livelit holes with their expansions and resume evaluation.

This provides live feedback even when livelits appear inside functions (selecting between multiple closures from different call sites).

### 4.4 Incremental Bidirectional Typing (2025)

The *Marked and Annotated Lambda Calculus (MALC)* and its incremental variant specify how type information is maintained efficiently.

#### 4.4.1 MALC: Annotations on Every Node

MALC extends the marked lambda calculus by storing:
- **Error marks** as boolean flags on each term (not as separate constructors).
- **Analyzed type** (optional): the type flowing in from the parent.
- **Synthesized type** (optional): the type flowing out from the expression.

Terms are stratified into three mutually recursive sorts: marked synthetic expressions (carrying synthesized type), marked constructor expressions (core syntax), and marked analytic expressions (carrying analyzed type and a consistency mark).

#### 4.4.2 Incremental Update Propagation

Rather than re-marking the entire program after each edit, Incremental MALC defines a **small-step update propagation dynamics**:

1. An edit action creates **dirty bits** on types at the edit location and at binding sites connected by the program's binding structure.
2. Each propagation step considers a dirty type at the **frontier**, computes its local ramifications, and advances the frontier.
3. Edits and updates can be **interleaved confluently**—editing is blocked only for the duration of individual propagation steps.
4. Correctness is guaranteed once propagation quiesces (the frontier is empty).

#### 4.4.3 Order Maintenance Data Structures

Efficient implementation uses **order maintenance data structures** (inspired by web browser layout engines) to:
- Maintain pointers connecting bound variables to their binding locations (for direct type update propagation).
- Prioritize the order of update propagation.

The implementation, called *Malcom*, achieves a **275.96× speedup** over from-scratch reanalysis on a large stress-test.

---

## 5. Data Structure Summary

| Component | Core Data Structure | Purpose |
|---|---|---|
| **Syntax** | Tiles (tile-based parsing) with syntactic obligations | Always-well-formed program sketches |
| **Collaborative state** | Directed labeled multi-graph (vertices + edges with UIDs, 2P-Set CRDT) | Commutative collaborative editing |
| **Decomposed view** | Grove (set of trees with holes, local conflicts, relocation/unicycle conflict references) | Tree-based editing/typing over graph state |
| **Static semantics** | Marked & Annotated expressions (3-sort stratified AST with boolean marks, optional analyzed/synthesized types) | Total type error localization & incremental update |
| **Type inference** | PotentialTypeSets (recursive sets of potential types with `etc` constraints for expression holes) | Neutral constraint-based type hole inference |
| **Hole closures** | Internal expressions with substitution environments `σ` (cf. CMTT metavariable closures) | Live evaluation, fill-and-resume, live context inspector |
| **Livelits** | Model-view-update-expand architecture; splices as typed, scoped sub-expression holes | Extensible domain-specific GUIs in code |
| **Incremental typing** | Dirty-bit frontier on MALC terms + order maintenance data structures + binding pointers | Sub-linear type information maintenance |

---

## 6. Open Problems and Tricky Issues

### 6.1 Side Effects and Fill-and-Resume

The commutativity theorem for fill-and-resume holds only for pure functional languages. Extending to languages with references, IO, or other non-commutative effects requires either checkpointing, type-and-effect systems for granular re-evaluation, or explicit user confirmation before continuing evaluation past holes that might trigger effects. The paper identifies this as future work and notes that algebraic effects / monadic approaches might help.

### 6.2 Scaling Live Evaluation

Hazel's vision requires evaluating a "planetary-scale" program live as thousands of collaborators edit simultaneously. This demands:
- **Incremental evaluation**: Only re-computing what's affected by edits (complementary to, but distinct from, incremental typing).
- **Distributed scheduling**: The Fairground planetary compute engine must parallelize and distribute pure functional computations across heterogeneous hardware.
- **Large/streaming datasets**: Live data ingestion where dataset updates function as edits to literal tables.

No formal calculus for incremental/distributed evaluation in this setting has been published yet.

### 6.3 Syntax Error Recovery Completeness

The tall tylr system handles many syntactic errors via obligations, but total syntax error recovery (handling unmatched delimiters, etc.) is described as still underway. Achieving it would complete the syntactic layer of the continuity invariant for the textual editing mode.

### 6.4 Incremental Marking + Type Inference Integration

The incremental typing paper (MALC/Malcom) explicitly scopes out the unification-based type hole inference layer. Incrementalizing constraint gathering and unification on top of incremental bidirectional marking is left as future work and is necessary for the full Hazel experience to scale.

### 6.5 Constraint Solving Under Collaboration

Grove extends marking to groves with conflicts, and layers unification on top. But the interaction between collaborative conflict resolution, incremental propagation, and constraint-based inference has not been fully worked out. As conflicts are resolved, type information must be updated; the incremental approach would need to handle the peculiar syntactic forms (local conflicts, relocation references) that arise in groves.

### 6.6 Polymorphic Generalization with Holes

The type hole inference system must be careful about polymorphic generalization. A type variable constrained only by expression holes (which might generate any constraint once filled) should not be generalized. The `etc` constraint in PotentialTypeSets addresses this, but scaling this to full System F-style polymorphism with higher-rank types and implicit instantiation remains future work.

### 6.7 Livelit Safety and Sandboxing

Livelits execute arbitrary GUI code (currently in the browser). In a collaborative/planetary setting, livelits from untrusted sources must be sandboxed. The formal calculus treats expansions as hygienic macros, but the practical sandboxing of livelit view/update code (which runs with access to the DOM) is an engineering challenge not addressed by the theory.

### 6.8 Interoperability with Existing Languages

Fairground proposes Fair Python (a pure functional Python subset) with FFI to arbitrary languages via Nix-sandboxed foreign nodes. Designing Fair Python to be recognizable to Python programmers while being genuinely pure and compositional is a significant language design challenge. The FFI must also handle the impedance mismatch between Python's mutable objects and Fair Python's immutable values.

### 6.9 Pattern Matching and Exhaustiveness with Holes

Recent work (Yuan et al. 2023, referenced but not included in the papers above) addresses pattern matching with typed holes, including reasoning about redundancy and exhaustiveness in the presence of pattern holes. Integrating this with the marked lambda calculus to produce error marks for inexhaustive matches and redundant patterns is identified as a direction for extending the system.

### 6.10 Dynamic Semantics for Groves

The Grove paper develops static semantics (marking + typing) for groves but does not define a dynamic semantics. Evaluating a program with unresolved conflicts (where, say, a local conflict offers two alternative expressions) is not yet formalized. Extending Hazelnut Live to groves would complete the continuity invariant for collaborative editing.

---

## 7. How the Pieces Fit Together

The Hazel system is organized as a pipeline, with each layer guaranteeing a property that the next layer depends on:

```
User Edits (keystrokes, structure edits, livelit interactions)
    │
    ▼
[tall tylr / Structure Editor]  ──→  Syntactically well-formed program sketch
    │                                 (tiles → terms with obligations/holes)
    ▼
[Grove CmRDT]  ──→  Convergent collaborative graph state
    │                 (if collaborative; decomposed into grove)
    ▼
[Marking (MALC)]  ──→  Marked & annotated program
    │                    (every expression has a type; errors localized)
    ▼
[Type Hole Inference]  ──→  Type holes filled where possible;
    │                        conflicts localized to holes with suggestions
    ▼
[Incremental MALC]  ──→  Efficient maintenance of type information
    │                      (dirty-bit propagation, order maintenance)
    ▼
[Elaboration]  ──→  Internal expression with hole closures + casts
    │
    ▼
[Hazelnut Live Evaluation]  ──→  Result (value, or indeterminate with hole closures)
    │
    ▼
[Live Context Inspector / Livelit Closure Collection]  ──→  Rich feedback to user
```

The **continuity invariant** is the composition of guarantees across this entire pipeline: sensibility (editor → well-formed sketch), totality of marking (sketch → marked expression with types), typed elaboration (marked expression → well-typed internal expression), and progress + preservation (internal expression → meaningful result).

---

## 8. Conclusion

Hazel represents perhaps the most ambitious attempt to put programming environment design on rigorous type-theoretic foundations. Every layer of the system—from syntax recovery to collaborative editing to live evaluation—is specified as a formal calculus with mechanized metatheory, and the layers compose to produce a strong end-to-end guarantee (the continuity invariant) that no editor state is ever meaningless.

The key architectural insight is that **holes are first-class citizens** at every level: empty holes in syntax, type holes in the static semantics, hole closures in the dynamic semantics, spliced holes for livelit composition, and holes in groves for conflict representation. Gradual typing provides the uniform recovery mechanism that makes totality possible across all these layers.

The principal open challenges lie in scaling these ideas: incremental evaluation (not just incremental typing), distributed execution, full syntax error recovery, integration of constraint inference with incremental and collaborative systems, and the design of Fair Python for real-world scientific workflows. These are hard problems, but the formal foundations provide a uniquely solid base from which to attack them.
