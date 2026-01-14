import { WasmState } from "./pkg/rust";
import { type Action, type Constructor, type Direction, type TermConstructor, type GroveConstructor } from "./RustTypes";

// Types matching the Rust structures
export type TermEdge = any;  // Opaque type from Rust
export type TermNode = any;  // Opaque type from Rust
export type TermLocation = { node: TermNode; position: number };
export type Term = { Node: TermNode } | { Reference: TermEdge };
export type Location = { node: any; position: number };
export type Edge = any;  // Opaque type from Rust
export type PatchNode = any;  // Opaque type from Rust
export type PatchLocation = any;  // Opaque type from Rust
export type Patch = any;  // Opaque type from Rust

// Cursor can be either on an edge or a location
export type Cursor =
  | { kind: 'Edge'; edge: TermEdge }
  | { kind: 'Location'; location: TermLocation };

// Clipboard can be empty or contain a cursor
export type Clipboard =
  | { kind: 'Empty' }
  | { kind: 'Cursor'; cursor: Cursor };

/**
 * Controller class that manages cursor, clipboard, and action handling.
 * This is the TypeScript equivalent of the Rust controller.rs module.
 */
export class Controller {
  private blossom: WasmState;
  private cursor: Cursor;
  private clipboard: Clipboard;

  constructor() {
    this.blossom = new WasmState();
    const rootLocation = this.blossom.root_location();
    this.cursor = { kind: 'Location', location: rootLocation };
    this.clipboard = { kind: 'Empty' };
  }

  // =====================================================
  // View methods (delegate to WasmState)
  // =====================================================

  rootTermLocation(): TermLocation {
    return this.blossom.root_location();
  }

  constructorOfTerm(t: Term): TermConstructor {
    return this.blossom.constructor_of_term(t);
  }

  constructorOfType(t: any): TermConstructor {
    return this.blossom.constructor_of_type(t);
  }

  childrenOfType(t: any): any[] {
    return this.blossom.children_of_type(t);
  }

  childrenOfTypeLocation(tl: any): any[] {
    return this.blossom.children_of_type_location(tl);
  }

  childrenOfTerm(t: Term): TermLocation[] {
    return this.blossom.children_of_term(t);
  }

  childrenOfTermLocation(tl: TermLocation): Term[] {
    return this.blossom.children_of_location(tl);
  }

  typesOfSite(site: any): any {
    if ('Term' in site || 'Node' in site || 'Reference' in site) {
      return {
        ana: this.blossom.ana_of_term(site),
        syn: this.blossom.syn_of_term(site),
        marks: this.blossom.marks_of_term(site),
        sort: this.blossom.sort_of_term(site),
      };
    } else {
      return {
        ana: this.blossom.ana_of_location(site),
        syn: this.blossom.syn_of_location(site),
      };
    }
  }

  isDirty(site: any): boolean {
    if ('Node' in site || 'Reference' in site) {
      return this.blossom.is_dirty_term(site);
    } else {
      return this.blossom.is_dirty_location(site);
    }
  }

  // =====================================================
  // Cursor methods
  // =====================================================

  private nodeDestinationOfTermEdge(te: TermEdge): TermNode | null {
    return this.blossom.node_destination_of_term_edge(te);
  }

  private innerCursorAtTerm(c: Cursor, t: Term): boolean {
    if (c.kind === 'Location') return false;

    if ('Node' in t) {
      const dest = this.nodeDestinationOfTermEdge(c.edge);
      return dest != null && this.termNodesEqual(dest, t.Node);
    } else if ('Reference' in t) {
      return this.termEdgesEqual(c.edge, t.Reference);
    }
    return false;
  }

  cursorAtTerm(t: Term): boolean {
    return this.innerCursorAtTerm(this.cursor, t);
  }

  cursorAlmostAtTerm(t: Term): boolean {
    if (this.cursor.kind !== 'Edge') return false;
    const cursorN = this.blossom.destination_of_edge(this.cursor.edge.edge);
    const tN = this.nodeOfTerm(t);
    return this.nodesEqual(cursorN, tN);
  }

  private innerCursorAtLocation(c: Cursor, tl: TermLocation): boolean {
    if (c.kind !== 'Location') return false;
    return this.termLocationsEqual(c.location, tl);
  }

  cursorAtLocation(tl: TermLocation): boolean {
    return this.innerCursorAtLocation(this.cursor, tl);
  }

  cursorAlmostAtLocation(tl: TermLocation): boolean {
    if (this.cursor.kind !== 'Location') return false;
    return tl.position === this.cursor.location.position &&
           this.termNodesEqual(tl.node, this.cursor.location.node);
  }

  clipboardAtTerm(t: Term): boolean {
    if (this.clipboard.kind === 'Empty') return false;
    return this.innerCursorAtTerm(this.clipboard.cursor, t);
  }

  clipboardAtLocation(tl: TermLocation): boolean {
    if (this.clipboard.kind === 'Empty') return false;
    return this.innerCursorAtLocation(this.clipboard.cursor, tl);
  }

  // =====================================================
  // Helper methods for equality checks
  // =====================================================

  private nodesEqual(a: any, b: any): boolean {
    return JSON.stringify(a) === JSON.stringify(b);
  }

  private termNodesEqual(a: TermNode, b: TermNode): boolean {
    return JSON.stringify(a) === JSON.stringify(b);
  }

  private termEdgesEqual(a: TermEdge, b: TermEdge): boolean {
    return JSON.stringify(a) === JSON.stringify(b);
  }

  private termLocationsEqual(a: TermLocation, b: TermLocation): boolean {
    return JSON.stringify(a) === JSON.stringify(b);
  }

  private nodeOfTerm(t: Term): any {
    if ('Node' in t) {
      return t.Node.node;
    } else {
      return this.blossom.destination_of_edge(t.Reference.edge);
    }
  }

  // =====================================================
  // Patch creation helpers
  // =====================================================

  private connectionPatchExisting(l: Location, n: any): Patch {
    const source = this.blossom.patch_location_of_location(l);
    const destination = this.blossom.patch_node_of_node(n);
    return this.blossom.connection_patch(source, destination);
  }

  private deleteEdges(edges: Edge[]): Patch[] {
    return edges.map(e => this.blossom.deletion_patch(e));
  }

  private deleteLocation(l: Location): Patch[] {
    const edges: Edge[] = this.blossom.edge_children_of_location(l);
    return this.deleteEdges(edges);
  }

  private insertionPatch(tl: TermLocation, c: Constructor): Patch {
    const l = this.termLocationToLocation(tl);
    const source = this.blossom.patch_location_of_location(l);
    const destination = this.blossom.new_patch_node(c);
    return this.blossom.connection_patch(source, destination);
  }

  private termLocationToLocation(tl: TermLocation): Location {
    return { node: tl.node.node, position: tl.position };
  }

  private sourceOfTermEdge(te: TermEdge): TermLocation {
    return this.blossom.source_of_term_edge(te);
  }

  private edgeChildrenOfTermLocation(tl: TermLocation): TermEdge[] {
    return this.blossom.edge_children_of_term_location(tl);
  }

  private numChildrenOfLocation(l: Location): number {
    return this.blossom.num_children_of_location(l);
  }

  private numChildrenOfTermLocation(tl: TermLocation): number {
    return this.blossom.num_children_of_term_location(tl);
  }

  private numChildrenOfTermNode(tn: TermNode): number {
    return this.blossom.num_children_of_term_node(tn);
  }

  private uniqueParentOfTermNode(tn: TermNode): TermEdge | null {
    return this.blossom.unique_parent_of_term_node(tn);
  }

  private rightSiblingOfTermEdge(te: TermEdge): TermEdge {
    return this.blossom.right_sibling_of_term_edge(te);
  }

  private rightSiblingOfTermLocation(tl: TermLocation): TermLocation {
    return this.blossom.right_sibling_of_term_location(tl);
  }

  private uniqueParentEdgeOfTerm(t: Term): TermEdge | null {
    return this.blossom.unique_parent_edge_of_term(t);
  }

  // =====================================================
  // Action computation methods
  // =====================================================

  private computeWrapLeft(c: Constructor): Patch[] {
    return this.computeWrapAtPosition(c, 0);
  }

  private computeWrapRight(c: Constructor): Patch[] {
    return this.computeWrapAtPosition(c, 1);
  }

  private computeWrapAtPosition(c: Constructor, position: number): Patch[] {
    const arity = this.constructorArity(c);
    if (arity === 0) return [];
    if (position >= arity) return [];

    if (this.cursor.kind === 'Edge') {
      const te = this.cursor.edge;
      const e = te.edge;
      const parentSource = this.blossom.patch_location_of_location(this.blossom.source_of_edge(e));
      const middleDestination = this.blossom.new_patch_node(c);
      const middleSource = this.blossom.new_patch_location(middleDestination, position);
      const lowerDestination = this.blossom.patch_node_of_node(this.blossom.destination_of_edge(e));

      this.cursor = { kind: 'Location', location: this.sourceOfTermEdge(te) };

      return [
        this.blossom.deletion_patch(e),
        this.blossom.connection_patch(parentSource, middleDestination),
        this.blossom.connection_patch(middleSource, lowerDestination),
      ];
    } else {
      const tl = this.cursor.location;
      const l = this.termLocationToLocation(tl);
      const newPn = this.blossom.new_patch_node(c);
      const newSource = this.blossom.new_patch_location(newPn, position);
      const parentSource = this.blossom.patch_location_of_location(l);

      const patches: Patch[] = [this.blossom.connection_patch(parentSource, newPn)];

      const edges: Edge[] = this.blossom.edge_children_of_location(l);
      for (const e of edges) {
        patches.push(this.blossom.deletion_patch(e));
        const childNode = this.blossom.destination_of_edge(e);
        const childDestination = this.blossom.patch_node_of_node(childNode);
        patches.push(this.blossom.connection_patch(newSource, childDestination));
      }

      return patches;
    }
  }

  private computeInsert(c: Constructor): Patch[] {
    if (this.cursor.kind !== 'Location') return [];

    const tl = this.cursor.location;
    const l = this.termLocationToLocation(tl);
    const numChildren = this.numChildrenOfLocation(l);
    if (numChildren > 0) return [];

    return [this.insertionPatch(tl, c)];
  }

  private computeDelete(): Patch[] {
    if (this.cursor.kind === 'Edge') {
      const te = this.cursor.edge;
      this.cursor = { kind: 'Location', location: this.sourceOfTermEdge(te) };
      return [this.blossom.deletion_patch(te.edge)];
    } else {
      const l = this.termLocationToLocation(this.cursor.location);
      return this.deleteLocation(l);
    }
  }

  private computePasteHelper(source: PatchLocation, e: Edge): Patch {
    const dest = this.blossom.patch_node_of_node(this.blossom.destination_of_edge(e));
    return this.blossom.connection_patch(source, dest);
  }

  private computePaste(): Patch[] {
    if (this.cursor.kind !== 'Location') return [];
    if (this.clipboard.kind === 'Empty') return [];

    const tl = this.cursor.location;
    const l = this.termLocationToLocation(tl);

    if (this.clipboard.cursor.kind === 'Edge') {
      const te = this.clipboard.cursor.edge;
      const e = te.edge;
      this.clipboard = { kind: 'Empty' };

      const patches: Patch[] = [this.blossom.deletion_patch(e)];
      const n = this.blossom.destination_of_edge(e);
      patches.push(this.connectionPatchExisting(l, n));
      return patches;
    } else {
      const tclipboard = this.clipboard.cursor.location;
      this.clipboard = { kind: 'Empty' };

      const clipboardL = this.termLocationToLocation(tclipboard);
      const edges: Edge[] = this.blossom.edge_children_of_location(clipboardL);
      const source = this.blossom.patch_location_of_location(l);

      const patches = this.deleteLocation(clipboardL);
      for (const e of edges) {
        patches.push(this.computePasteHelper(source, e));
      }
      return patches;
    }
  }

  private normalizeCursor(): void {
    if (this.cursor.kind !== 'Location') return;

    const children = this.edgeChildrenOfTermLocation(this.cursor.location);
    if (children.length === 1) {
      this.cursor = { kind: 'Edge', edge: children[0] };
    }
  }

  // Check if a term node is a Proj node
  private isProjectorNode(tn: TermNode): boolean {
    const tc = this.constructorOfTerm({ Node: tn });
    if ('Constructor' in tc) {
      const gc = tc.Constructor;
      if (gc !== 'Root' && 'Lang' in gc) {
        return gc.Lang === 'Proj';
      }
    }
    return false;
  }

  // Check if a location is inside a Proj node (position 0 or 1)
  private isInsideProjector(tl: TermLocation): boolean {
    return this.isProjectorNode(tl.node);
  }

  // Check if a location is the content slot (position 1) of a Proj node
  private isAtProjectorContent(tl: TermLocation): boolean {
    return this.isProjectorNode(tl.node) && tl.position === 1;
  }

  // Check if a location is the internal slot (position 0) of a Proj node
  private isAtProjectorInternal(tl: TermLocation): boolean {
    return this.isProjectorNode(tl.node) && tl.position === 0;
  }

  private computeMove(c: Cursor, d: Direction): Cursor {
    switch (d) {
      case 'Up':
        if (c.kind === 'Edge') {
          const l = this.sourceOfTermEdge(c.edge);
          // If inside a Proj, skip the internal structure and go to Proj's parent
          if (this.isInsideProjector(l)) {
            const projParent = this.uniqueParentOfTermNode(l.node);
            if (projParent != null) {
              return { kind: 'Edge', edge: projParent };
            }
          }
          const numChildren = this.numChildrenOfLocation(this.termLocationToLocation(l));
          if (numChildren === 1) {
            // Skip to equivalent location selection before move up
            return this.computeMove({ kind: 'Location', location: l }, 'Up');
          }
          return { kind: 'Location', location: l };
        } else {
          // If at position 1 of Proj, skip to Proj's parent (not position 0)
          if (this.isAtProjectorContent(c.location)) {
            const projParent = this.uniqueParentOfTermNode(c.location.node);
            if (projParent != null) {
              return { kind: 'Edge', edge: projParent };
            }
          }
          const parent = this.uniqueParentOfTermNode(c.location.node);
          if (parent == null) return c;  // Use loose equality to catch both null and undefined
          return { kind: 'Edge', edge: parent };
        }

      case 'Down':
        if (c.kind === 'Edge') {
          const dest = this.nodeDestinationOfTermEdge(c.edge);
          if (dest == null) return c;  // Use loose equality to catch both null and undefined
          const numChildren = this.numChildrenOfTermNode(dest);
          if (numChildren === 0) return c;
          // If entering a Proj, skip position 0 and go directly to position 1 (content)
          if (this.isProjectorNode(dest)) {
            return { kind: 'Location', location: { node: dest, position: 1 } };
          }
          return { kind: 'Location', location: { node: dest, position: 0 } };
        } else {
          // Stop at projector content boundary (can't go into collapsed content)
          if (this.isAtProjectorContent(c.location)) {
            return c;
          }
          // If somehow at position 0 of Proj, move to position 1 instead
          if (this.isAtProjectorInternal(c.location)) {
            return { kind: 'Location', location: { node: c.location.node, position: 1 } };
          }
          const children = this.edgeChildrenOfTermLocation(c.location);
          if (children.length === 0) return c;
          if (children.length === 1) {
            // Skip to equivalent mode selection before move down
            return this.computeMove({ kind: 'Edge', edge: children[0] }, 'Down');
          }
          return { kind: 'Edge', edge: children[0] };
        }

      case 'Right':
        if (c.kind === 'Edge') {
          const l = this.sourceOfTermEdge(c.edge);
          // If inside a Proj, there's only one navigable position (content), so Right does nothing
          if (this.isInsideProjector(l)) {
            return c;
          }
          const numChildren = this.numChildrenOfTermLocation(l);
          if (numChildren === 1) {
            // Skip to equivalent location selection before move right
            return this.computeMove({ kind: 'Location', location: l }, 'Right');
          }
          return { kind: 'Edge', edge: this.rightSiblingOfTermEdge(c.edge) };
        } else {
          // If inside a Proj, there's only one navigable position (content), so Right does nothing
          if (this.isInsideProjector(c.location)) {
            return c;
          }
          return { kind: 'Location', location: this.rightSiblingOfTermLocation(c.location) };
        }
    }
  }

  private computeMoveToTerm(t: Term): void {
    const e = this.uniqueParentEdgeOfTerm(t);
    if (e != null) {  // Use loose equality to catch both null and undefined
      this.cursor = { kind: 'Edge', edge: e };
    }
  }

  private computeTextInsert(x: string): Patch[] {
    if (this.cursor.kind === 'Edge') {
      const te = this.cursor.edge;  // Save edge before computeDelete changes cursor
      const dest = this.nodeDestinationOfTermEdge(te);
      if (dest == null) return [];  // Use loose equality to catch both null and undefined

      const tc = this.constructorOfTerm({ Node: dest });
      if (!('Constructor' in tc)) return [];
      const gc = tc.Constructor;
      if (gc === 'Root' || !('Lang' in gc)) return [];
      const c = gc.Lang;
      if (typeof c !== 'object' || !('Identifier' in c)) return [];

      const id = c.Identifier;
      const tl = this.sourceOfTermEdge(te);  // Get source before delete
      const patches = this.computeDelete();
      patches.push(this.insertionPatch(tl, { Identifier: id + x }));
      return patches;
    } else {
      const cs = this.edgeChildrenOfTermLocation(this.cursor.location);
      if (cs.length > 1) return [];
      if (cs.length === 0) {
        return this.computeInsert({ Identifier: x });
      }
      this.cursor = { kind: 'Edge', edge: cs[0] };
      return this.computeTextInsert(x);
    }
  }

  private computeTextBackspace(): Patch[] {
    if (this.cursor.kind === 'Edge') {
      const te = this.cursor.edge;  // Save edge before computeDelete changes cursor
      const dest = this.nodeDestinationOfTermEdge(te);
      if (dest == null) return [];  // Use loose equality to catch both null and undefined

      const tc = this.constructorOfTerm({ Node: dest });
      if (!('Constructor' in tc)) return [];
      const gc = tc.Constructor;
      if (gc === 'Root' || !('Lang' in gc)) return [];
      const c = gc.Lang;
      if (typeof c !== 'object' || !('Identifier' in c)) return [];

      const id: string = c.Identifier;
      const tl = this.sourceOfTermEdge(te);  // Get source before delete
      const patches = this.computeDelete();
      if (id.length > 1) {
        const newId = id.slice(0, -1);
        patches.push(this.insertionPatch(tl, { Identifier: newId }));
      }
      return patches;
    } else {
      const cs = this.edgeChildrenOfTermLocation(this.cursor.location);
      if (cs.length !== 1) return [];
      this.cursor = { kind: 'Edge', edge: cs[0] };
      return this.computeTextBackspace();
    }
  }

  private constructorArity(c: Constructor): number {
    if (typeof c === 'string') {
      switch (c) {
        case 'Typ': return 0;
        case 'Num': return 0;
        case 'Zero': return 0;
        case 'Plus': return 2;
        case 'Prod': return 2;
        case 'Pair': return 2;
        case 'Arrow': return 2;
        case 'Fun': return 2;
        case 'Ap': return 2;
        case 'Asc': return 2;
        case 'Let': return 3;
        // Projector wrapper has 2 children: projector type and child term
        case 'Proj': return 2;
        // Projector types
        case 'Structural': return 0;
        case 'Collapsed': return 0;
        case 'Labeled': return 1;  // child 0 stores the label
      }
    } else if ('Identifier' in c) {
      return 0;
    }
    return 0;
  }

  // =====================================================
  // Public action interface
  // =====================================================

  private computeAction(a: Action): Patch[] {
    if (typeof a === 'string') {
      switch (a) {
        case 'Delete':
          return this.computeDelete();
        case 'Cut':
          this.clipboard = { kind: 'Cursor', cursor: this.cursor };
          return [];
        case 'Paste':
          return this.computePaste();
        case 'TextBackspace':
          return this.computeTextBackspace();
      }
    } else if ('BlossomAction' in a) {
      this.blossom.apply_blossom_action(a.BlossomAction);
      return [];
    } else if ('WrapLeft' in a) {
      return this.computeWrapLeft(a.WrapLeft);
    } else if ('WrapRight' in a) {
      return this.computeWrapRight(a.WrapRight);
    } else if ('Insert' in a) {
      return this.computeInsert(a.Insert);
    } else if ('Move' in a) {
      this.cursor = this.computeMove(this.cursor, a.Move);
      return [];
    } else if ('MoveToLocation' in a) {
      this.cursor = { kind: 'Location', location: a.MoveToLocation };
      return [];
    } else if ('MoveToTerm' in a) {
      this.computeMoveToTerm(a.MoveToTerm);
      return [];
    } else if ('TextInsert' in a) {
      return this.computeTextInsert(a.TextInsert as string);
    }
    return [];
  }

  applyPatch(p: Patch): void {
    this.blossom.apply_patch(p);
  }

  applyAction(a: Action): Patch[] {
    const patches = this.computeAction(a);
    for (const p of patches) {
      this.applyPatch(p);
    }
    this.normalizeCursor();
    return patches;
  }

  // =====================================================
  // Compatibility methods for existing App.tsx interface
  // =====================================================

  // These methods maintain backward compatibility with the current WasmState interface
  apply_serial_action(action: Action): Patch[] {
    return this.applyAction(action);
  }

  apply_patch(patch: Patch): void {
    this.applyPatch(patch);
  }

  move_to_location(tl: TermLocation): void {
    this.applyAction({ MoveToLocation: tl });
  }

  move_to_term(t: Term): void {
    this.applyAction({ MoveToTerm: t });
  }

  root_location(): TermLocation {
    return this.rootTermLocation();
  }

  constructor_of_term(t: Term): TermConstructor {
    return this.constructorOfTerm(t);
  }

  ana_of_term(t: Term): any {
    return this.blossom.ana_of_term(t);
  }

  syn_of_term(t: Term): any {
    return this.blossom.syn_of_term(t);
  }

  ana_of_location(tl: TermLocation): any {
    return this.blossom.ana_of_location(tl);
  }

  syn_of_location(tl: TermLocation): any {
    return this.blossom.syn_of_location(tl);
  }

  marks_of_term(t: Term): any {
    return this.blossom.marks_of_term(t);
  }

  sort_of_term(t: Term): any {
    return this.blossom.sort_of_term(t);
  }

  constructor_of_type(t: any): TermConstructor {
    return this.blossom.constructor_of_type(t);
  }

  children_of_type(t: any): any[] {
    return this.blossom.children_of_type(t);
  }

  children_of_type_location(tl: any): any[] {
    return this.blossom.children_of_type_location(tl);
  }

  children_of_term(t: Term): TermLocation[] {
    return this.blossom.children_of_term(t);
  }

  children_of_location(tl: TermLocation): Term[] {
    return this.blossom.children_of_location(tl);
  }

  cursor_at_term(t: Term): boolean {
    return this.cursorAtTerm(t);
  }

  cursor_almost_at_term(t: Term): boolean {
    return this.cursorAlmostAtTerm(t);
  }

  cursor_at_location(tl: TermLocation): boolean {
    return this.cursorAtLocation(tl);
  }

  cursor_almost_at_location(tl: TermLocation): boolean {
    return this.cursorAlmostAtLocation(tl);
  }

  clipboard_at_term(t: Term): boolean {
    return this.clipboardAtTerm(t);
  }

  clipboard_at_location(tl: TermLocation): boolean {
    return this.clipboardAtLocation(tl);
  }

  // Get the term at cursor if cursor is at an Edge, or null otherwise
  get_term_at_cursor(): Term | null {
    if (this.cursor.kind !== 'Edge') return null;
    const dest = this.nodeDestinationOfTermEdge(this.cursor.edge);
    if (dest == null) return null;
    return { Node: dest };
  }


  is_dirty_term(t: Term): boolean {
    return this.blossom.is_dirty_term(t);
  }

  is_dirty_location(tl: TermLocation): boolean {
    return this.blossom.is_dirty_location(tl);
  }
}
