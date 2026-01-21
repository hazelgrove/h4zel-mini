import { WasmState } from "./pkg/rust";
import { type Action, type Constructor, type Direction, type TermConstructor } from "./RustTypes";

// Types matching the Rust structures (from grove.rs and forest.rs)
// These mirror the serde serialization format from Rust.

// grove.rs types
export type NodeId = "Root" | { Uuid: string };
export type Node = { id: NodeId };
export type Edge = { id: string };  // UUID serialized as string
export type Location = { node: Node; position: number };

// forest.rs types
// PathHash is [u8; 16] - serialized as array of numbers
export type TermNode = { path: number[]; node: Node };
export type TermEdge = { path: number[]; edge: Edge };
export type TermLocation = { node: TermNode; position: number };
export type Term = { Node: TermNode } | { Reference: TermEdge };

// Patch types remain opaque - they're created by Rust and passed back
export type PatchNode = unknown;
export type PatchLocation = unknown;
export type Patch = unknown;

// Clipboard stores a location (for cut/paste)
export type Clipboard =
  | { kind: 'Empty' }
  | { kind: 'Location'; location: TermLocation };

// Generate a UUID for cursor identity
function generateUUID(): string {
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
    const r = Math.random() * 16 | 0;
    const v = c === 'x' ? r : (r & 0x3 | 0x8);
    return v.toString(16);
  });
}

// Get or create a cursor identity for this tab
// Uses sessionStorage so each tab gets its own identity, but refreshing the same tab keeps it
function getOrCreateCursorIdentity(): string {
  const STORAGE_KEY = 'hazel_cursor_identity';
  let identity = sessionStorage.getItem(STORAGE_KEY);
  if (!identity) {
    identity = generateUUID();
    sessionStorage.setItem(STORAGE_KEY, identity);
  }
  return identity;
}

/**
 * Controller class that manages cursor, clipboard, and action handling.
 *
 * CURSOR ARCHITECTURE:
 * ====================
 * There is NO local cursor state. The Grove cursor IS the cursor.
 *
 * The Grove cursor is a Cursor(identity, content) node:
 *   - Position 0: Identity node (Identifier with UUID) - STABLE, never moves
 *   - Position 1: Content (the selected term, or empty for hole selection)
 *
 * The ONLY local state is `myIdentityNode` - a reference to our identity node.
 * Everything else is computed from this:
 *   - Cursor node = identity node's parent
 *   - Cursor content location = { node: cursorNode, position: 1 }
 *   - Selected term = child of cursor content (if any)
 *
 * MOVEMENT:
 * Movement is achieved by generating patches that:
 *   1. Unwrap: Move cursor's content to cursor's parent location
 *   2. Move: Delete cursor's parent edge, connect cursor to new location
 *   3. Wrap: Move target content into cursor's content
 *
 * IMPORTANT: Never store cursor position locally. Always derive from Grove state.
 */
export class Controller {
  private blossom: WasmState;
  private clipboard: Clipboard;
  private cursorIdentity: string;
  private myIdentityNode: TermNode | null = null;  // The ONLY cursor state - identity node reference

  constructor() {
    this.blossom = new WasmState();
    this.cursorIdentity = getOrCreateCursorIdentity();
    this.clipboard = { kind: 'Empty' };
  }

  // Get this client's cursor identity
  getCursorIdentity(): string {
    return this.cursorIdentity;
  }

  // =====================================================
  // Core cursor accessors - all derived from myIdentityNode
  // =====================================================

  // Get our cursor node (identity node's parent)
  private getMyCursorNode(): TermNode | null {
    if (!this.myIdentityNode) return null;
    const parentEdge = this.uniqueParentOfTermNode(this.myIdentityNode);
    if (!parentEdge) return null;
    const parentLoc = this.sourceOfTermEdge(parentEdge);
    return parentLoc.node;
  }

  // Get cursor content location (position 1 of cursor node)
  getMyCursorContentLocation(): TermLocation | null {
    const cursorNode = this.getMyCursorNode();
    if (!cursorNode) return null;
    return { node: cursorNode, position: 1 };
  }

  // Get the term inside cursor content (null if empty)
  private getMyCursorContent(): Term | null {
    const contentLoc = this.getMyCursorContentLocation();
    if (!contentLoc) return null;
    const terms = this.childrenOfTermLocation(contentLoc);
    if (terms.length === 1) return terms[0];
    return null;
  }

  // Get the location where cursor node is attached
  private getMyCursorParentLocation(): TermLocation | null {
    const cursorNode = this.getMyCursorNode();
    if (!cursorNode) return null;
    const parentEdge = this.uniqueParentOfTermNode(cursorNode);
    if (!parentEdge) return null;
    return this.sourceOfTermEdge(parentEdge);
  }

  // Initialize identity node reference (call ONCE at startup)
  initializeIdentityNode(cursorInfo: { cursorTerm: Term; identity: string; contentLocation: TermLocation }): void {
    if ('Node' in cursorInfo.cursorTerm) {
      const cursorNode = cursorInfo.cursorTerm.Node;
      const identityLoc: TermLocation = { node: cursorNode, position: 0 };
      const identityTerms = this.childrenOfTermLocation(identityLoc);
      if (identityTerms.length === 1 && 'Node' in identityTerms[0]) {
        this.myIdentityNode = identityTerms[0].Node;
      }
    }
  }

  // =====================================================
  // Cursor state queries - all check Grove directly
  // =====================================================

  // Is term t the cursor's content?
  cursorAtTerm(t: Term): boolean {
    const content = this.getMyCursorContent();
    if (!content) return false;
    if ('Node' in t && 'Node' in content) {
      return this.termNodesEqual(t.Node, content.Node);
    }
    if ('Reference' in t && 'Reference' in content) {
      return this.termEdgesEqual(t.Reference, content.Reference);
    }
    return false;
  }

  // Is cursor at location tl with empty content?
  cursorAtLocation(tl: TermLocation): boolean {
    const cursorParentLoc = this.getMyCursorParentLocation();
    if (!cursorParentLoc) return false;
    // Cursor is "at" a location if:
    // 1. The cursor node is at that location (cursorParentLoc equals tl)
    // 2. AND cursor content is empty
    if (!this.termLocationsEqual(cursorParentLoc, tl)) return false;
    const content = this.getMyCursorContent();
    return content === null;
  }

  // "Almost at" checks for rendering (same node, different path hash)
  cursorAlmostAtTerm(t: Term): boolean {
    const content = this.getMyCursorContent();
    if (!content) return false;
    const contentNode = this.nodeOfTerm(content);
    const tNode = this.nodeOfTerm(t);
    return this.nodesEqual(contentNode, tNode);
  }

  cursorAlmostAtLocation(tl: TermLocation): boolean {
    const cursorParentLoc = this.getMyCursorParentLocation();
    if (!cursorParentLoc) return false;
    return tl.position === cursorParentLoc.position &&
           this.nodesEqual(cursorParentLoc.node.node, tl.node.node);
  }

  // Clipboard checks
  clipboardAtTerm(t: Term): boolean {
    if (this.clipboard.kind === 'Empty') return false;
    // Check if t is at the clipboard location
    const clipLoc = this.clipboard.location;
    const terms = this.childrenOfTermLocation(clipLoc);
    if (terms.length !== 1) return false;
    const clipTerm = terms[0];
    if ('Node' in t && 'Node' in clipTerm) {
      return this.termNodesEqual(t.Node, clipTerm.Node);
    }
    if ('Reference' in t && 'Reference' in clipTerm) {
      return this.termEdgesEqual(t.Reference, clipTerm.Reference);
    }
    return false;
  }

  clipboardAtLocation(tl: TermLocation): boolean {
    if (this.clipboard.kind === 'Empty') return false;
    return this.termLocationsEqual(this.clipboard.location, tl);
  }

  // =====================================================
  // Cursor node type checks
  // =====================================================

  private isCursorNode(t: Term): boolean {
    const tc = this.constructorOfTerm(t);
    if ('Constructor' in tc) {
      const gc = tc.Constructor;
      if (gc !== 'Root' && 'Lang' in gc) {
        return gc.Lang === 'Cursor';
      }
    }
    return false;
  }

  private isCursorTermNode(tn: TermNode): boolean {
    return this.isCursorNode({ Node: tn });
  }

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

  // =====================================================
  // Helper methods for equality checks
  // =====================================================

  private nodesEqual(a: Node, b: Node): boolean {
    return JSON.stringify(a.id) === JSON.stringify(b.id);
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

  // Compare locations by logical identity (same node ID and position)
  // ignoring path hash differences
  private sameLogicalLocation(a: TermLocation, b: TermLocation): boolean {
    return this.nodesEqual(a.node.node, b.node.node) && a.position === b.position;
  }

  private nodeOfTerm(t: Term): Node {
    if ('Node' in t) {
      return t.Node.node;
    } else {
      return this.blossom.destination_of_edge(t.Reference.edge);
    }
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
  // Low-level Grove accessors
  // =====================================================

  private nodeDestinationOfTermEdge(te: TermEdge): TermNode | null {
    return this.blossom.node_destination_of_term_edge(te);
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

  private uniqueParentEdgeOfTerm(t: Term): TermEdge | null {
    return this.blossom.unique_parent_edge_of_term(t);
  }

  private rightSiblingOfTermEdge(te: TermEdge): TermEdge {
    return this.blossom.right_sibling_of_term_edge(te);
  }

  private rightSiblingOfTermLocation(tl: TermLocation): TermLocation {
    return this.blossom.right_sibling_of_term_location(tl);
  }

  private termLocationToLocation(tl: TermLocation): Location {
    return { node: tl.node.node, position: tl.position };
  }

  private constructorArity(c: Constructor): number {
    return this.blossom.arity_of_constructor(c);
  }

  // =====================================================
  // Patch creation helpers
  // =====================================================

  private connectionPatchExisting(l: Location, n: Node): Patch {
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

  // =====================================================
  // Cursor movement patches
  // =====================================================

  // Create patches for initial cursor (only called once on startup)
  createInitialCursorPatches(targetLocation: TermLocation): Patch[] {
    const patches: Patch[] = [];
    const l = this.termLocationToLocation(targetLocation);

    const cursorNode = this.blossom.new_patch_node('Cursor');
    const source = this.blossom.patch_location_of_location(l);
    patches.push(this.blossom.connection_patch(source, cursorNode));

    const identityLocation = this.blossom.new_patch_location(cursorNode, 0);
    const identityNode = this.blossom.new_patch_node({ Identifier: this.cursorIdentity });
    patches.push(this.blossom.connection_patch(identityLocation, identityNode));

    return patches;
  }

  getInitialCursorPatches(): Patch[] {
    const rootLocation = this.rootTermLocation();
    return this.createInitialCursorPatches(rootLocation);
  }

  // Unwrap: move cursor's content to cursor's parent location
  private createUnwrapPatches(): Patch[] {
    const cursorNode = this.getMyCursorNode();
    if (!cursorNode) return [];

    const patches: Patch[] = [];

    const cursorParentEdge = this.uniqueParentOfTermNode(cursorNode);
    if (!cursorParentEdge) return [];
    const cursorParentLoc = this.sourceOfTermEdge(cursorParentEdge);
    const parentLocGrove = this.termLocationToLocation(cursorParentLoc);

    const contentLoc: TermLocation = { node: cursorNode, position: 1 };
    const contentEdges = this.edgeChildrenOfTermLocation(contentLoc);

    for (const contentEdge of contentEdges) {
      const contentNode = this.nodeDestinationOfTermEdge(contentEdge);
      if (contentNode) {
        patches.push(this.blossom.deletion_patch(contentEdge.edge));
        patches.push(this.blossom.connection_patch(
          this.blossom.patch_location_of_location(parentLocGrove),
          this.blossom.patch_node_of_node(contentNode.node)
        ));
      }
    }

    return patches;
  }

  // Move cursor node to new location
  private createMoveCursorNodePatches(newParentLoc: TermLocation): Patch[] {
    const cursorNode = this.getMyCursorNode();
    if (!cursorNode) return [];

    const patches: Patch[] = [];

    const cursorParentEdge = this.uniqueParentOfTermNode(cursorNode);
    if (cursorParentEdge) {
      patches.push(this.blossom.deletion_patch(cursorParentEdge.edge));
    }

    const newParentLocGrove = this.termLocationToLocation(newParentLoc);
    patches.push(this.blossom.connection_patch(
      this.blossom.patch_location_of_location(newParentLocGrove),
      this.blossom.patch_node_of_node(cursorNode.node)
    ));

    return patches;
  }

  // Wrap: move a term into cursor's content
  private createWrapPatches(targetNode: TermNode): Patch[] {
    const cursorNode = this.getMyCursorNode();
    if (!cursorNode) return [];

    const patches: Patch[] = [];

    const targetParentEdge = this.uniqueParentOfTermNode(targetNode);
    if (targetParentEdge) {
      patches.push(this.blossom.deletion_patch(targetParentEdge.edge));
    }

    const contentLoc: TermLocation = { node: cursorNode, position: 1 };
    const contentLocGrove = this.termLocationToLocation(contentLoc);
    patches.push(this.blossom.connection_patch(
      this.blossom.patch_location_of_location(contentLocGrove),
      this.blossom.patch_node_of_node(targetNode.node)
    ));

    return patches;
  }

  // Move cursor to wrap a term
  private createMoveCursorToTermPatches(targetTermNode: TermNode): Patch[] {
    const myCursorNode = this.getMyCursorNode();
    if (myCursorNode && this.termNodesEqual(targetTermNode, myCursorNode)) {
      return [];  // Don't wrap our own cursor
    }

    const patches: Patch[] = [];

    patches.push(...this.createUnwrapPatches());

    const targetParentEdge = this.uniqueParentOfTermNode(targetTermNode);
    if (!targetParentEdge) return patches;
    const targetParentLoc = this.sourceOfTermEdge(targetParentEdge);

    patches.push(...this.createMoveCursorNodePatches(targetParentLoc));
    patches.push(...this.createWrapPatches(targetTermNode));

    return patches;
  }

  // Move cursor to an empty location
  private createMoveCursorToLocationPatches(targetLocation: TermLocation): Patch[] {
    const myCursorNode = this.getMyCursorNode();
    if (myCursorNode && this.termNodesEqual(targetLocation.node, myCursorNode)) {
      return [];  // Don't move to our own cursor's internal locations
    }

    const patches: Patch[] = [];

    patches.push(...this.createUnwrapPatches());
    patches.push(...this.createMoveCursorNodePatches(targetLocation));

    // If target location has content, wrap it
    const targetChildren = this.edgeChildrenOfTermLocation(targetLocation);
    if (targetChildren.length > 0) {
      const targetNode = this.nodeDestinationOfTermEdge(targetChildren[0]);
      if (targetNode) {
        patches.push(...this.createWrapPatches(targetNode));
      }
    }

    return patches;
  }

  // =====================================================
  // Movement computation - returns target for patch generation
  // =====================================================

  // Compute where cursor should move, returns patches directly
  private computeMovePatches(d: Direction): Patch[] {
    const cursorParentLoc = this.getMyCursorParentLocation();
    if (!cursorParentLoc) return [];

    const content = this.getMyCursorContent();

    switch (d) {
      case 'Up': {
        // Move to parent of where cursor currently is
        const grandparentEdge = this.uniqueParentOfTermNode(cursorParentLoc.node);
        if (!grandparentEdge) return [];  // At root, can't go up

        // If cursor is inside a Proj, skip the Proj
        if (this.isProjectorNode(cursorParentLoc.node)) {
          // Recursively compute move from Proj's parent
          // For now, just go to the Proj itself
        }

        return this.createMoveCursorToTermPatches(cursorParentLoc.node);
      }

      case 'Down': {
        if (!content) return [];  // Nothing to go down into
        if (!('Node' in content)) return [];  // Reference, can't go down

        const contentNode = content.Node;
        const numChildren = this.numChildrenOfTermNode(contentNode);
        if (numChildren === 0) return [];  // Leaf node

        // Go to first child position (position 0, or position 1 for Proj/Cursor)
        let targetPosition = 0;
        if (this.isProjectorNode(contentNode) || this.isCursorTermNode(contentNode)) {
          targetPosition = 1;  // Skip metadata, go to content
        }

        const targetLoc: TermLocation = { node: contentNode, position: targetPosition };
        return this.createMoveCursorToLocationPatches(targetLoc);
      }

      case 'Right': {
        // Move to right sibling location
        const rightLoc = this.rightSiblingOfTermLocation(cursorParentLoc);

        // Check if we actually moved (rightSibling wraps around)
        // Use logical comparison (node ID + position) since path hashes may differ
        if (this.sameLogicalLocation(rightLoc, cursorParentLoc)) {
          return [];  // Only one sibling position, nowhere to go
        }

        return this.createMoveCursorToLocationPatches(rightLoc);
      }
    }

    return [];
  }

  // =====================================================
  // Action handlers
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

    const cursorParentLoc = this.getMyCursorParentLocation();
    if (!cursorParentLoc) return [];

    const content = this.getMyCursorContent();
    const l = this.termLocationToLocation(cursorParentLoc);

    const newPn = this.blossom.new_patch_node(c);
    const newSource = this.blossom.new_patch_location(newPn, position);
    const parentSource = this.blossom.patch_location_of_location(l);

    // First: unwrap cursor content to cursor's parent
    const patches: Patch[] = [...this.createUnwrapPatches()];

    // Then: insert new node at cursor's parent location
    patches.push(this.blossom.connection_patch(parentSource, newPn));

    // Move cursor to new node's content position
    const cursorNode = this.getMyCursorNode();
    if (cursorNode) {
      const cursorParentEdge = this.uniqueParentOfTermNode(cursorNode);
      if (cursorParentEdge) {
        patches.push(this.blossom.deletion_patch(cursorParentEdge.edge));
      }
      const newContentLoc = this.blossom.new_patch_location(newPn, position);
      patches.push(this.blossom.connection_patch(newContentLoc, this.blossom.patch_node_of_node(cursorNode.node)));
    }

    // If there was content, move it into the new node's position
    if (content && 'Node' in content) {
      // Content is now at cursor's former parent location (after unwrap)
      // Connect it to the new node's target position
      patches.push(this.blossom.connection_patch(newSource, this.blossom.patch_node_of_node(content.Node.node)));
    }

    return patches;
  }

  private computeInsert(c: Constructor): Patch[] {
    const cursorContentLoc = this.getMyCursorContentLocation();
    if (!cursorContentLoc) return [];

    const content = this.getMyCursorContent();
    if (content !== null) return [];  // Can only insert into empty location

    // Insert into cursor's content location (position 1), not cursor's parent
    return [this.insertionPatch(cursorContentLoc, c)];
  }

  private computeDelete(): Patch[] {
    const cursorParentLoc = this.getMyCursorParentLocation();
    if (!cursorParentLoc) return [];

    const l = this.termLocationToLocation(cursorParentLoc);

    // Delete cursor content
    const contentLoc = this.getMyCursorContentLocation();
    if (!contentLoc) return [];

    const contentLocGrove = this.termLocationToLocation(contentLoc);
    const edges: Edge[] = this.blossom.edge_children_of_location(contentLocGrove);
    return edges.map(e => this.blossom.deletion_patch(e));
  }

  private computeCut(): Patch[] {
    const cursorParentLoc = this.getMyCursorParentLocation();
    if (cursorParentLoc) {
      this.clipboard = { kind: 'Location', location: cursorParentLoc };
    }
    return [];
  }

  private computePaste(): Patch[] {
    if (this.clipboard.kind === 'Empty') return [];

    const cursorParentLoc = this.getMyCursorParentLocation();
    if (!cursorParentLoc) return [];

    const clipLoc = this.clipboard.location;
    const clipTerms = this.childrenOfTermLocation(clipLoc);
    if (clipTerms.length !== 1) return [];
    const clipTerm = clipTerms[0];
    if (!('Node' in clipTerm)) return [];

    this.clipboard = { kind: 'Empty' };

    const patches: Patch[] = [];
    const contentLoc = this.getMyCursorContentLocation();
    if (!contentLoc) return [];

    // Delete clip term from its location
    const clipEdge = this.uniqueParentEdgeOfTerm(clipTerm);
    if (clipEdge) {
      patches.push(this.blossom.deletion_patch(clipEdge.edge));
    }

    // Connect to cursor content
    const contentLocGrove = this.termLocationToLocation(contentLoc);
    patches.push(this.blossom.connection_patch(
      this.blossom.patch_location_of_location(contentLocGrove),
      this.blossom.patch_node_of_node(clipTerm.Node.node)
    ));

    return patches;
  }

  private computeTextInsert(x: string): Patch[] {
    const cursorContentLoc = this.getMyCursorContentLocation();
    if (!cursorContentLoc) return [];

    const content = this.getMyCursorContent();

    if (content && 'Node' in content) {
      // Append to existing identifier
      const tc = this.constructorOfTerm(content);
      if (!('Constructor' in tc)) return [];
      const gc = tc.Constructor;
      if (gc === 'Root' || !('Lang' in gc)) return [];
      const c = gc.Lang;
      if (typeof c !== 'object' || !('Identifier' in c)) return [];

      const id = c.Identifier;
      const patches = this.computeDelete();
      // Insert new identifier into cursor's content location
      patches.push(this.insertionPatch(cursorContentLoc, { Identifier: id + x }));
      return patches;
    } else {
      // Insert new identifier
      return this.computeInsert({ Identifier: x });
    }
  }

  private computeTextBackspace(): Patch[] {
    const content = this.getMyCursorContent();
    if (!content || !('Node' in content)) return [];

    const cursorContentLoc = this.getMyCursorContentLocation();
    if (!cursorContentLoc) return [];

    const tc = this.constructorOfTerm(content);
    if (!('Constructor' in tc)) return [];
    const gc = tc.Constructor;
    if (gc === 'Root' || !('Lang' in gc)) return [];
    const c = gc.Lang;
    if (typeof c !== 'object' || !('Identifier' in c)) return [];

    const id: string = c.Identifier;
    const patches = this.computeDelete();
    if (id.length > 1) {
      const newId = id.slice(0, -1);
      // Insert shortened identifier into cursor's content location
      patches.push(this.insertionPatch(cursorContentLoc, { Identifier: newId }));
    }
    return patches;
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
          return this.computeCut();
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
      return this.computeMovePatches(a.Move);
    } else if ('MoveToLocation' in a) {
      return this.createMoveCursorToLocationPatches(a.MoveToLocation);
    } else if ('MoveToTerm' in a) {
      if ('Node' in a.MoveToTerm) {
        return this.createMoveCursorToTermPatches(a.MoveToTerm.Node);
      }
      return [];
    } else if ('TextInsert' in a) {
      return this.computeTextInsert(a.TextInsert as string);
    }
    return [];
  }

  applyPatch(p: Patch): void {
    this.blossom.apply_patch(p);
  }

  runAllUpdates(): void {
    this.blossom.apply_blossom_action("AllUpdateSteps");
  }

  applyAction(a: Action): Patch[] {
    const patches = this.computeAction(a);
    for (const p of patches) {
      this.applyPatch(p);
    }
    return patches;
  }

  // =====================================================
  // Compatibility methods for existing interface
  // =====================================================

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

  get_term_at_cursor(): Term | null {
    return this.getMyCursorContent();
  }

  get_location_at_cursor(): TermLocation | null {
    const content = this.getMyCursorContent();
    if (content !== null) return null;  // Has content, not at a location
    // Cursor is at its content location (position 1), not its parent location
    return this.getMyCursorContentLocation();
  }

  // Get the node where cursor is attached (cursor's parent node)
  // Useful after WrapLeft/WrapRight to get the newly created wrapper node
  get_cursor_parent_term(): Term | null {
    const parentLoc = this.getMyCursorParentLocation();
    if (!parentLoc) return null;
    return { Node: parentLoc.node };
  }

  is_dirty_term(t: Term): boolean {
    return this.blossom.is_dirty_term(t);
  }

  is_dirty_location(tl: TermLocation): boolean {
    return this.blossom.is_dirty_location(tl);
  }

  // =====================================================
  // Cursor discovery (for initialization and multi-cursor)
  // =====================================================

  private getCursorIdentityFromNode(cursorTerm: Term): string | null {
    if (!this.isCursorNode(cursorTerm)) return null;
    if (!('Node' in cursorTerm)) return null;

    const cursorNode = cursorTerm.Node;
    const identityLocation: TermLocation = { node: cursorNode, position: 0 };
    const identityTerms = this.childrenOfTermLocation(identityLocation);
    if (identityTerms.length !== 1) return null;
    const identityTerm = identityTerms[0];
    const tc = this.constructorOfTerm(identityTerm);
    if ('Constructor' in tc) {
      const gc = tc.Constructor;
      if (gc !== 'Root' && 'Lang' in gc) {
        const c = gc.Lang;
        if (typeof c === 'object' && 'Identifier' in c) {
          return c.Identifier;
        }
      }
    }
    return null;
  }

  private getCursorContentLocation(cursorTerm: Term): TermLocation | null {
    if (!this.isCursorNode(cursorTerm)) return null;
    if (!('Node' in cursorTerm)) return null;
    return { node: cursorTerm.Node, position: 1 };
  }

  findAllCursors(): Array<{ cursorTerm: Term; identity: string; contentLocation: TermLocation; hasContent: boolean }> {
    const cursors: Array<{ cursorTerm: Term; identity: string; contentLocation: TermLocation; hasContent: boolean }> = [];
    this.findCursorsRecursive(this.rootTermLocation(), cursors);
    return cursors;
  }

  private findCursorsRecursive(
    location: TermLocation,
    cursors: Array<{ cursorTerm: Term; identity: string; contentLocation: TermLocation; hasContent: boolean }>
  ): void {
    const terms = this.childrenOfTermLocation(location);
    for (const t of terms) {
      if (this.isCursorNode(t)) {
        const identity = this.getCursorIdentityFromNode(t);
        const contentLoc = this.getCursorContentLocation(t);
        if (identity && contentLoc) {
          const contentTerms = this.childrenOfTermLocation(contentLoc);
          cursors.push({
            cursorTerm: t,
            identity,
            contentLocation: contentLoc,
            hasContent: contentTerms.length > 0
          });
          this.findCursorsRecursive(contentLoc, cursors);
        }
      } else {
        const children = this.childrenOfTerm(t);
        for (const childLoc of children) {
          this.findCursorsRecursive(childLoc, cursors);
        }
      }
    }
  }

  isCursorWrapping(t: Term, identity: string): Term | null {
    const parentEdge = this.uniqueParentEdgeOfTerm(t);
    if (!parentEdge) return null;
    const parentLocation = this.sourceOfTermEdge(parentEdge);
    const grandparentNode = parentLocation.node;
    const grandparentTerm: Term = { Node: grandparentNode };
    if (this.isCursorNode(grandparentTerm)) {
      const cursorIdentity = this.getCursorIdentityFromNode(grandparentTerm);
      if (cursorIdentity === identity && parentLocation.position === 1) {
        return grandparentTerm;
      }
    }
    return null;
  }

  isCursorAtLocation(tl: TermLocation, identity: string): Term | null {
    const terms = this.childrenOfTermLocation(tl);
    for (const t of terms) {
      if (this.isCursorNode(t)) {
        const cursorIdentity = this.getCursorIdentityFromNode(t);
        if (cursorIdentity === identity) {
          const contentLoc = this.getCursorContentLocation(t);
          if (contentLoc) {
            const contentTerms = this.childrenOfTermLocation(contentLoc);
            if (contentTerms.length === 0) {
              return t;
            }
          }
        }
      }
    }
    return null;
  }

  // =====================================================
  // Canvas position map methods
  // =====================================================

  createPositionMapPatches(posListLocation: TermLocation, positions: Map<string, { x: number; y: number }>): Patch[] {
    const patches: Patch[] = [];
    const l = this.termLocationToLocation(posListLocation);

    const edges: Edge[] = this.blossom.edge_children_of_location(l);
    for (const e of edges) {
      patches.push(this.blossom.deletion_patch(e));
    }

    const entries = Array.from(positions.entries());

    if (entries.length === 0) {
      const source = this.blossom.patch_location_of_location(l);
      const nilNode = this.blossom.new_patch_node("PosNil");
      patches.push(this.blossom.connection_patch(source, nilNode));
    } else {
      let tailNode = this.blossom.new_patch_node("PosNil");

      for (let i = entries.length - 1; i >= 0; i--) {
        const [nodeId, pos] = entries[i];
        const consNode = this.blossom.new_patch_node("PosCons");

        const nodeIdLocation = this.blossom.new_patch_location(consNode, 0);
        const nodeIdNode = this.blossom.new_patch_node({ Identifier: nodeId });
        patches.push(this.blossom.connection_patch(nodeIdLocation, nodeIdNode));

        const xLocation = this.blossom.new_patch_location(consNode, 1);
        const xNode = this.blossom.new_patch_node({ Identifier: String(Math.round(pos.x)) });
        patches.push(this.blossom.connection_patch(xLocation, xNode));

        const yLocation = this.blossom.new_patch_location(consNode, 2);
        const yNode = this.blossom.new_patch_node({ Identifier: String(Math.round(pos.y)) });
        patches.push(this.blossom.connection_patch(yLocation, yNode));

        const tailLocation = this.blossom.new_patch_location(consNode, 3);
        patches.push(this.blossom.connection_patch(tailLocation, tailNode));

        tailNode = consNode;
      }

      const source = this.blossom.patch_location_of_location(l);
      patches.push(this.blossom.connection_patch(source, tailNode));
    }

    return patches;
  }
}
