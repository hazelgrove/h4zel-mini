import { describe, it, expect, beforeEach, beforeAll } from 'vitest';
import { readFile } from 'fs/promises';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';
import init from './pkg/rust';
import { Controller, TermLocation, Term, TermNode } from './Controller';

// Mock sessionStorage for Node environment
const mockSessionStorage: Record<string, string> = {};
(globalThis as any).sessionStorage = {
  getItem: (key: string) => mockSessionStorage[key] || null,
  setItem: (key: string, value: string) => { mockSessionStorage[key] = value; },
  removeItem: (key: string) => { delete mockSessionStorage[key]; },
  clear: () => { Object.keys(mockSessionStorage).forEach(k => delete mockSessionStorage[k]); },
};

// Initialize WASM before tests - load from file for Node.js
beforeAll(async () => {
  const __filename = fileURLToPath(import.meta.url);
  const __dirname = dirname(__filename);
  const wasmPath = join(__dirname, '..', '..', 'rust', 'pkg', 'rust_bg.wasm');
  const wasmBuffer = await readFile(wasmPath);
  await init(wasmBuffer);
});

// Helper to create a controller with cursor initialized
function createTestController(): Controller {
  // Clear session storage so each test gets fresh identity
  mockSessionStorage['hazel_cursor_identity'] = 'test-cursor-' + Math.random().toString(36).slice(2);

  const controller = new Controller();

  // Create and apply cursor patches
  const cursorPatches = controller.getInitialCursorPatches();
  for (const p of cursorPatches) {
    controller.apply_patch(p);
  }

  // Initialize identity node reference
  const cursors = controller.findAllCursors();
  const myCursor = cursors.find(c => c.identity === controller.getCursorIdentity());
  if (myCursor) {
    controller.initializeIdentityNode(myCursor);
  }

  return controller;
}

// Helper to apply action and return patches
function applyAction(controller: Controller, action: any): any[] {
  const patches = controller.apply_serial_action(action);
  for (const p of patches) {
    controller.apply_patch(p);
  }
  return patches;
}

// Helper to get a string representation of the tree for debugging
function treeToString(controller: Controller, loc?: TermLocation, depth = 0): string {
  const location = loc ?? controller.root_location();
  const indent = '  '.repeat(depth);
  const children = controller.children_of_location(location);

  if (children.length === 0) {
    return `${indent}?`;  // hole
  }

  const lines: string[] = [];
  for (const child of children) {
    const tc = controller.constructor_of_term(child);
    let name: string;
    if ('Root' in tc) {
      name = 'Root';
    } else if ('Constructor' in tc) {
      if (tc.Constructor === 'Root') {
        name = 'Root';
      } else if ('Lang' in tc.Constructor) {
        const lang = tc.Constructor.Lang;
        if (typeof lang === 'string') {
          name = lang;
        } else if (typeof lang === 'object' && 'Identifier' in lang) {
          name = `"${lang.Identifier}"`;
        } else {
          name = JSON.stringify(lang);
        }
      } else if ('Identifier' in tc.Constructor) {
        name = `"${tc.Constructor.Identifier}"`;
      } else {
        name = JSON.stringify(tc.Constructor);
      }
    } else {
      name = JSON.stringify(tc);
    }

    // Handle Cursor specially - show as [cursor: content]
    if (name === 'Cursor') {
      const contentLoc = getCursorContentLocation(controller, child);
      if (contentLoc) {
        const contentStr = treeToString(controller, contentLoc, 0).trim();
        lines.push(`${indent}[cursor: ${contentStr}]`);
      } else {
        lines.push(`${indent}[cursor]`);
      }
    } else if ('Node' in child) {
      // Check children of this node
      const tn = child.Node;
      const arity = getArity(controller, child);
      if (arity === 0) {
        lines.push(`${indent}${name}`);
      } else {
        const childStrs: string[] = [];
        for (let pos = 0; pos < arity; pos++) {
          const childLoc: TermLocation = { node: tn, position: pos };
          const childStr = treeToString(controller, childLoc, 0).trim();
          childStrs.push(childStr);
        }
        lines.push(`${indent}(${name} ${childStrs.join(' ')})`);
      }
    } else {
      lines.push(`${indent}ref`);
    }
  }

  return lines.join('\n');
}

// Get the arity of a term
function getArity(controller: Controller, term: Term): number {
  const tc = controller.constructor_of_term(term);
  if ('Constructor' in tc) {
    const c = tc.Constructor;
    if (c === 'Root') return 1;
    if (typeof c === 'object' && 'Lang' in c) {
      // Known arities
      const arities: Record<string, number> = {
        Plus: 2, Prod: 2, Pair: 2, Arrow: 2, Ap: 2, Asc: 2,
        Let: 3, Fun: 2, Proj: 2, Cursor: 2, Labeled: 1,
        Zero: 0, Num: 0, Typ: 0, Structural: 0, Collapsed: 0, Canvas: 0,
      };
      return arities[c.Lang] ?? 0;
    }
    if (typeof c === 'object' && 'Identifier' in c) {
      return 0;
    }
  }
  return 0;
}

// Get cursor's content location
function getCursorContentLocation(controller: Controller, cursorTerm: Term): TermLocation | null {
  if ('Node' in cursorTerm) {
    return { node: cursorTerm.Node, position: 1 };
  }
  return null;
}

describe('Controller', () => {
  describe('initialization', () => {
    it('creates a controller with cursor at root', () => {
      const controller = createTestController();
      const cursors = controller.findAllCursors();

      expect(cursors.length).toBe(1);
      expect(cursors[0].identity).toBe(controller.getCursorIdentity());
    });

    it('cursor starts at hole (no content)', () => {
      const controller = createTestController();
      const cursors = controller.findAllCursors();

      expect(cursors[0].hasContent).toBe(false);
    });
  });

  describe('insert operations', () => {
    it('inserting Zero creates a Zero node', () => {
      const controller = createTestController();

      applyAction(controller, { Insert: 'Zero' });

      const tree = treeToString(controller);
      expect(tree).toContain('Zero');
    });

    it('inserting identifier creates identifier node', () => {
      const controller = createTestController();

      applyAction(controller, { TextInsert: 'x' });

      const tree = treeToString(controller);
      expect(tree).toContain('"x"');
    });
  });

  describe('wrap operations', () => {
    it('wrapping with Plus creates Plus with cursor in left child', () => {
      const controller = createTestController();

      // Insert Zero first
      applyAction(controller, { Insert: 'Zero' });
      // Wrap with Plus
      applyAction(controller, { WrapLeft: 'Plus' });

      const tree = treeToString(controller);
      // Should have (Plus [cursor: Zero] ?)
      expect(tree).toContain('Plus');
      expect(tree).toContain('Zero');
    });

    it('inserting Plus then moving down and right does not crash', () => {
      const controller = createTestController();

      // This was a reported crash scenario
      applyAction(controller, { WrapLeft: 'Plus' });
      applyAction(controller, { Move: 'Down' });
      applyAction(controller, { Move: 'Right' });

      // Should not throw, and cursor should be somewhere valid
      const cursors = controller.findAllCursors();
      expect(cursors.length).toBe(1);
    });
  });

  describe('cursor movement', () => {
    it('Move Down enters a node', () => {
      const controller = createTestController();

      // Create (Plus ? ?)
      applyAction(controller, { WrapLeft: 'Plus' });

      // Cursor should wrap the Plus
      let cursors = controller.findAllCursors();
      expect(cursors[0].hasContent).toBe(true);

      // Move down into first child
      applyAction(controller, { Move: 'Down' });

      // Cursor should now be at hole
      cursors = controller.findAllCursors();
      expect(cursors[0].hasContent).toBe(false);
    });

    it('Move Right cycles through siblings', () => {
      const controller = createTestController();

      // Create (Plus ? ?) and enter it
      applyAction(controller, { WrapLeft: 'Plus' });
      applyAction(controller, { Move: 'Down' });

      // At first child (hole), move right
      applyAction(controller, { Move: 'Right' });

      // Should be at second child
      // Move right again should cycle back to first
      applyAction(controller, { Move: 'Right' });

      // Should not crash
      const cursors = controller.findAllCursors();
      expect(cursors.length).toBe(1);
    });

    it('Move Up exits a node', () => {
      const controller = createTestController();

      applyAction(controller, { WrapLeft: 'Plus' });
      applyAction(controller, { Move: 'Down' });

      // Now at first child, move up
      applyAction(controller, { Move: 'Up' });

      // Should be wrapping Plus again
      const cursors = controller.findAllCursors();
      expect(cursors[0].hasContent).toBe(true);
    });
  });

  describe('delete operations', () => {
    it('Delete removes wrapped content', () => {
      const controller = createTestController();

      applyAction(controller, { Insert: 'Zero' });

      // Verify Zero exists
      let tree = treeToString(controller);
      expect(tree).toContain('Zero');

      // Delete it
      applyAction(controller, 'Delete');

      // Should just have cursor at hole
      tree = treeToString(controller);
      expect(tree).not.toContain('Zero');
    });
  });

  describe('projector operations', () => {
    it('wrapping with Proj creates a projector', () => {
      const controller = createTestController();

      applyAction(controller, { Insert: 'Zero' });
      applyAction(controller, { WrapRight: 'Proj' });

      const tree = treeToString(controller);
      expect(tree).toContain('Proj');
    });

    it('cursor cannot enter projector position 0', () => {
      const controller = createTestController();

      // Create Proj with content
      applyAction(controller, { Insert: 'Zero' });
      applyAction(controller, { WrapRight: 'Proj' });

      // Now cursor is inside Proj at position 1
      // Move right should NOT enter position 0
      applyAction(controller, { Move: 'Right' });

      // Should still be valid
      const cursors = controller.findAllCursors();
      expect(cursors.length).toBe(1);
    });

    it('inserting Canvas projector type creates Canvas node', () => {
      const controller = createTestController();

      applyAction(controller, { Insert: 'Canvas' });

      const tree = treeToString(controller);
      expect(tree).toContain('Canvas');
    });

    it('Proj with Canvas type has correct structure', () => {
      const controller = createTestController();

      applyAction(controller, { Insert: 'Zero' });
      applyAction(controller, { WrapRight: 'Proj' });

      const cursors = controller.findAllCursors();
      expect(cursors.length).toBe(1);
      expect(cursors[0].hasContent).toBe(true);

      // Cursor should wrap a Proj
      const cursorContent = controller.children_of_location(cursors[0].contentLocation);
      expect(cursorContent.length).toBe(1);
    });

    it('creating Proj on hole - cursor wraps Proj', () => {
      const controller = createTestController();

      applyAction(controller, { WrapRight: 'Proj' });

      const tree = treeToString(controller);
      expect(tree).toContain('Proj');
    });

    it('cursor inside Proj remains findable after navigation', () => {
      const controller = createTestController();

      applyAction(controller, { Insert: 'Zero' });
      applyAction(controller, { WrapRight: 'Proj' });

      // Move down into Proj content (position 1)
      applyAction(controller, { Move: 'Down' });

      let cursors = controller.findAllCursors();
      expect(cursors.length).toBe(1);

      // Move down again into Zero
      applyAction(controller, { Move: 'Down' });

      cursors = controller.findAllCursors();
      expect(cursors.length).toBe(1);
    });

    it('Proj on hole allows moving into content position', () => {
      const controller = createTestController();

      applyAction(controller, { WrapRight: 'Proj' });
      applyAction(controller, { Move: 'Down' });

      const cursors = controller.findAllCursors();
      expect(cursors.length).toBe(1);
    });

    it('MoveToLocation can access Proj position 0 for setting type', () => {
      const controller = createTestController();

      applyAction(controller, { WrapRight: 'Proj' });

      const cursors = controller.findAllCursors();
      const projTerms = controller.children_of_location(cursors[0].contentLocation);
      expect(projTerms.length).toBe(1);

      if ('Node' in projTerms[0]) {
        const projNode = projTerms[0].Node;
        const pos0Location: TermLocation = { node: projNode, position: 0 };

        applyAction(controller, { MoveToLocation: pos0Location });
        applyAction(controller, { Insert: 'Canvas' });

        const tree = treeToString(controller);
        expect(tree).toContain('Canvas');
      }
    });

    it('complete Canvas projector creation workflow', () => {
      const controller = createTestController();

      // Create content and wrap with Proj
      applyAction(controller, { Insert: 'Zero' });
      applyAction(controller, { WrapRight: 'Proj' });

      // Get Proj node and set type
      let cursors = controller.findAllCursors();
      const projTerms = controller.children_of_location(cursors[0].contentLocation);

      if (projTerms.length === 1 && 'Node' in projTerms[0]) {
        const projNode = projTerms[0].Node;
        const pos0Location: TermLocation = { node: projNode, position: 0 };

        applyAction(controller, { MoveToLocation: pos0Location });
        applyAction(controller, { Insert: 'Canvas' });

        const tree = treeToString(controller);
        expect(tree).toContain('Canvas');
        expect(tree).toContain('Zero');
      }

      cursors = controller.findAllCursors();
      expect(cursors.length).toBe(1);
    });

    it('navigating into and out of Proj', () => {
      const controller = createTestController();

      // Create (Plus Zero ?) and wrap second hole with Proj
      applyAction(controller, { Insert: 'Zero' });
      applyAction(controller, { WrapLeft: 'Plus' });
      applyAction(controller, { Move: 'Down' });
      applyAction(controller, { Move: 'Right' });
      applyAction(controller, { WrapRight: 'Proj' });

      // Move into Proj
      applyAction(controller, { Move: 'Down' });

      let cursors = controller.findAllCursors();
      expect(cursors.length).toBe(1);

      // Move out of Proj
      applyAction(controller, { Move: 'Up' });

      cursors = controller.findAllCursors();
      expect(cursors.length).toBe(1);
    });
  });

  describe('complex sequences', () => {
    it('building (+ 0 0) works correctly', () => {
      const controller = createTestController();

      // Start: cursor at hole
      // Insert Zero
      applyAction(controller, { Insert: 'Zero' });
      // Wrap with Plus: (Plus [cursor: Zero] ?)
      applyAction(controller, { WrapLeft: 'Plus' });
      // Move down: cursor at Zero
      applyAction(controller, { Move: 'Down' });
      // Move right: cursor at second hole
      applyAction(controller, { Move: 'Right' });
      // Insert Zero
      applyAction(controller, { Insert: 'Zero' });

      const tree = treeToString(controller);
      // Should have (Plus Zero [cursor: Zero])
      expect(tree).toContain('Plus');
      // Count Zero occurrences
      const zeroCount = (tree.match(/Zero/g) || []).length;
      expect(zeroCount).toBe(2);
    });

    it('cut and paste works', () => {
      const controller = createTestController();

      // Create Zero
      applyAction(controller, { Insert: 'Zero' });
      // Cut it
      applyAction(controller, 'Cut');

      // Should be at hole now
      let cursors = controller.findAllCursors();
      expect(cursors[0].hasContent).toBe(false);

      // Create a Plus
      applyAction(controller, { WrapLeft: 'Plus' });
      applyAction(controller, { Move: 'Down' });

      // Paste Zero
      applyAction(controller, 'Paste');

      // Should have Zero in the Plus
      const tree = treeToString(controller);
      expect(tree).toContain('Plus');
      expect(tree).toContain('Zero');
    });
  });
});
