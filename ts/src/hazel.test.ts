import { describe, it, expect, beforeAll } from "vitest";
import { readFileSync } from "fs";
import { resolve } from "path";
import { initSync, HazelState } from "./pkg";

// Helper: check if a RenderNode is a specific constructor
function isConstructor(node: any, name: string): boolean {
  return node?.kind === "term" && node?.constructor === name;
}

describe("HazelState end-to-end", () => {
  beforeAll(() => {
    const wasmPath = resolve(__dirname, "../..", "rust/pkg/rust_bg.wasm");
    const wasmBytes = readFileSync(wasmPath);
    initSync({ module: wasmBytes });
  });

  it("creates a state and initializes", () => {
    const state = new HazelState();
    state.genesis();
    state.update_all();

    const tree = state.render();
    expect(tree).toBeDefined();
    expect(tree.kind).toBe("hole");
  });

  it("initializes cursor", () => {
    const state = new HazelState();
    state.genesis();

    const patches = state.init_cursor("test-session");
    expect(patches.length).toBeGreaterThan(0);

    state.update_all();
    const tree = state.render();
    expect(tree).toBeDefined();
    expect(tree.kind).toBe("term");
    expect(tree.constructor).toBe("Cursor");
  });

  it("inserts a Zero", () => {
    const state = new HazelState();
    state.genesis();
    state.init_cursor("test");
    state.update_all();

    const patches = state.perform_action({ Insert: "Zero" });
    expect(patches.length).toBeGreaterThan(0);

    const tree = state.render();
    expect(tree.kind).toBe("term");
    expect(tree.constructor).toBe("Cursor");
    const content = tree.slots.find((s: any) => s.position === 1);
    expect(content).toBeDefined();
    expect(content.content.kind).toBe("term");
    expect(content.content.constructor).toBe("Zero");
  });

  it("type checks Zero as Num", () => {
    const state = new HazelState();
    state.genesis();
    state.init_cursor("test");

    state.perform_action({ Insert: "Zero" });
    const info = state.cursor_info();
    // syn is now a RenderNode, not a string
    expect(isConstructor(info.syn, "Num")).toBe(true);
  });

  it("handles text insert for identifiers", () => {
    const state = new HazelState();
    state.genesis();
    state.init_cursor("test");

    state.perform_action({ TextInsert: "x" });
    const info = state.cursor_info();
    expect(info.contentConstructor).toBe("Identifier");
  });

  it("handles delete", () => {
    const state = new HazelState();
    state.genesis();
    state.init_cursor("test");

    state.perform_action({ Insert: "Zero" });
    expect(state.cursor_info().hasContent).toBe(true);

    state.perform_action("Delete");
    expect(state.cursor_info().hasContent).toBe(false);
  });

  it("patches are serializable", () => {
    const state = new HazelState();
    state.genesis();
    const patches = state.init_cursor("test");

    for (const p of patches) {
      expect(p.sign).toBeDefined();
      expect(p.edge).toBeDefined();
      expect(p.edge.id).toBeDefined();
      const json = JSON.stringify(p);
      const roundtrip = JSON.parse(json);
      expect(roundtrip.sign).toBe(p.sign);
    }
  });

  it("sort inconsistency produces marks", () => {
    const state = new HazelState();
    state.genesis();
    state.init_cursor("test");

    state.perform_action({ Insert: "Num" });
    const info = state.cursor_info();
    expect(info.marks.length).toBeGreaterThan(0);
    expect(info.marks[0].kind).toBe("sort");
  });

  it("cursor navigation works", () => {
    const state = new HazelState();
    state.genesis();
    state.init_cursor("test");

    state.perform_action({ WrapLeft: "Plus" });
    const info1 = state.cursor_info();
    expect(info1.sort).toBe("Expression");

    state.perform_action({ Insert: "Zero" });
    expect(isConstructor(state.cursor_info().syn, "Num")).toBe(true);

    state.perform_action({ Move: "Right" });
    expect(state.cursor_info().hasContent).toBe(false);

    state.perform_action({ Insert: "Zero" });

    state.perform_action({ Move: "Up" });
    const info2 = state.cursor_info();
    expect(isConstructor(info2.syn, "Num")).toBe(true);
  });

  it("cut and paste work", () => {
    const state = new HazelState();
    state.genesis();
    state.init_cursor("test");

    state.perform_action({ Insert: "Zero" });
    expect(state.cursor_info().hasContent).toBe(true);

    state.perform_action("Cut");
    expect(state.cursor_info().hasContent).toBe(true);

    state.perform_action({ Move: "Up" });
    state.perform_action({ WrapLeft: "Plus" });
    state.perform_action({ Move: "Right" });
    expect(state.cursor_info().hasContent).toBe(false);

    state.perform_action("Paste");
    expect(state.cursor_info().hasContent).toBe(true);
    expect(isConstructor(state.cursor_info().syn, "Num")).toBe(true);
  });
});
