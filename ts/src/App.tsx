import { useEffect, useRef, useState, useCallback } from "react";
import type { DocHandle } from "@automerge/react";
import {
  groveToAutomerge,
  grovePatchesFromDocHandle,
  amPatchToGrovePatch,
  type GroveDoc,
} from "./Automerge";
import init, { HazelState } from "./pkg";
import type { Action, Constructor } from "./RustTypes";

// ── Types for the render tree coming from Rust ───────────────────────────────

interface RenderSlot {
  position: number;
  content: RenderNode;
}

type RenderNode =
  | { kind: "hole"; locNode: string; locPos: number }
  | {
      kind: "term";
      id: string;
      constructor: string;
      value?: string;
      slots: RenderSlot[];
      cursor: string;
      clipboard: boolean;
      sort?: string;
      ana?: string;
      syn?: string;
      marks: string[];
    }
  | { kind: "conflict"; children: RenderNode[]; locNode: string; locPos: number }
  | { kind: "ref"; id: string };

interface CursorInfo {
  sort?: string;
  ana?: string;
  syn?: string;
  marks: string[];
  hasContent: boolean;
  contentConstructor?: string;
}

// ── Session ID ───────────────────────────────────────────────────────────────

function getSessionId(): string {
  let id = sessionStorage.getItem("hazel-session-id");
  if (!id) {
    id = crypto.randomUUID();
    sessionStorage.setItem("hazel-session-id", id);
  }
  return id;
}

// ── Keyboard → Action mapping (spec section 16) ─────────────────────────────

function keyToAction(e: KeyboardEvent): Action | null {
  // Ctrl combos
  if (e.ctrlKey || e.metaKey) {
    if (e.shiftKey) {
      switch (e.key) {
        case "S": return { Insert: "Structural" };
        case "C": return { Insert: "Collapsed" };
        case "G": return { Insert: "Canvas" };
      }
    }
    switch (e.key) {
      case "x": return "Cut";
      case "v": return "Paste";
      case "t": return { Insert: "Typ" };
      case "n": return { Insert: "Num" };
      case "p": return { WrapLeft: "Prod" };
      case "f": return { WrapLeft: "Fun" };
      case "l": return { WrapLeft: "Let" };
      case "u": return { BlossomAction: "UpdateStep" };
    }
    return null;
  }

  // Navigation
  switch (e.key) {
    case "ArrowUp": return { Move: "Up" };
    case "ArrowDown": return { Move: "Down" };
    case "ArrowRight": return { Move: "Right" };
  }

  // Editing
  switch (e.key) {
    case "Backspace":
      return e.shiftKey ? "TextBackspace" : "Delete";
    case "0": return { Insert: "Zero" };
    case "+": return { WrapLeft: "Plus" };
    case "*": return { WrapLeft: "Prod" };
    case ",": return { WrapLeft: "Pair" };
    case "-": return { WrapLeft: "Arrow" };
    case " ": return { WrapLeft: "Ap" };
    case ":": return { WrapLeft: "Asc" };
    case "[": return { WrapWithProjector: "Structural" };
  }

  // Text insert (letters)
  if (e.key.length === 1 && /[a-zA-Z]/.test(e.key)) {
    return { TextInsert: e.key };
  }

  return null;
}

// ── Render tree component ────────────────────────────────────────────────────

function RenderNodeView({
  node,
  onClickTerm,
  onClickHole,
}: {
  node: RenderNode;
  onClickTerm: (id: string) => void;
  onClickHole: (locNode: string, locPos: number) => void;
}) {
  if (node.kind === "hole") {
    return (
      <span
        className="hole"
        onClick={(e) => {
          e.stopPropagation();
          onClickHole(node.locNode, node.locPos);
        }}
      >
        ⬚
      </span>
    );
  }

  if (node.kind === "ref") {
    return <span className="reference">🌀</span>;
  }

  if (node.kind === "conflict") {
    return (
      <span className="conflict">
        {"⟨"}
        {node.children.map((child, i) => (
          <span key={i}>
            {i > 0 && " | "}
            <RenderNodeView node={child} onClickTerm={onClickTerm} onClickHole={onClickHole} />
          </span>
        ))}
        {"⟩"}
      </span>
    );
  }

  // Term node
  const t = node;
  const cursorClass =
    t.cursor === "own" ? "cursor-own" : t.cursor === "other" ? "cursor-other" : "";
  const markClass = t.marks.length > 0 ? "has-marks" : "";
  const classes = [cursorClass, markClass, t.clipboard ? "clipboard" : ""].filter(Boolean).join(" ");

  const handleClick = (e: React.MouseEvent) => {
    e.stopPropagation();
    onClickTerm(t.id);
  };

  // Special rendering for Cursor: just render content (position 1)
  if (t.constructor === "Cursor") {
    const content = t.slots.find((s) => s.position === 1);
    return (
      <span className={`cursor-wrapper ${cursorClass}`} onClick={handleClick}>
        {content ? (
          <RenderNodeView node={content.content} onClickTerm={onClickTerm} onClickHole={onClickHole} />
        ) : (
          <span className="hole cursor-own">⬚</span>
        )}
      </span>
    );
  }

  // Special rendering for Root: just render content
  if (t.constructor === "Root") {
    const content = t.slots[0];
    if (!content) return <span className="hole">⬚</span>;
    return <RenderNodeView node={content.content} onClickTerm={onClickTerm} onClickHole={onClickHole} />;
  }

  // Render children helper
  const child = (pos: number) => {
    const slot = t.slots.find((s) => s.position === pos);
    if (!slot) return <span className="hole">⬚</span>;
    return <RenderNodeView node={slot.content} onClickTerm={onClickTerm} onClickHole={onClickHole} />;
  };

  // Proj: render based on projector type
  if (t.constructor === "Proj") {
    return (
      <span className={`proj ${classes}`} onClick={handleClick}>
        <span className="proj-badge">▣</span>
        {child(1)}
      </span>
    );
  }

  // Constructor-specific rendering
  let inner: React.ReactNode;
  switch (t.constructor) {
    case "Typ":
      inner = "□";
      break;
    case "Num":
      inner = "ℕ";
      break;
    case "Zero":
      inner = "0";
      break;
    case "Plus":
      inner = <>{child(0)}<span className="op"> + </span>{child(1)}</>;
      break;
    case "Prod":
      inner = <>{child(0)}<span className="op"> × </span>{child(1)}</>;
      break;
    case "Pair":
      inner = <>{child(0)}<span className="op">, </span>{child(1)}</>;
      break;
    case "Arrow":
      inner = <>{child(0)}<span className="op"> → </span>{child(1)}</>;
      break;
    case "Fun":
      inner = <><span className="kw">fun </span>{child(0)}<span className="op"> ↦ </span>{child(1)}</>;
      break;
    case "Ap":
      inner = <>{child(0)}<span className="op"> ◁ </span>{child(1)}</>;
      break;
    case "Asc":
      inner = <>{child(0)}<span className="op"> : </span>{child(1)}</>;
      break;
    case "Let":
      inner = (
        <>
          <span className="kw">let </span>{child(0)}<span className="op"> = </span>
          {child(1)}<span className="kw"> in </span>{child(2)}
        </>
      );
      break;
    case "Identifier":
      inner = <span className="identifier">{t.value || "?"}</span>;
      break;
    default:
      // Metadata constructors, etc.
      inner = <span className="unknown">{t.constructor}</span>;
      break;
  }

  return (
    <span className={`term ${classes}`} onClick={handleClick}>
      {inner}
    </span>
  );
}

// ── Inspector ────────────────────────────────────────────────────────────────

function Inspector({ info }: { info: CursorInfo | null }) {
  if (!info) return null;
  return (
    <div className="inspector">
      <div className="inspector-row">
        <span className="inspector-label">sort:</span>
        <span>{info.sort ?? "—"}</span>
      </div>
      <div className="inspector-row">
        <span className="inspector-label">ana:</span>
        <span>{info.ana ?? "—"}</span>
      </div>
      <div className="inspector-row">
        <span className="inspector-label">syn:</span>
        <span>{info.syn ?? "—"}</span>
      </div>
      {info.marks.length > 0 && (
        <div className="inspector-row marks">
          <span className="inspector-label">marks:</span>
          <span>{info.marks.join(", ")}</span>
        </div>
      )}
      {info.contentConstructor && (
        <div className="inspector-row">
          <span className="inspector-label">ctor:</span>
          <span>{info.contentConstructor}</span>
        </div>
      )}
    </div>
  );
}

// ── App ──────────────────────────────────────────────────────────────────────

export default function App({ handle }: { handle: DocHandle<GroveDoc> }) {
  const stateRef = useRef<HazelState | null>(null);
  const [renderTree, setRenderTree] = useState<RenderNode | null>(null);
  const [cursorInfo, setCursorInfo] = useState<CursorInfo | null>(null);
  const [ready, setReady] = useState(false);

  // Initialize WASM and state
  useEffect(() => {
    let cancelled = false;

    async function initialize() {
      // Initialize WASM
      await init();

      if (cancelled) return;

      const state = new HazelState();
      stateRef.current = state;

      // Load existing patches from Automerge
      const existingPatches = grovePatchesFromDocHandle(handle);

      if (existingPatches.length > 0) {
        // Existing document: replay all patches (order doesn't matter — CRDT)
        for (const patch of existingPatches) {
          state.apply_patch(patch);
        }
      }

      // Ensure root exists (genesis is idempotent)
      state.genesis();

      // Run type updates
      state.update_all();

      // Initialize cursor
      const sessionId = getSessionId();
      const cursorPatches = state.init_cursor(sessionId);

      // Save cursor patches to Automerge
      if (cursorPatches && cursorPatches.length > 0) {
        groveToAutomerge(cursorPatches, handle);
      }

      state.update_all();

      // Initial render
      setRenderTree(state.render());
      setCursorInfo(state.cursor_info());
      setReady(true);
    }

    initialize().catch(console.error);

    return () => {
      cancelled = true;
    };
  }, [handle]);

  // Listen for Automerge changes (remote patches)
  useEffect(() => {
    if (!ready) return;

    const onChange = ({ patches: amPatches }: { patches: any[] }) => {
      const state = stateRef.current;
      if (!state) return;

      let applied = false;
      for (const amPatch of amPatches) {
        const grovePatch = amPatchToGrovePatch(amPatch);
        if (grovePatch) {
          state.apply_patch(grovePatch);
          applied = true;
        }
      }

      if (applied) {
        state.update_all();
        setRenderTree(state.render());
        setCursorInfo(state.cursor_info());
      }
    };

    handle.on("change", onChange);
    return () => {
      handle.off("change", onChange);
    };
  }, [handle, ready]);

  // Keyboard handler
  const handleKeyDown = useCallback(
    (e: KeyboardEvent) => {
      const state = stateRef.current;
      if (!state) return;

      const action = keyToAction(e);
      if (!action) return;

      e.preventDefault();

      const patches = state.perform_action(action);

      // Save patches to Automerge
      if (patches && patches.length > 0) {
        groveToAutomerge(patches, handle);
      }

      // Re-render
      setRenderTree(state.render());
      setCursorInfo(state.cursor_info());
    },
    [handle],
  );

  useEffect(() => {
    window.addEventListener("keydown", handleKeyDown);
    return () => window.removeEventListener("keydown", handleKeyDown);
  }, [handleKeyDown]);

  // Click handlers
  const onClickTerm = useCallback(
    (id: string) => {
      const state = stateRef.current;
      if (!state) return;

      const patches = state.perform_action({ MoveToTerm: { id } });
      if (patches && patches.length > 0) {
        groveToAutomerge(patches, handle);
      }
      setRenderTree(state.render());
      setCursorInfo(state.cursor_info());
    },
    [handle],
  );

  const onClickHole = useCallback(
    (locNode: string, locPos: number) => {
      const state = stateRef.current;
      if (!state) return;

      const patches = state.perform_action({
        MoveToLocation: { node: locNode, position: locPos },
      });
      if (patches && patches.length > 0) {
        groveToAutomerge(patches, handle);
      }
      setRenderTree(state.render());
      setCursorInfo(state.cursor_info());
    },
    [handle],
  );

  if (!ready || !renderTree) {
    return <div className="loading">Loading...</div>;
  }

  return (
    <div className="hazel-app">
      <div className="editor">
        <RenderNodeView
          node={renderTree}
          onClickTerm={onClickTerm}
          onClickHole={onClickHole}
        />
      </div>
      <Inspector info={cursorInfo} />
      <div className="help">
        ↑↓→ navigate · letters = identifier · 0 = zero · + * , - : space = wrap ·
        Ctrl+F = fun · Ctrl+L = let · Backspace = delete · [ = projector
      </div>
    </div>
  );
}
