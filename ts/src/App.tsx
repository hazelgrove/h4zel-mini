import { useEffect, useRef, useState, useCallback } from "react";
import type { DocHandle } from "@automerge/react";
import {
  groveToAutomerge,
  grovePatchesFromDocHandle,
  amPatchToGrovePatch,
  type GroveDoc,
} from "./Automerge";
import init, { HazelState } from "./pkg";
import type { Action } from "./RustTypes";

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
      ana?: RenderNode;
      syn?: RenderNode;
      marks: RenderMark[];
    }
  | { kind: "conflict"; children: RenderNode[]; locNode: string; locPos: number }
  | { kind: "ref"; id: string };

type RenderMark =
  | { kind: "sort"; expected: string; actual: string }
  | { kind: "type"; expected: RenderNode; actual: RenderNode };

interface CursorInfo {
  sort?: string;
  ana?: RenderNode;
  syn?: RenderNode;
  marks: RenderMark[];
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
        case "G": return { WrapWithProjector: "Canvas" };
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
    case "ArrowLeft": return { Move: "Left" };
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
    case "]": return { WrapWithProjector: "Canvas" };
  }

  // Text insert (letters)
  if (e.key.length === 1 && /[a-zA-Z]/.test(e.key)) {
    return { TextInsert: e.key };
  }

  return null;
}

type TermNode = Extract<RenderNode, { kind: "term" }>;

// ── Helpers ──────────────────────────────────────────────────────────────────

/** Look through Cursor/Proj (transparent wrappers) to find a Canvas node. */
function findCanvasNode(node: RenderNode | undefined): TermNode | null {
  if (!node || node.kind !== "term") return null;
  if (node.constructor === "Canvas") return node;
  if (node.constructor === "Cursor" || node.constructor === "Proj") {
    const content = node.slots.find((s) => s.position === 1)?.content;
    return findCanvasNode(content);
  }
  return null;
}

// ── Render tree component ────────────────────────────────────────────────────

const noop = () => {};

function RenderNodeView({
  node,
  onClickTerm = noop,
  onClickHole = noop,
  onCanvasDrag = noop as any,
}: {
  node: RenderNode;
  onClickTerm?: (id: string) => void;
  onClickHole?: (locNode: string, locPos: number) => void;
  onCanvasDrag?: (canvasId: string, positions: { node_id: string; x: number; y: number }[]) => void;
}) {
  const interactive = onClickTerm !== noop;

  if (node.kind === "hole") {
    return (
      <span
        className="hole"
        onClick={interactive ? (e) => { e.stopPropagation(); onClickHole(node.locNode, node.locPos); } : undefined}
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
            <RenderNodeView node={child} onClickTerm={onClickTerm} onClickHole={onClickHole} onCanvasDrag={onCanvasDrag} />
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

  const handleClick = interactive
    ? (e: React.MouseEvent) => { e.stopPropagation(); onClickTerm(t.id); }
    : undefined;

  // Special rendering for Cursor: just render content (position 1)
  if (t.constructor === "Cursor") {
    const content = t.slots.find((s) => s.position === 1);
    return (
      <span className={`cursor-wrapper ${cursorClass}`} onClick={handleClick}>
        {content ? (
          <RenderNodeView node={content.content} onClickTerm={onClickTerm} onClickHole={onClickHole} onCanvasDrag={onCanvasDrag} />
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
    return <RenderNodeView node={content.content} onClickTerm={onClickTerm} onClickHole={onClickHole} onCanvasDrag={onCanvasDrag} />;
  }

  // Render children helper
  const child = (pos: number) => {
    const slot = t.slots.find((s) => s.position === pos);
    if (!slot) return <span className="hole">⬚</span>;
    return <RenderNodeView node={slot.content} onClickTerm={onClickTerm} onClickHole={onClickHole} onCanvasDrag={onCanvasDrag} />;
  };

  // Proj: render based on projector type
  if (t.constructor === "Proj") {
    const projTypeSlot = t.slots.find((s) => s.position === 0);
    const canvasNode = findCanvasNode(projTypeSlot?.content);

    if (canvasNode) {
      return (
        <CanvasView
          projNode={t}
          canvasNode={canvasNode}
          onClickTerm={onClickTerm}
          onCanvasDrag={onCanvasDrag}
        />
      );
    }

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
      inner = <span className="unknown">{t.constructor}</span>;
      break;
  }

  return (
    <span className={`term ${classes}`} onClick={handleClick}>
      {inner}
    </span>
  );
}

// ── Canvas projector ─────────────────────────────────────────────────────────

type GraphNode = { id: string; label: string; x: number; y: number; cursor: string; slots: { position: number; childId: string | null }[]; renderNode?: RenderNode };
type Wire = { fromId: string; fromPos: number; toId: string };

/** Read positions from the PosCons linked list in the render tree. */
function readPositions(canvasNode: TermNode): Map<string, { x: number; y: number }> {
  const map = new Map<string, { x: number; y: number }>();
  let current: RenderNode | undefined = canvasNode.slots.find(s => s.position === 0)?.content;
  for (let i = 0; i < 1000 && current; i++) {
    if (current.kind !== "term" || current.constructor !== "PosCons") break;
    const slots = current.slots;
    const nodeIdent = slots.find(s => s.position === 0)?.content;
    const xNode = slots.find(s => s.position === 1)?.content;
    const yNode = slots.find(s => s.position === 2)?.content;
    const tail = slots.find(s => s.position === 3)?.content;
    if (nodeIdent?.kind === "term" && nodeIdent.value &&
        xNode?.kind === "term" && xNode.value &&
        yNode?.kind === "term" && yNode.value) {
      map.set(nodeIdent.value, { x: parseFloat(xNode.value), y: parseFloat(yNode.value) });
    }
    current = tail;
  }
  return map;
}

const LABEL_MAP: Record<string, string> = {
  Typ: "□", Num: "ℕ", Zero: "0", Plus: "+", Prod: "×", Pair: ",",
  Arrow: "→", Fun: "fun", Ap: "◁", Asc: ":", Let: "let",
};

/**
 * Collect graph nodes and wires from a render tree.
 * Cursor nodes are invisible — we look through them, inheriting their highlight.
 */
function collectGraph(root: RenderNode, positions: Map<string, { x: number; y: number }>) {
  const nodes: GraphNode[] = [];
  const wires: Wire[] = [];
  let idx = 0;

  // Peel all Cursor wrappers, tracking whether we're inside own/other cursor
  function peel(node: RenderNode, cursorCtx: string): { node: RenderNode; cursor: string } {
    if (node.kind === "term" && node.constructor === "Cursor") {
      const cur = node.cursor !== "none" ? node.cursor : cursorCtx;
      const content = node.slots.find(s => s.position === 1)?.content;
      if (content) return peel(content, cur);
      // Empty cursor → return as hole
      return { node: { kind: "hole", locNode: "", locPos: 0 } as RenderNode, cursor: cur };
    }
    return { node, cursor: cursorCtx };
  }

  function visit(raw: RenderNode, cursorCtx: string): string | null {
    const { node, cursor } = peel(raw, cursorCtx);
    if (node.kind !== "term") return null;

    // If this node is a Proj (projector wrapper), treat the entire subtree
    // as a single opaque canvas node rendered via structural UI — never
    // expose projector internals as bare graph nodes.
    if (node.constructor === "Proj") {
      const pos = positions.get(node.id) ?? {
        x: 80 + (idx % 5) * 140,
        y: 60 + Math.floor(idx / 5) * 100,
      };
      const gn: GraphNode = {
        id: node.id, label: "", x: pos.x, y: pos.y,
        cursor, slots: [], renderNode: raw,
      };
      nodes.push(gn);
      idx++;
      return node.id;
    }

    const pos = positions.get(node.id) ?? {
      x: 80 + (idx % 5) * 140,
      y: 60 + Math.floor(idx / 5) * 100,
    };
    const label = node.constructor === "Identifier"
      ? (node.value ?? "?")
      : (LABEL_MAP[node.constructor] ?? node.constructor);

    const gn: GraphNode = {
      id: node.id, label, x: pos.x, y: pos.y,
      cursor, slots: [],
    };
    nodes.push(gn);
    idx++;

    for (const slot of node.slots) {
      const childId = visit(slot.content, "none");
      gn.slots.push({ position: slot.position, childId });
      if (childId) {
        wires.push({ fromId: node.id, fromPos: slot.position, toId: childId });
      }
    }
    return node.id;
  }

  visit(root, "none");
  return { nodes, wires };
}

const NODE_W = 60;
const NODE_H = 36;

/** Persists canvas sizes across remounts (keyed by canvas node ID). */
const canvasSizes = new Map<string, { w: number; h: number }>();

function CanvasView({
  projNode,
  canvasNode,
  onClickTerm,
  onCanvasDrag,
}: {
  projNode: TermNode;
  canvasNode: TermNode;
  onClickTerm: (id: string) => void;
  onCanvasDrag: (canvasId: string, positions: { node_id: string; x: number; y: number }[]) => void;
}) {
  const svgRef = useRef<SVGSVGElement>(null);
  const [dragState, setDragState] = useState<{
    id: string; startX: number; startY: number; origX: number; origY: number;
  } | null>(null);
  const [localPositions, setLocalPositions] = useState<Map<string, { x: number; y: number }>>(new Map());
  const [userSize, _setUserSize] = useState<{ w: number; h: number } | null>(canvasSizes.get(canvasNode.id) ?? null);
  const setUserSize = useCallback((size: { w: number; h: number } | null) => {
    _setUserSize(size);
    if (size) canvasSizes.set(canvasNode.id, size);
    else canvasSizes.delete(canvasNode.id);
  }, [canvasNode.id]);
  const [resizing, setResizing] = useState<{ startX: number; startY: number; origW: number; origH: number } | null>(null);

  // Always read content — may be empty, that's fine
  const contentSlot = projNode.slots.find(s => s.position === 1);
  const contentNode = contentSlot?.content;

  const storedPositions = readPositions(canvasNode);
  const positions = new Map(storedPositions);
  for (const [k, v] of localPositions) positions.set(k, v);

  const { nodes, wires } = contentNode
    ? collectGraph(contentNode, positions)
    : { nodes: [], wires: [] };
  const nodeMap = new Map(nodes.map(n => [n.id, n]));
  const canvasId = canvasNode.id;

  const handleMouseDown = (nodeId: string, e: React.MouseEvent) => {
    e.stopPropagation();
    const node = nodeMap.get(nodeId);
    if (!node) return;
    setDragState({ id: nodeId, startX: e.clientX, startY: e.clientY, origX: node.x, origY: node.y });
  };

  const handleMouseMove = useCallback((e: MouseEvent) => {
    if (!dragState) return;
    const dx = e.clientX - dragState.startX;
    const dy = e.clientY - dragState.startY;
    setLocalPositions(prev => {
      const next = new Map(prev);
      next.set(dragState.id, { x: dragState.origX + dx, y: dragState.origY + dy });
      return next;
    });
  }, [dragState]);

  const handleMouseUp = useCallback(() => {
    if (!dragState) return;
    const allPositions = nodes.map(n => {
      const local = localPositions.get(n.id);
      return { node_id: n.id, x: local?.x ?? n.x, y: local?.y ?? n.y };
    });
    onCanvasDrag(canvasId, allPositions);
    setDragState(null);
    setLocalPositions(new Map());
  }, [dragState, nodes, localPositions, canvasId, onCanvasDrag]);

  // Resize handlers
  const handleResizeMouseMove = useCallback((e: MouseEvent) => {
    if (!resizing) return;
    const w = Math.max(200, resizing.origW + (e.clientX - resizing.startX));
    const h = Math.max(100, resizing.origH + (e.clientY - resizing.startY));
    setUserSize({ w, h });
  }, [resizing]);

  const handleResizeMouseUp = useCallback(() => {
    setResizing(null);
  }, []);

  useEffect(() => {
    if (dragState) {
      window.addEventListener("mousemove", handleMouseMove);
      window.addEventListener("mouseup", handleMouseUp);
      return () => {
        window.removeEventListener("mousemove", handleMouseMove);
        window.removeEventListener("mouseup", handleMouseUp);
      };
    }
  }, [dragState, handleMouseMove, handleMouseUp]);

  useEffect(() => {
    if (resizing) {
      window.addEventListener("mousemove", handleResizeMouseMove);
      window.addEventListener("mouseup", handleResizeMouseUp);
      return () => {
        window.removeEventListener("mousemove", handleResizeMouseMove);
        window.removeEventListener("mouseup", handleResizeMouseUp);
      };
    }
  }, [resizing, handleResizeMouseMove, handleResizeMouseUp]);

  // Compute SVG bounds: content-fit or user override, whichever is larger
  const contentX = Math.max(400, ...nodes.map(n => (localPositions.get(n.id)?.x ?? n.x) + NODE_W + 20));
  const contentY = Math.max(200, ...nodes.map(n => (localPositions.get(n.id)?.y ?? n.y) + NODE_H + 20));
  const maxX = userSize ? Math.max(userSize.w, contentX) : contentX;
  const maxY = userSize ? Math.max(userSize.h, contentY) : contentY;

  return (
    <svg ref={svgRef} className="canvas-svg" width={maxX} height={maxY}>
      {/* Wires */}
      {wires.map((w, i) => {
        const from = nodeMap.get(w.fromId);
        const to = nodeMap.get(w.toId);
        if (!from || !to) return null;
        const fx = (localPositions.get(from.id)?.x ?? from.x) + NODE_W / 2;
        const fy = (localPositions.get(from.id)?.y ?? from.y) + NODE_H;
        const tx = (localPositions.get(to.id)?.x ?? to.x) + NODE_W / 2;
        const ty = (localPositions.get(to.id)?.y ?? to.y);
        return <line key={i} x1={fx} y1={fy} x2={tx} y2={ty} className="canvas-wire" />;
      })}
      {/* Nodes */}
      {nodes.map(n => {
        const x = localPositions.get(n.id)?.x ?? n.x;
        const y = localPositions.get(n.id)?.y ?? n.y;
        const cls = n.cursor === "own" ? "canvas-node cursor-own" : n.cursor === "other" ? "canvas-node cursor-other" : "canvas-node";

        // Projector nodes render their full structural UI via foreignObject
        if (n.renderNode) {
          return (
            <g key={n.id} transform={`translate(${x},${y})`}
               onMouseDown={(e) => handleMouseDown(n.id, e)}
               className={cls}>
              <foreignObject width={200} height={100} overflow="visible">
                <div className="canvas-embedded-node">
                  <RenderNodeView
                    node={n.renderNode}
                    onClickTerm={onClickTerm}
                    onClickHole={noop}
                    onCanvasDrag={onCanvasDrag}
                  />
                </div>
              </foreignObject>
            </g>
          );
        }

        return (
          <g key={n.id} transform={`translate(${x},${y})`}
             onMouseDown={(e) => handleMouseDown(n.id, e)}
             onClick={(e) => { e.stopPropagation(); onClickTerm(n.id); }}
             className={cls}>
            <rect width={NODE_W} height={NODE_H} rx={4} />
            <text x={NODE_W / 2} y={NODE_H / 2 + 5} textAnchor="middle">{n.label}</text>
          </g>
        );
      })}
      {/* Resize handle */}
      <g className="canvas-resize-handle"
         onMouseDown={(e) => {
           e.stopPropagation();
           setResizing({ startX: e.clientX, startY: e.clientY, origW: maxX, origH: maxY });
         }}>
        <rect x={maxX - 16} y={maxY - 16} width={16} height={16} fill="transparent" />
        <path d={`M${maxX - 3} ${maxY - 12}L${maxX - 3} ${maxY - 3}L${maxX - 12} ${maxY - 3}`}
              className="canvas-resize-grip" />
        <path d={`M${maxX - 3} ${maxY - 7}L${maxX - 3} ${maxY - 3}L${maxX - 7} ${maxY - 3}`}
              className="canvas-resize-grip" />
      </g>
    </svg>
  );
}

// ── Inspector ────────────────────────────────────────────────────────────────

function TypeView({ node }: { node: RenderNode }) {
  return <RenderNodeView node={node} />;
}

function MarkView({ mark }: { mark: RenderMark }) {
  if (mark.kind === "sort") {
    return (
      <span className="mark-detail">
        <span className="inspector-label">sort:</span>
        <span className="mark-expected">{mark.expected}</span>
        <span className="op"> ≠ </span>
        <span className="mark-actual">{mark.actual}</span>
      </span>
    );
  }
  return (
    <span className="mark-detail">
      <span className="mark-expected"><TypeView node={mark.expected} /></span>
      <span className="op"> ≠ </span>
      <span className="mark-actual"><TypeView node={mark.actual} /></span>
    </span>
  );
}

function Inspector({ info }: { info: CursorInfo | null }) {
  if (!info) return null;
  return (
    <div className="inspector">
      <div className="inspector-row">
        <span className="inspector-label">sort</span>
        <span>{info.sort ?? "—"}</span>
      </div>
      <div className="inspector-row">
        <span className="inspector-label">ana</span>
        {info.ana ? <TypeView node={info.ana} /> : <span className="dim">—</span>}
      </div>
      <div className="inspector-row">
        <span className="inspector-label">syn</span>
        {info.syn ? <TypeView node={info.syn} /> : <span className="dim">—</span>}
      </div>
      {info.marks.map((mark, i) => (
        <div key={i} className="inspector-row inspector-mark">
          <MarkView mark={mark} />
        </div>
      ))}
    </div>
  );
}

// ── App ──────────────────────────────────────────────────────────────────────

export default function App({ handle }: { handle: DocHandle<GroveDoc> }) {
  const stateRef = useRef<HazelState | null>(null);
  const [renderTree, setRenderTree] = useState<RenderNode | null>(null);
  const [cursorInfo, setCursorInfo] = useState<CursorInfo | null>(null);
  const [ready, setReady] = useState(false);
  const [syncEnabled, setSyncEnabled] = useState(true);
  const outBufferRef = useRef<any[]>([]);
  const inBufferRef = useRef<any[]>([]);

  /** Send patches to Automerge, or buffer them if sync is paused. */
  const sendPatches = useCallback(
    (patches: any[]) => {
      if (!patches || patches.length === 0) return;
      if (syncEnabled) {
        groveToAutomerge(patches, handle);
      } else {
        outBufferRef.current.push(...patches);
      }
    },
    [syncEnabled, handle],
  );

  /** Flush both outgoing and incoming buffers when sync is re-enabled. */
  useEffect(() => {
    if (!syncEnabled) return;
    const state = stateRef.current;

    // Flush outgoing
    if (outBufferRef.current.length > 0) {
      groveToAutomerge(outBufferRef.current, handle);
      outBufferRef.current = [];
    }

    // Apply buffered incoming
    if (state && inBufferRef.current.length > 0) {
      for (const grovePatch of inBufferRef.current) {
        state.apply_patch(grovePatch);
      }
      inBufferRef.current = [];
      state.update_all();
      setRenderTree(state.render());
      setCursorInfo(state.cursor_info());
    }
  }, [syncEnabled, handle]);

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

      // Save cursor patches — goes through same sync path as all other patches
      if (cursorPatches && cursorPatches.length > 0) {
        if (syncEnabled) {
          groveToAutomerge(cursorPatches, handle);
        } else {
          outBufferRef.current.push(...cursorPatches);
        }
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

      const grovePatches: any[] = [];
      for (const amPatch of amPatches) {
        const grovePatch = amPatchToGrovePatch(amPatch);
        if (grovePatch) grovePatches.push(grovePatch);
      }
      if (grovePatches.length === 0) return;

      if (!syncEnabled) {
        // Buffer incoming patches until sync is re-enabled
        inBufferRef.current.push(...grovePatches);
        return;
      }

      for (const grovePatch of grovePatches) {
        state.apply_patch(grovePatch);
      }
      state.update_all();
      setRenderTree(state.render());
      setCursorInfo(state.cursor_info());
    };

    handle.on("change", onChange);
    return () => {
      handle.off("change", onChange);
    };
  }, [handle, ready, syncEnabled]);

  // Keyboard handler
  const handleKeyDown = useCallback(
    (e: KeyboardEvent) => {
      const state = stateRef.current;
      if (!state) return;

      const action = keyToAction(e);
      if (!action) return;

      e.preventDefault();

      const patches = state.perform_action(action);

      sendPatches(patches);

      // Re-render
      setRenderTree(state.render());
      setCursorInfo(state.cursor_info());
    },
    [handle, sendPatches],
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
      sendPatches(patches);
      setRenderTree(state.render());
      setCursorInfo(state.cursor_info());
    },
    [handle, sendPatches],
  );

  const onClickHole = useCallback(
    (locNode: string, locPos: number) => {
      const state = stateRef.current;
      if (!state) return;

      const patches = state.perform_action({
        MoveToLocation: { node: locNode, position: locPos },
      });
      sendPatches(patches);
      setRenderTree(state.render());
      setCursorInfo(state.cursor_info());
    },
    [handle, sendPatches],
  );

  const onCanvasDrag = useCallback(
    (canvasId: string, positions: { node_id: string; x: number; y: number }[]) => {
      const state = stateRef.current;
      if (!state) return;

      const patches = state.perform_action({
        CanvasDrag: { canvas: canvasId, positions },
      });
      sendPatches(patches);
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
          onCanvasDrag={onCanvasDrag}
        />
      </div>
      <Inspector info={cursorInfo} />
      <div className="help">
        <div>←↑↓→ navigate</div>
        <div>a–z  identifier · 0  zero</div>
        <div>+  plus · *  product · ,  pair · -  arrow · :  ascription · space  apply</div>
        <div>Ctrl+F  fun · Ctrl+L  let · Ctrl+T  type · Ctrl+N  nat</div>
        <div>Backspace  delete · Shift+Backspace  delete last char</div>
        <div>Ctrl+X  cut · Ctrl+V  paste</div>
        <div>[  structural projector · ]  canvas projector</div>
      </div>
      <label className="sync-toggle">
        <input
          type="checkbox"
          checked={syncEnabled}
          onChange={(e) => setSyncEnabled(e.target.checked)}
        />
        sync
        {!syncEnabled && (outBufferRef.current.length > 0 || inBufferRef.current.length > 0) && (
          <span className="sync-buffered"> ({outBufferRef.current.length}↑ {inBufferRef.current.length}↓)</span>
        )}
      </label>
    </div>
  );
}
