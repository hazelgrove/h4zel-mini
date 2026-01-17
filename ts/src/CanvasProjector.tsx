import { useState, useRef, useEffect, useCallback } from "react";
import { Controller } from "./Controller";
import type { TermLocation, Term, TermNode } from "./Controller";

// Position for a node on the canvas
interface NodePosition {
  x: number;
  y: number;
}

// Node data for rendering
interface CanvasNode {
  id: string;
  term: Term;
  termNode: TermNode;
  label: string;
  children: TermLocation[];
  // If this is a Proj with non-Canvas projector, store info for embedded rendering
  isEmbedded: boolean;
  embeddedLocation?: TermLocation; // The content location to render embedded
}

// Edge data for rendering
interface CanvasEdge {
  id: string;
  fromNodeId: string;
  fromPosition: number;
  toNodeId: string;
}

// Props for the canvas projector
interface CanvasProjectorProps {
  controller: Controller;
  contentLocation: TermLocation;
  rerender: () => void;
  renderLocation: (controller: Controller, location: TermLocation, rerender: () => void) => React.ReactNode;
  updateInspectorsForTerm: (controller: Controller, term: Term) => void;
  updateInspectorsForLocation: (controller: Controller, location: TermLocation) => void;
}

// Generate a unique string ID for a term node
function termNodeId(tn: TermNode): string {
  return JSON.stringify(tn);
}

// Get a display label for a constructor
function getConstructorLabel(controller: Controller, term: Term): string {
  const tc = controller.constructor_of_term(term);
  if ("Constructor" in tc) {
    const gc = tc.Constructor;
    if (gc === "Root") return "Root";
    if ("Lang" in gc) {
      const c = gc.Lang;
      if (typeof c === "string") return c;
      if ("Identifier" in c) return c.Identifier;
    }
  } else if ("Reference" in tc) {
    return "Ref";
  }
  return "?";
}

// Check if a term is a Proj node
function isProj(controller: Controller, term: Term): boolean {
  const tc = controller.constructor_of_term(term);
  if ("Constructor" in tc) {
    const gc = tc.Constructor;
    if (gc !== "Root" && "Lang" in gc) {
      return gc.Lang === "Proj";
    }
  }
  return false;
}

// Get the projector type from a Proj node (returns null if not a recognized projector)
function getProjectorType(controller: Controller, term: Term): string | null {
  if (!isProj(controller, term)) return null;

  const children = controller.children_of_term(term);
  if (children.length < 2) return null;

  const projTypeLocation = children[0];
  const projTypeTerms = controller.children_of_location(projTypeLocation);
  if (projTypeTerms.length !== 1) return null;

  const projTypeTerm = projTypeTerms[0];
  const tc = controller.constructor_of_term(projTypeTerm);
  if ("Constructor" in tc) {
    const gc = tc.Constructor;
    if (gc !== "Root" && "Lang" in gc) {
      const c = gc.Lang;
      if (c === "Structural" || c === "Collapsed" || c === "Labeled" || c === "Canvas") {
        return c as string;
      }
    }
  }
  return null;
}

// Recursively collect all nodes and edges from a term tree
// Stops at Proj nodes with non-Canvas projectors (they become embedded nodes)
function collectNodesAndEdges(
  controller: Controller,
  location: TermLocation,
  nodes: Map<string, CanvasNode>,
  edges: CanvasEdge[],
  visited: Set<string>
): void {
  const terms = controller.children_of_location(location);

  for (const term of terms) {
    if (!("Node" in term)) continue; // Skip references for now

    const tn = term.Node;
    const id = termNodeId(tn);

    if (visited.has(id)) continue;
    visited.add(id);

    const children = controller.children_of_term(term);
    const label = getConstructorLabel(controller, term);

    // Check if this is a Proj with any projector type - all projectors are embedded
    const embeddedType = getProjectorType(controller, term);

    if (embeddedType !== null) {
      // This is an embedded projector - don't traverse into it
      // The content location is children[1] (position 1 of Proj)
      nodes.set(id, {
        id,
        term,
        termNode: tn,
        label: `[${embeddedType}]`,
        children: [], // No children to wire - it's embedded
        isEmbedded: true,
        embeddedLocation: children[1],
      });
      // Don't create edges or recurse into children
      continue;
    }

    nodes.set(id, {
      id,
      term,
      termNode: tn,
      label,
      children,
      isEmbedded: false,
    });

    // Create edges and recurse into children
    for (let i = 0; i < children.length; i++) {
      const childLocation = children[i];
      const childTerms = controller.children_of_location(childLocation);

      for (const childTerm of childTerms) {
        if ("Node" in childTerm) {
          const childId = termNodeId(childTerm.Node);
          edges.push({
            id: `${id}-${i}-${childId}`,
            fromNodeId: id,
            fromPosition: i,
            toNodeId: childId,
          });
        }
      }

      collectNodesAndEdges(controller, childLocation, nodes, edges, visited);
    }
  }
}

// Auto-layout nodes in a simple tree layout
function autoLayout(
  nodes: Map<string, CanvasNode>,
  edges: CanvasEdge[],
  rootId: string | null
): Map<string, NodePosition> {
  const positions = new Map<string, NodePosition>();

  if (!rootId || !nodes.has(rootId)) {
    // Fallback: arrange in a grid
    let x = 50;
    let y = 50;
    for (const [id] of nodes) {
      positions.set(id, { x, y });
      x += 180;
      if (x > 600) {
        x = 50;
        y += 120;
      }
    }
    return positions;
  }

  // Build adjacency map
  const childrenMap = new Map<string, string[]>();
  for (const edge of edges) {
    const children = childrenMap.get(edge.fromNodeId) || [];
    if (!children.includes(edge.toNodeId)) {
      children.push(edge.toNodeId);
    }
    childrenMap.set(edge.fromNodeId, children);
  }

  // BFS layout
  const visited = new Set<string>();
  const queue: { id: string; depth: number; index: number }[] = [
    { id: rootId, depth: 0, index: 0 },
  ];
  const depthCounts = new Map<number, number>();

  while (queue.length > 0) {
    const { id, depth } = queue.shift()!;
    if (visited.has(id)) continue;
    visited.add(id);

    const count = depthCounts.get(depth) || 0;
    depthCounts.set(depth, count + 1);

    positions.set(id, {
      x: 50 + count * 180,
      y: 50 + depth * 120,
    });

    const children = childrenMap.get(id) || [];
    for (let i = 0; i < children.length; i++) {
      if (!visited.has(children[i])) {
        queue.push({ id: children[i], depth: depth + 1, index: i });
      }
    }
  }

  // Handle any unvisited nodes (disconnected)
  let offsetX = 50;
  for (const [id] of nodes) {
    if (!positions.has(id)) {
      positions.set(id, { x: offsetX, y: 450 });
      offsetX += 180;
    }
  }

  return positions;
}

// Wire drag state
interface WireDragState {
  fromNodeId: string;
  fromSlotIndex: number;
  fromLocation: TermLocation;
  startX: number;
  startY: number;
  currentX: number;
  currentY: number;
}

export function CanvasProjector({ controller, contentLocation, rerender, renderLocation, updateInspectorsForTerm, updateInspectorsForLocation }: CanvasProjectorProps) {
  const [positions, setPositions] = useState<Map<string, NodePosition>>(new Map());
  const [dragging, setDragging] = useState<string | null>(null);
  const [dragOffset, setDragOffset] = useState<{ x: number; y: number }>({ x: 0, y: 0 });
  const [wireDrag, setWireDrag] = useState<WireDragState | null>(null);
  const svgRef = useRef<SVGSVGElement>(null);

  // Collect nodes and edges from the tree
  const nodes = new Map<string, CanvasNode>();
  const edges: CanvasEdge[] = [];
  const visited = new Set<string>();
  collectNodesAndEdges(controller, contentLocation, nodes, edges, visited);

  // Find root node (first node added)
  const rootId = nodes.size > 0 ? nodes.keys().next().value : null;

  // Initialize positions if needed
  useEffect(() => {
    if (positions.size === 0 && nodes.size > 0) {
      setPositions(autoLayout(nodes, edges, rootId ?? null));
    }
  }, [nodes.size]);

  // Re-layout when nodes change significantly
  useEffect(() => {
    const currentIds = new Set(positions.keys());
    const newIds = new Set(nodes.keys());

    // Check if we have new nodes that need positioning
    let needsLayout = false;
    for (const id of newIds) {
      if (!currentIds.has(id)) {
        needsLayout = true;
        break;
      }
    }

    if (needsLayout) {
      const newPositions = autoLayout(nodes, edges, rootId ?? null);
      // Preserve existing positions for nodes that haven't moved
      for (const [id, pos] of positions) {
        if (newIds.has(id)) {
          newPositions.set(id, pos);
        }
      }
      setPositions(newPositions);
    }
  }, [JSON.stringify([...nodes.keys()])]);

  // Mouse handlers for dragging
  const handleMouseDown = useCallback((e: React.MouseEvent, nodeId: string) => {
    e.preventDefault();
    e.stopPropagation();
    const pos = positions.get(nodeId);
    if (pos && svgRef.current) {
      const rect = svgRef.current.getBoundingClientRect();
      setDragging(nodeId);
      setDragOffset({
        x: e.clientX - rect.left - pos.x,
        y: e.clientY - rect.top - pos.y,
      });
    }
  }, [positions]);

  const handleMouseMove = useCallback((e: React.MouseEvent) => {
    if (svgRef.current) {
      const rect = svgRef.current.getBoundingClientRect();

      // Handle node dragging
      if (dragging) {
        const newX = e.clientX - rect.left - dragOffset.x;
        const newY = e.clientY - rect.top - dragOffset.y;

        setPositions((prev) => {
          const next = new Map(prev);
          next.set(dragging, { x: Math.max(0, newX), y: Math.max(0, newY) });
          return next;
        });
      }

      // Handle wire dragging
      if (wireDrag) {
        const currentX = e.clientX - rect.left;
        const currentY = e.clientY - rect.top;
        setWireDrag((prev) => prev ? { ...prev, currentX, currentY } : null);
      }
    }
  }, [dragging, dragOffset, wireDrag]);

  const handleMouseUp = useCallback(() => {
    setDragging(null);
    setWireDrag(null);
  }, []);

  // Wire drag handlers
  const handleWireMouseDown = useCallback((
    e: React.MouseEvent,
    nodeId: string,
    slotIndex: number,
    location: TermLocation,
    slotX: number,
    slotY: number
  ) => {
    e.preventDefault();
    e.stopPropagation();
    if (svgRef.current) {
      const rect = svgRef.current.getBoundingClientRect();
      const currentX = e.clientX - rect.left;
      const currentY = e.clientY - rect.top;
      setWireDrag({
        fromNodeId: nodeId,
        fromSlotIndex: slotIndex,
        fromLocation: location,
        startX: slotX,
        startY: slotY,
        currentX,
        currentY,
      });
    }
  }, []);

  // Find node at a given position (for wire drop target detection)
  const findNodeAtPosition = useCallback((x: number, y: number): CanvasNode | null => {
    for (const [nodeId, node] of nodes) {
      const pos = positions.get(nodeId);
      if (!pos) continue;
      const height = node.isEmbedded ? embeddedMinHeight : nodeHeight;
      // Check if near the input port (top center)
      const portX = pos.x + nodeWidth / 2;
      const portY = pos.y;
      const dist = Math.sqrt((x - portX) ** 2 + (y - portY) ** 2);
      if (dist < slotRadius * 2) {
        return node;
      }
    }
    return null;
  }, [nodes, positions]);

  // Handle wire drop
  const handleWireDrop = useCallback((e: React.MouseEvent) => {
    if (!wireDrag || !svgRef.current) return;

    const rect = svgRef.current.getBoundingClientRect();
    const dropX = e.clientX - rect.left;
    const dropY = e.clientY - rect.top;

    const targetNode = findNodeAtPosition(dropX, dropY);

    if (targetNode && targetNode.id !== wireDrag.fromNodeId) {
      // For now, select the source location when wire is dropped on a target
      // Future: could implement cut/paste or reference creation
      controller.move_to_location(wireDrag.fromLocation);
      rerender();
    }

    setWireDrag(null);
  }, [wireDrag, findNodeAtPosition, controller, rerender]);

  // Click handler to select a node
  const handleNodeClick = useCallback((nodeId: string) => {
    const node = nodes.get(nodeId);
    if (node) {
      controller.move_to_term(node.term);
      updateInspectorsForTerm(controller, node.term);
      rerender();
    }
  }, [controller, nodes, rerender, updateInspectorsForTerm]);

  // Click handler to select a location (empty slot)
  const handleSlotClick = useCallback((location: TermLocation) => {
    controller.move_to_location(location);
    updateInspectorsForLocation(controller, location);
    rerender();
  }, [controller, rerender, updateInspectorsForLocation]);

  // Render constants (defined here but also used in callbacks above - they're hoisted)
  const nodeWidth = 140;
  const nodeHeight = 60;
  const embeddedMinHeight = 80;
  const slotRadius = 8;

  // Calculate SVG dimensions based on node positions
  let maxX = 700;
  let maxY = 500;
  for (const [nodeId, pos] of positions) {
    const node = nodes.get(nodeId);
    const height = node?.isEmbedded ? embeddedMinHeight : nodeHeight;
    maxX = Math.max(maxX, pos.x + nodeWidth + 50);
    maxY = Math.max(maxY, pos.y + height + 50);
  }

  // Check if cursor is at the canvas content location (for empty canvas highlighting)
  const isCanvasSelected = controller.cursor_at_location(contentLocation);
  const cursorColor = "rgb(157, 229, 242)";
  const fadedCursorColor = "rgba(157, 229, 242, 0.3)";

  return (
    <svg
      ref={svgRef}
      width={maxX}
      height={maxY}
      style={{
        border: isCanvasSelected ? `2px solid ${cursorColor}` : "1px solid #ccc",
        borderRadius: "4px",
        backgroundColor: isCanvasSelected ? fadedCursorColor : "#fafafa",
        cursor: dragging ? "grabbing" : "default",
      }}
      onMouseMove={handleMouseMove}
      onMouseUp={(e) => {
        if (wireDrag) {
          handleWireDrop(e);
        } else {
          handleMouseUp();
        }
      }}
      onMouseLeave={handleMouseUp}
    >
      {/* Render edges */}
      {edges.map((edge) => {
        const fromPos = positions.get(edge.fromNodeId);
        const toPos = positions.get(edge.toNodeId);
        const fromNode = nodes.get(edge.fromNodeId);
        const toNode = nodes.get(edge.toNodeId);

        if (!fromPos || !toPos || !fromNode || !toNode) return null;

        // Calculate slot position (bottom of node)
        const slotCount = fromNode.children.length;
        const slotSpacing = nodeWidth / (slotCount + 1);
        const fromX = fromPos.x + slotSpacing * (edge.fromPosition + 1);
        const fromY = fromPos.y + nodeHeight;

        // Target position - adjust for embedded nodes
        // Embedded nodes use foreignObject with padding, so target slightly inside
        const toX = toNode.isEmbedded ? toPos.x + 10 : toPos.x + nodeWidth / 2;
        const toY = toNode.isEmbedded ? toPos.y + 10 : toPos.y;

        // Bezier curve for the wire
        const midY = (fromY + toY) / 2;

        return (
          <path
            key={edge.id}
            d={`M ${fromX} ${fromY} C ${fromX} ${midY}, ${toX} ${midY}, ${toX} ${toY}`}
            fill="none"
            stroke="#666"
            strokeWidth={2}
          />
        );
      })}

      {/* Render dragging wire */}
      {wireDrag && (
        <path
          d={`M ${wireDrag.startX} ${wireDrag.startY} C ${wireDrag.startX} ${(wireDrag.startY + wireDrag.currentY) / 2}, ${wireDrag.currentX} ${(wireDrag.startY + wireDrag.currentY) / 2}, ${wireDrag.currentX} ${wireDrag.currentY}`}
          fill="none"
          stroke="#0088aa"
          strokeWidth={2}
          strokeDasharray="5,5"
          style={{ pointerEvents: "none" }}
        />
      )}

      {/* Render nodes */}
      {[...nodes.entries()].map(([nodeId, node]) => {
        const pos = positions.get(nodeId);
        if (!pos) return null;

        const isSelected = controller.cursor_at_term(node.term);
        const isAlmostSelected = controller.cursor_almost_at_term(node.term);

        let fillColor = "#fff";
        let strokeColor = "#333";
        if (isSelected) {
          fillColor = "#9de5f2";
          strokeColor = "#0088aa";
        } else if (isAlmostSelected) {
          fillColor = "#cbeef4";
          strokeColor = "#66aabb";
        }

        const height = node.isEmbedded ? embeddedMinHeight : nodeHeight;

        return (
          <g key={nodeId}>
            {/* Node rectangle - only for non-embedded nodes */}
            {!node.isEmbedded && (
              <rect
                x={pos.x}
                y={pos.y}
                width={nodeWidth}
                height={height}
                rx={6}
                ry={6}
                fill={fillColor}
                stroke={strokeColor}
                strokeWidth={2}
                style={{ cursor: "grab" }}
                onMouseDown={(e) => handleMouseDown(e, nodeId)}
                onClick={() => handleNodeClick(nodeId)}
              />
            )}

            {/* Node label - only for non-embedded nodes */}
            {!node.isEmbedded && (
              <text
                x={pos.x + nodeWidth / 2}
                y={pos.y + height / 2}
                textAnchor="middle"
                dominantBaseline="middle"
                fontSize={12}
                fontFamily="monospace"
                fill="#333"
                style={{ pointerEvents: "none" }}
              >
                {node.label}
              </text>
            )}

            {/* Embedded content for non-Canvas projectors - renders as normal projector */}
            {node.isEmbedded && node.embeddedLocation && (
              <foreignObject
                x={pos.x}
                y={pos.y}
                width={1}
                height={1}
                style={{ overflow: "visible" }}
              >
                <div
                  style={{
                    display: "inline-block",
                    backgroundColor: fillColor,
                    border: `2px solid ${strokeColor}`,
                    borderRadius: "6px",
                    padding: "8px",
                    minWidth: "200px",
                  }}
                  onMouseDown={(e) => {
                    // Only start drag if clicking on the container itself (padding/border area)
                    if (e.target === e.currentTarget) {
                      handleMouseDown(e as unknown as React.MouseEvent, nodeId);
                    }
                  }}
                  onClick={(e) => {
                    // Only select node if clicking directly on container, not on children
                    if (e.target === e.currentTarget) {
                      handleNodeClick(nodeId);
                    }
                  }}
                >
                  {renderLocation(controller, node.embeddedLocation, rerender)}
                </div>
              </foreignObject>
            )}

            {/* Child slots (circles at bottom) - only for non-embedded nodes */}
            {!node.isEmbedded && node.children.map((childLoc, i) => {
              const slotCount = node.children.length;
              const slotSpacing = nodeWidth / (slotCount + 1);
              const slotX = pos.x + slotSpacing * (i + 1);
              const slotY = pos.y + nodeHeight;

              const childTerms = controller.children_of_location(childLoc);
              const isEmpty = childTerms.length === 0;
              const isSlotSelected = controller.cursor_at_location(childLoc);
              const isWireSource = wireDrag?.fromNodeId === nodeId && wireDrag?.fromSlotIndex === i;

              let slotFill = isEmpty ? "#eee" : "#666";
              if (isSlotSelected) {
                slotFill = "#9de5f2";
              }
              if (isWireSource) {
                slotFill = "#0088aa";
              }

              return (
                <circle
                  key={i}
                  cx={slotX}
                  cy={slotY}
                  r={slotRadius}
                  fill={slotFill}
                  stroke="#333"
                  strokeWidth={1}
                  style={{ cursor: isEmpty ? "pointer" : "grab" }}
                  onMouseDown={(e) => {
                    if (!isEmpty) {
                      handleWireMouseDown(e, nodeId, i, childLoc, slotX, slotY);
                    }
                  }}
                  onClick={(e) => {
                    e.stopPropagation();
                    if (!wireDrag) {
                      handleSlotClick(childLoc);
                    }
                  }}
                />
              );
            })}

            {/* Input port (circle at top) - only for non-embedded nodes */}
            {!node.isEmbedded && (() => {
              const portX = pos.x + nodeWidth / 2;
              const portY = pos.y;
              // Check if this is a valid drop target during wire drag
              const isDropTarget = wireDrag && wireDrag.fromNodeId !== nodeId;
              // Check if mouse is near this port during wire drag
              const isHovered = isDropTarget && wireDrag &&
                Math.sqrt((wireDrag.currentX - portX) ** 2 + (wireDrag.currentY - portY) ** 2) < slotRadius * 2;

              return (
                <circle
                  cx={portX}
                  cy={portY}
                  r={isHovered ? slotRadius * 1.5 : slotRadius}
                  fill={isHovered ? "#0088aa" : isDropTarget ? "#66aabb" : "#999"}
                  stroke={isHovered ? "#005577" : "#333"}
                  strokeWidth={isHovered ? 2 : 1}
                />
              );
            })()}
          </g>
        );
      })}

      {/* Empty state message */}
      {nodes.size === 0 && (
        <text
          x={maxX / 2}
          y={maxY / 2}
          textAnchor="middle"
          dominantBaseline="middle"
          fontSize={16}
          fill="#999"
        >
          Empty canvas. Add nodes using keyboard shortcuts.
        </text>
      )}
    </svg>
  );
}
