
import { Controller } from "./Controller";
import type { TermLocation } from "./Controller";
import { type Constructor, type TermConstructor, type Mark } from  './RustTypes'
import { CanvasProjector } from "./CanvasProjector";

const cursor_color = "rgb(157, 229, 242)";
const almost_cursor_color = "rgb(203, 240, 246)";
const other_cursor_color = "rgb(144, 238, 144)";  // Light green for other users' cursors
const clipboard_color = "rgb(247, 207, 147)";
const dirty_color = "rgb(213, 107, 62)";

// const sort_mark = "rgb(231, 32, 184)";
const type_mark = "rgb(231, 32, 108)";
const sort_mark = type_mark;
const underline_mark = "rgba(231, 32, 108, 0.6)";

var sort_inspector = <>-</>;
var ana_inspector = <>-</>;
var syn_inspector = <>-</>;
var marks_inspector = undefined;
var emit_patches_callback: ((patches: unknown[]) => void) | undefined = undefined;
var apply_action_callback: ((action: any) => void) | undefined = undefined;
var my_cursor_identity: string | undefined = undefined;
const MAX_RENDER_DEPTH = 50;

function hole(color : string | undefined) {
    return <svg
    width="1em"
    height="1em"
    viewBox="0 0 100 100"
    style={{
        display: "inline-block",
        verticalAlign: "middle",
        cursor: "pointer",
    }}>
    <polygon
        points="95,45 72.5,85 27.5,85 5,45 27.5,5 72.5,5"
        fill={color}
        stroke="black"
        strokeWidth="5"
    />
    </svg>
}

function cursor_span(contents : any) {
    return <span style={{ backgroundColor: cursor_color, color: "black"}}>{contents}</span>
}
function almost_cursor_span(contents : any) {
    return <span style={{ backgroundColor: almost_cursor_color, color: "black"}}>{contents}</span>
}
function clipboard_span(contents : any) {
    return <span style={{ backgroundColor: clipboard_color, color: "black"}}>{contents}</span>
}
function dirty_span(contents : any) {
    return <span style={{ backgroundColor: dirty_color, color: "white"}}>{contents}</span>
}
function other_cursor_span(contents : any) {
    return <span style={{ backgroundColor: other_cursor_color, color: "black"}}>{contents}</span>
}

function mark_span(contents : any, marks : Mark[] | undefined) {
    if (marks == undefined) { return contents }
    if(marks.length > 0) {
        return <span style={{ borderBottom: "2px solid " + underline_mark }}>{contents}</span>
    } else {
        return contents
    }
}

function render_size_of_term(a : number | undefined): string {
    if(a == null) {
        return "-"
    } else {
        return "" + a
    }
}

function render_mark(controller : Controller, mark : Mark) {
    if ("SortInconsistent" in mark) {
        const [s1, s2] = mark.SortInconsistent;
        return <span style={{ color: ""+sort_mark }}>Expected {s1.toLowerCase()}, found {s2.toLowerCase()}.</span>
    } else if ("TypeInconsistent" in mark) {
        const [t1, t2] = mark.TypeInconsistent;
        return <span style={{ color: ""+type_mark }}>Expected {render_opt_type_location(controller, t1)}, found {render_opt_type_location(controller, t2)}.</span>
    } {
        return <>impossible</>
    }
}

function render_marks(controller : Controller, marks : Mark[] | undefined) {
    if (marks == undefined) { return <span></span> }
    return <>{marks.map((mark, i) => <span key={i}>{render_mark(controller, mark)}</span>)}</>
}

// Get a display name for a constructor (used in collapsed view)
function constructor_display_name(c : Constructor): string {
    if (typeof c === "string") {
        return c;
    } else if ("Identifier" in c) {
        return c.Identifier;
    }
    return "?";
}

// Render a term in collapsed form: shows constructor name with "..." for children
function render_collapsed_term(controller : Controller, t : any, rerender : Function): any {
    const tc = controller.constructor_of_term(t);
    if ("Constructor" in tc) {
        const gc = tc.Constructor;
        if (gc === "Root") {
            return <span style={{ color: "gray", fontStyle: "italic" }}>Root(...)</span>;
        } else if ("Lang" in gc) {
            const c = gc.Lang;
            const name = constructor_display_name(c);
            const children = controller.children_of_term(t);
            if (children.length === 0) {
                return <span style={{ color: "gray", fontStyle: "italic" }}>{name}</span>;
            } else {
                return <span style={{ color: "gray", fontStyle: "italic" }}>{name}(...)</span>;
            }
        }
    } else if ("Reference" in tc) {
        return <span style={{ color: "gray", fontStyle: "italic" }}>🌀</span>;
    }
    return <span style={{ color: "gray", fontStyle: "italic" }}>?</span>;
}

// Render a location in collapsed form
function render_collapsed_location(controller : Controller, location : any, rerender : Function): any {
    if (location == "Unknown") { return <span style={{ color: "gray", fontStyle: "italic" }}>-</span> }
    const ns = controller.children_of_location(location);
    if (ns.length == 0) {
        return <span style={{ color: "gray", fontStyle: "italic" }}>⬡</span>;
    } else if (ns.length == 1) {
        return render_collapsed_term(controller, ns[0], rerender);
    } else {
        return <span style={{ color: "gray", fontStyle: "italic" }}>{"{...}"}</span>;
    }
}

function render_lang_term(clickable : Function, c : Constructor, render_children : () => any[]) {
    var contents = <></>
    if (typeof c === "string") {
        switch (c) {
            case "Typ": {
                contents = clickable("□")
                break
            }
            case "Num": {
                contents = clickable("ℕ")
                break
            }
            case "Zero": {
                contents = clickable(0);
                break
            }
            case "Plus": {
                const [child0, child1] = render_children();
                contents = <span>({child0}{" "}{clickable(<>+</>)}{" "}{child1})</span>;
                break
            }
            case "Prod": {
                const [child0, child1] = render_children();
                contents = <span>({child0}{" "}{clickable(<>×</>)}{" "}{child1})</span>;
                break
            }
            case "Pair": {
                const [child0, child1] = render_children();
                contents = <span>{clickable(<>(</>)}{child0}{", "}{child1}{clickable(<>)</>)}</span>;
                break
            }
            case "Arrow": {
                const [child0, child1] = render_children();
                contents = <span>({child0}{" "}{clickable(<>→</>)}{" "}{child1})</span>;
                break
            }
            case "Fun": {
                const [child0, child1] = render_children();
                contents = (
                    <span>
                        ({clickable(<>fun</>)}{" "}
                        {child0}{" "}
                        {clickable(<>↦</>)}{" "}
                        {child1})
                    </span>
                );
                break
            }
            case "Ap": {
                const [child0, child1] = render_children();
                contents = <span>({child0}{" "}{clickable(<>◁</>)}{" "}{child1})</span>;
                break
            }
            case "Asc": {
                const [child0, child1] = render_children();
                contents = <span>({child0}{" "}{clickable(<>:</>)}{" "}{child1})</span>;
                break
            }
            case "Let": {
                const [child0, child1, child2] = render_children();
                contents = (
                    <span>
                        {clickable(<>let</>)}{" "}
                        {child0}{" "}
                        {clickable(<>=</>)}{" "}
                        {child1}{" "}
                        {clickable(<>in</>)}<br />{" "}
                        {child2}
                    </span>
                );
                break
            }
            // Projector types (nullary labels)
            case "Structural": {
                contents = clickable(<span style={{ color: "#666", fontSize: "0.8em" }}>📐</span>);
                break
            }
            case "Collapsed": {
                contents = clickable(<span style={{ color: "#666", fontSize: "0.8em" }}>📦</span>);
                break
            }
            case "Labeled": {
                // Labeled has 1 child (the label)
                const [child0] = render_children();
                contents = <span>{clickable(<span style={{ color: "#666", fontSize: "0.8em" }}>🏷️</span>)}{child0}</span>;
                break
            }
            // Proj is handled specially in render_node, but add fallback here
            case "Proj": {
                const [child0, child1] = render_children();
                contents = <span>{clickable(<>⟨</>)}{child0}{" "}{child1}{clickable(<>⟩</>)}</span>;
                break
            }
            default: {
                contents = <>{JSON.stringify(c)}</>;
                contents = clickable(contents);
            }
        }
    } else if ("Identifier" in c) {
        const x = c.Identifier;
        contents = clickable(x);
    }
    return contents
}

function render_opt_type_location(controller  : Controller, type_location : any) {
    if (type_location == undefined) { return <span>-</span> }
    const ns = controller.children_of_type_location(type_location);
    var contents = <span></span>;
    if (ns.length == 0) {
        contents = <span>?</span>;
    } else if (ns.length == 1) {
        contents = render_type(controller, ns[0])
    } else {
        contents = (
            <span>
                {"{"}
                {ns.map((n, i) => (
                    <span key={i}>{render_type(controller, n)}{i < ns.length - 1 && " "}</span>
                ))}
                {"}"}
            </span>
        );
    }
    return contents
}

function render_type(controller : Controller, t : any) {
    var contents = <span>-</span>;
    const tc : TermConstructor = controller.constructor_of_type(t);
    if ("Constructor" in tc) {
        const gc = tc.Constructor;
        if (gc === "Root") {
            throw Error("impossible Root in type")
        } else if ("Lang" in gc) {
            const c = gc.Lang; 
            const clickable = (element: any) => element;
            const render_children = () => {
                const children = controller.children_of_type(t);
                return children.map(child => render_opt_type_location(controller, child));
            };
            contents = render_lang_term(clickable, c, render_children);
        }
    } else if ("Reference" in tc) {
        contents = <>🌀</>;
    }
    return contents
}

function render_hole(controller : Controller, color : string | undefined, location : any, rerender : Function) {
    const handleClick = () => {
        if (apply_action_callback) {
            apply_action_callback({ MoveToLocation: location });
        } else {
            controller.move_to_location(location);
        }
        rerender();
    };
    return <span onClick={handleClick}>{hole(color)}</span>;
}

// Public render_location — delegates to depth-tracked version
function render_location(controller: Controller, location: any, rerender: Function) {
    return render_location_d(controller, location, rerender, 0);
}

function clickable_node(controller : Controller, t : any, rerender : Function, contents : any) {
    const handleClick = () => {
        if (apply_action_callback) {
            apply_action_callback({ MoveToTerm: t });
        } else {
            controller.move_to_term(t);
        }
        rerender();
    };
    return <span onClick={handleClick}>{contents}</span>;
}

function reference(controller : Controller, r : any, rerender : Function, contents : any) {
    return <span onClick={() => { controller.apply_serial_action({BlossomAction: {ForestAction : {OpenReference: r}}}); rerender()} }>{contents}</span>;
}

// Get the constructor string from position 0 of a Proj node
function get_proj_type(controller: Controller, projTypeLocation: any): string | null {
    const children = controller.children_of_location(projTypeLocation);
    if (children.length !== 1) return null;
    const tc = controller.constructor_of_term(children[0]);
    if (!("Constructor" in tc)) return null;
    const gc = tc.Constructor;
    if (gc === "Root" || !("Lang" in gc)) return null;
    const c = gc.Lang;
    return typeof c === "string" ? c : null;
}

// Get the label text from a Labeled projector's label location
function get_label_text(controller: Controller, labelLocation: any): string {
    const children = controller.children_of_location(labelLocation);
    if (children.length !== 1) return "?";
    const labelTerm = children[0];
    const tc = controller.constructor_of_term(labelTerm);
    if ("Constructor" in tc) {
        const gc = tc.Constructor;
        if (gc !== "Root" && "Lang" in gc) {
            const c = gc.Lang;
            if (typeof c === "object" && "Identifier" in c) {
                return c.Identifier;
            }
        }
    }
    return "?";
}
// Click handler to edit the label of a Labeled projector
// Uses MoveToLocation to bypass cursor navigation restrictions
function edit_label(controller: Controller, labelLocation: any, rerender: Function) {
    // Move cursor directly to the label location, then to the label term if present
    const labelChildren = controller.children_of_location(labelLocation);
    if (labelChildren.length > 0 && apply_action_callback) {
        apply_action_callback({ MoveToTerm: labelChildren[0] });
    } else if (apply_action_callback) {
        apply_action_callback({ MoveToLocation: labelLocation });
    } else {
        // Fallback
        controller.apply_serial_action({ MoveToLocation: labelLocation });
        if (labelChildren.length > 0) {
            controller.move_to_term(labelChildren[0]);
        }
    }
    rerender();
}

// Render a Proj node — dispatch on position 0's constructor
function render_proj(controller: Controller, t: any, rerender: Function, depth: number): any {
    if (depth > MAX_RENDER_DEPTH) return <span>{"..."}</span>;
    if (!("Node" in t)) return <span>{"<invalid>"}</span>;

    const projNode = t.Node;
    const typeLocation = { node: projNode, position: 0 };
    const contentLocation = { node: projNode, position: 1 };
    const projType = get_proj_type(controller, typeLocation);

    switch (projType) {
        case "Structural":
            return <span>
                <span style={{color: "#999", fontSize: "0.7em"}}>📐</span>
                {render_location_d(controller, contentLocation, rerender, depth + 1)}
            </span>;

        case "Collapsed":
            return <span>📦{render_collapsed_location(controller, contentLocation, rerender)}</span>;

        case "Labeled": {
            const typeChildren = controller.children_of_location(typeLocation);
            if (typeChildren.length === 1 && "Node" in typeChildren[0]) {
                const labelLocation = { node: typeChildren[0].Node, position: 0 };
                const labelText = get_label_text(controller, labelLocation);
                const labelClick = () => edit_label(controller, labelLocation, rerender);
                return (
                    <span>
                        <span style={{fontSize: "0.8em"}}>🏷️</span>
                        <span onClick={labelClick} style={{
                            cursor: "pointer", backgroundColor: "#e0e7ff",
                            padding: "0 4px", borderRadius: "3px",
                            fontSize: "0.85em", marginRight: "4px"
                        }}>{labelText}</span>
                        {render_location_d(controller, contentLocation, rerender, depth + 1)}
                    </span>
                );
            }
            return render_location_d(controller, contentLocation, rerender, depth + 1);
        }

        case "Canvas": {
            const emitPatches = emit_patches_callback ?? (() => {});
            const applyAction = apply_action_callback ?? ((a: any) => { controller.apply_serial_action(a); });
            // Pass a depth-aware render function so nested rendering tracks depth correctly
            const renderLocationFn = (c: Controller, l: any, r: () => void) =>
                render_location_d(c, l, r, depth + 1);
            return (
                <div style={{ display: "inline-block", verticalAlign: "top" }}>
                    <div style={{ marginBottom: "4px" }}>
                        <span style={{fontSize: "0.8em"}}>🎨</span>
                        <span style={{ fontSize: "0.7em", color: "#666", marginLeft: "4px" }}>Canvas View</span>
                    </div>
                    <CanvasProjector
                        controller={controller}
                        contentLocation={contentLocation as TermLocation}
                        rerender={rerender as () => void}
                        renderLocation={renderLocationFn}
                        emitPatches={emitPatches}
                        applyAction={applyAction}
                    />
                </div>
            );
        }

        default:
            // No type set or unknown — render content transparently
            return render_location_d(controller, contentLocation, rerender, depth + 1);
    }
}

// Get cursor identity from a Cursor node
function get_cursor_identity(controller: Controller, cursorTerm: any): string | null {
    // Cursor has: position 0 = identity, position 1 = content
    // Construct identity location directly (don't rely on children_of_term array indices)
    if (!("Node" in cursorTerm)) return null;
    const cursorNode = cursorTerm.Node;
    const identityLocation = { node: cursorNode, position: 0 };
    const identityTerms = controller.children_of_location(identityLocation);
    if (identityTerms.length !== 1) return null;
    const identityTerm = identityTerms[0];
    const tc = controller.constructor_of_term(identityTerm);
    if ("Constructor" in tc) {
        const gc = tc.Constructor;
        if (gc !== "Root" && "Lang" in gc) {
            const c = gc.Lang;
            if (typeof c === "object" && "Identifier" in c) {
                return c.Identifier;
            }
        }
    }
    return null;
}

// Render a Cursor node - transparent wrapper that shows cursor highlighting
function render_cursor(controller: Controller, cursorTerm: any, rerender: Function, depth: number = 0): any {
    // Guard against infinite recursion (e.g., nested cursors)
    if (depth > MAX_RENDER_DEPTH) {
        console.warn("render_cursor: max depth exceeded");
        return <span>{"..."}</span>;
    }

    // Cursor has: position 0 = identity, position 1 = content
    // Construct content location directly (don't rely on children_of_term array indices)
    if (!('Node' in cursorTerm)) {
        return <span>{"<invalid cursor>"}</span>;
    }
    const cursorNode = cursorTerm.Node;
    const contentLocation = { node: cursorNode, position: 1 };
    const cursorIdentity = get_cursor_identity(controller, cursorTerm);
    const isMyCursor = cursorIdentity === my_cursor_identity;

    // Render the content (position 1)
    const contentTerms = controller.children_of_location(contentLocation);

    if (contentTerms.length === 0) {
        // Cursor on empty location (hole) - show highlighted hole
        const color = isMyCursor ? cursor_color : other_cursor_color;
        return render_hole(controller, color, contentLocation, rerender);
    } else if (contentTerms.length === 1) {
        // Cursor wrapping a term
        const content = render_node_d(controller, contentTerms[0], rerender, depth + 1);
        if (isMyCursor) {
            // For my cursor: render content normally, let render_node handle
            // highlighting the specific term via cursor_at_term/cursor_at_location
            return content;
        } else {
            // For other cursors: wrap with green highlight to show their selection
            return other_cursor_span(content);
        }
    } else {
        // Multiple terms in cursor content (shouldn't happen normally)
        const contents = contentTerms.map((ct: any, i: number) =>
            <span key={i}>{render_node_d(controller, ct, rerender, depth + 1)}{i < contentTerms.length - 1 && " "}</span>
        );
        const wrapper = <span>{"{"}{contents}{"}"}</span>;
        return isMyCursor ? wrapper : other_cursor_span(wrapper);
    }
}

// Internal render_node with depth tracking
function render_node_d(controller: Controller, t: any, rerender: Function, depth: number): any {
    // Guard against infinite recursion
    if (depth > MAX_RENDER_DEPTH) {
        console.warn("render_node: max depth exceeded");
        return <span>{"..."}</span>;
    }

    var contents = <span></span>;
    const tc : TermConstructor = controller.constructor_of_term(t);
    if ("Constructor" in tc) {
        const gc = tc.Constructor;
        if (gc === "Root") {
            const [child0] = controller.children_of_term(t);
            contents = render_location_d(controller, child0, rerender, depth + 1);
            contents = clickable_node(controller, t, rerender, contents);
        } else if ("Lang" in gc) {
            const c = gc.Lang;
            // Special handling for Proj nodes
            if (c === "Proj") {
                contents = render_proj(controller, t, rerender, depth + 1);
            } else if (c === "Cursor") {
                // Cursor is transparent - render its content with highlighting
                return render_cursor(controller, t, rerender, depth + 1);
            } else {
                const clickable = (element: any) => clickable_node(controller, t, rerender, element);
                const render_children = () => {
                    const children = controller.children_of_term(t);
                    return children.map(child => render_location_d(controller, child, rerender, depth + 1));
                };
                contents = render_lang_term(clickable, c, render_children);
            }
        }
    } else if ("Reference" in tc) {
        contents = reference(controller, tc.Reference, rerender, "🌀");
    }
    contents = mark_span(contents, controller.marks_of_term(t));
    if (controller.cursor_at_term(t)) {
        sort_inspector = controller.sort_of_term(t);
        ana_inspector = render_opt_type_location(controller, controller.ana_of_term(t));
        syn_inspector = render_opt_type_location(controller, controller.syn_of_term(t));
        marks_inspector = render_marks(controller, controller.marks_of_term(t));
        // inspector = render_size_of_term(controller.size_of_term(t));
        return cursor_span(contents)
    } else if(controller.cursor_almost_at_term(t)) {
        return almost_cursor_span(contents)
    } else if(controller.clipboard_at_term(t)) {
        return clipboard_span(contents)
    } else if(controller.is_dirty_term(t)) {
        return dirty_span(contents)
    }
    return contents
}

// Unified render_location with depth tracking and full highlighting
function render_location_d(controller: Controller, tl: any, rerender: Function, depth: number): any {
    if (depth > MAX_RENDER_DEPTH) return <span>{"..."}</span>;
    if (tl == "Unknown") return <span>-</span>;

    const ns = controller.children_of_location(tl);
    let contents;

    if (ns.length === 0) {
        // Hole — pick color from cursor/clipboard/dirty state
        let color: string | undefined = "none";
        if (controller.cursor_at_location(tl)) {
            color = cursor_color;
            ana_inspector = render_opt_type_location(controller, controller.ana_of_location(tl));
            syn_inspector = render_opt_type_location(controller, controller.syn_of_location(tl));
        } else if (controller.cursor_almost_at_location(tl)) {
            color = almost_cursor_color;
        } else if (controller.clipboard_at_location(tl)) {
            color = clipboard_color;
        } else if (controller.is_dirty_location(tl)) {
            color = dirty_color;
        }
        return render_hole(controller, color, tl, rerender);
    } else if (ns.length === 1) {
        contents = render_node_d(controller, ns[0], rerender, depth + 1);
    } else {
        contents = <span>{"{"}{ns.map((n: any, i: number) =>
            <span key={i}>{render_node_d(controller, n, rerender, depth + 1)}{i < ns.length - 1 && " "}</span>
        )}{"}"}</span>;
    }

    // Location-level highlighting for non-holes
    if (controller.cursor_at_location(tl)) {
        ana_inspector = render_opt_type_location(controller, controller.ana_of_location(tl));
        syn_inspector = render_opt_type_location(controller, controller.syn_of_location(tl));
        return cursor_span(contents);
    } else if (controller.cursor_almost_at_location(tl)) {
        return almost_cursor_span(contents);
    } else if (controller.clipboard_at_location(tl)) {
        return clipboard_span(contents);
    } else if (controller.is_dirty_location(tl)) {
        return dirty_span(contents);
    }
    return contents;
}

export function render_node(controller : Controller, t : any, rerender : Function) {
    return render_node_d(controller, t, rerender, 0);
}

// Update inspectors for a given term (used by Canvas projector)
export function update_inspectors_for_term(controller: Controller, t: any) {
    sort_inspector = controller.sort_of_term(t);
    ana_inspector = render_opt_type_location(controller, controller.ana_of_term(t));
    syn_inspector = render_opt_type_location(controller, controller.syn_of_term(t));
    marks_inspector = render_marks(controller, controller.marks_of_term(t));
}

// Update inspectors for a given location (used by Canvas projector)
export function update_inspectors_for_location(controller: Controller, location: any) {
    ana_inspector = render_opt_type_location(controller, controller.ana_of_location(location));
    syn_inspector = render_opt_type_location(controller, controller.syn_of_location(location));
}

export function render_root(controller : Controller, rerender : Function, emitPatches?: (patches: unknown[]) => void, applyAction?: (action: any) => void) {
    sort_inspector = <>-</>;
    ana_inspector = <>-</>;
    syn_inspector = <>-</>;
    marks_inspector = undefined;
    my_cursor_identity = controller.getCursorIdentity();
    emit_patches_callback = emitPatches;
    apply_action_callback = applyAction;
    const contents = render_location(controller, controller.root_location(), rerender);

    // If inspectors weren't set during normal rendering (e.g., cursor is inside a canvas),
    // set them now based on cursor position
    const cursorTerm = controller.get_term_at_cursor();
    if (cursorTerm) {
        sort_inspector = controller.sort_of_term(cursorTerm);
        ana_inspector = render_opt_type_location(controller, controller.ana_of_term(cursorTerm));
        syn_inspector = render_opt_type_location(controller, controller.syn_of_term(cursorTerm));
        marks_inspector = render_marks(controller, controller.marks_of_term(cursorTerm));
    } else {
        // Cursor might be at a location (hole)
        const cursorLocation = controller.get_location_at_cursor();
        if (cursorLocation) {
            ana_inspector = render_opt_type_location(controller, controller.ana_of_location(cursorLocation));
            syn_inspector = render_opt_type_location(controller, controller.syn_of_location(cursorLocation));
        }
    }

    return [<span style={{ cursor: "default", userSelect: "none" }}>{contents}</span>, sort_inspector, ana_inspector, syn_inspector, marks_inspector]
}