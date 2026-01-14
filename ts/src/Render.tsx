
import { Controller } from "./Controller";
import { type Constructor, type TermConstructor, type Mark } from  './RustTypes'

const cursor_color = "rgb(157, 229, 242)";
const almost_cursor_color = "rgb(203, 240, 246)";
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
var cursor_found = true;

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
    return <span onClick={() => { controller.move_to_location(location); rerender()} }>{hole(color)}</span>;
}

function render_location(controller : Controller, location : any, rerender : Function) {
    if (location == "Unknown") { return <span>-</span> }
    const ns = controller.children_of_location(location);
    var contents = <span></span>;
    if (ns.length == 0) {
        contents = render_hole(controller, "none", location, rerender);
    } else if (ns.length == 1) {
        contents = render_node(controller, ns[0], rerender)
    } else {
        contents = (
            <span>
                {"{"}
                {ns.map((n, i) => (
                    <span key={i}>{render_node(controller, n, rerender)}{i < ns.length - 1 && " "}</span>
                ))}
                {"}"}
            </span>
        );
    }
    if (controller.cursor_at_location(location)) {
        cursor_found = true;
        ana_inspector = render_opt_type_location(controller, controller.ana_of_location(location));
        syn_inspector = render_opt_type_location(controller, controller.syn_of_location(location));
        // inspector = render_size_of_term(controller.size_of_location(location));
        if (ns.length == 0) {
            return render_hole(controller, cursor_color, location, rerender);
        } else {
            return cursor_span(contents)
        }
    } else if(controller.cursor_almost_at_location(location)) {
        if (ns.length == 0) {
            return render_hole(controller, almost_cursor_color, location, rerender);
        } else {
            return almost_cursor_span(contents)
        }
    } else if(controller.clipboard_at_location(location)) {
        if (ns.length == 0) {
            return render_hole(controller, clipboard_color, location, rerender);
        } else {
            return clipboard_span(contents)
        }
    } else if(controller.is_dirty_location(location)) {
        if (ns.length == 0) {
            return render_hole(controller, dirty_color, location, rerender);
        } else {
            return dirty_span(contents)
        }
    }
    return contents
}

function clickable_node(controller : Controller, t : any, rerender : Function, contents : any) {
    return <span onClick={() => { controller.move_to_term(t); rerender()} }>{contents}</span>;
}

function reference(controller : Controller, r : any, rerender : Function, contents : any) {
    return <span onClick={() => { controller.apply_serial_action({BlossomAction: {ForestAction : {OpenReference: r}}}); rerender()} }>{contents}</span>;
}

// Projector info: type and additional data (like label location for Labeled)
type ProjectorInfo =
    | { type: "Structural" }
    | { type: "Collapsed" }
    | { type: "Labeled", projectorTerm: any, labelLocation: any }
    | null;

// Get projector info from a Proj node's first child location
function get_projector_info(controller: Controller, projTypeLocation: any): ProjectorInfo {
    const children = controller.children_of_location(projTypeLocation);
    if (children.length !== 1) return null;
    const projTypeNode = children[0];
    const tc = controller.constructor_of_term(projTypeNode);
    if ("Constructor" in tc) {
        const gc = tc.Constructor;
        if (gc !== "Root" && "Lang" in gc) {
            const c = gc.Lang;
            if (c === "Structural") return { type: "Structural" };
            if (c === "Collapsed") return { type: "Collapsed" };
            if (c === "Labeled") {
                // Get the label location (child 0 of the Labeled node)
                const labelChildren = controller.children_of_term(projTypeNode);
                if (labelChildren.length >= 1) {
                    return { type: "Labeled", projectorTerm: projTypeNode, labelLocation: labelChildren[0] };
                }
            }
        }
    }
    return null;
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

// Change projector type by deleting old type and inserting new one
// Uses MoveToLocation to bypass cursor navigation restrictions on Proj internals
function change_projector_type(controller: Controller, projTerm: any, newType: "Structural" | "Collapsed", rerender: Function) {
    // Get the projector type location (position 0 of Proj)
    const children = controller.children_of_term(projTerm);
    const projTypeLocation = children[0];

    // 1. Move directly to the projector type location (bypasses navigation restrictions)
    controller.apply_serial_action({ MoveToLocation: projTypeLocation });
    // 2. Get children at this location to select the type term
    const typeChildren = controller.children_of_location(projTypeLocation);
    if (typeChildren.length > 0) {
        // 3. Move to select the existing type term and delete it
        controller.move_to_term(typeChildren[0]);
        controller.apply_serial_action("Delete");
    }
    // 4. Insert the new type (cursor is now at the empty location)
    controller.apply_serial_action({ Insert: newType });
    // 5. Move back up to select the Proj node
    controller.move_to_term(projTerm);
    rerender();
}

// Click handler to edit the label of a Labeled projector
// Uses MoveToLocation to bypass cursor navigation restrictions
function edit_label(controller: Controller, labelLocation: any, rerender: Function) {
    // Move cursor directly to the label location
    controller.apply_serial_action({ MoveToLocation: labelLocation });
    // Select the label term if present
    const labelChildren = controller.children_of_location(labelLocation);
    if (labelChildren.length > 0) {
        controller.move_to_term(labelChildren[0]);
    }
    rerender();
}

// Render a Proj node, dispatching to the appropriate projector
function render_proj(controller: Controller, t: any, rerender: Function): any {
    const children = controller.children_of_term(t);
    const [projTypeLocation, childLocation] = children;

    const projInfo = get_projector_info(controller, projTypeLocation);

    if (projInfo === null) {
        // No projector type set: render child transparently
        return render_location(controller, childLocation, rerender);
    } else if (projInfo.type === "Structural") {
        // Structural projector: click 📐 to collapse
        const toggleClick = () => change_projector_type(controller, t, "Collapsed", rerender);
        const childRendered = render_location(controller, childLocation, rerender);
        return <span><span onClick={toggleClick} style={{cursor: "pointer", color: "#999", fontSize: "0.7em"}}>📐</span>{childRendered}</span>;
    } else if (projInfo.type === "Collapsed") {
        // Collapsed projector: click 📦 to expand
        const toggleClick = () => change_projector_type(controller, t, "Structural", rerender);
        const childRendered = render_collapsed_location(controller, childLocation, rerender);
        return <span><span onClick={toggleClick} style={{cursor: "pointer"}}>📦</span>{childRendered}</span>;
    } else if (projInfo.type === "Labeled") {
        // Labeled projector: show label badge + content
        // Click 🏷️ to switch to Structural (loses label)
        // Click label text to edit label
        const labelText = get_label_text(controller, projInfo.labelLocation);
        const toggleClick = () => change_projector_type(controller, t, "Structural", rerender);
        const labelClick = () => edit_label(controller, projInfo.labelLocation, rerender);
        const childRendered = render_location(controller, childLocation, rerender);
        return (
            <span>
                <span onClick={toggleClick} style={{cursor: "pointer", fontSize: "0.8em"}}>🏷️</span>
                <span
                    onClick={labelClick}
                    style={{
                        cursor: "pointer",
                        backgroundColor: "#e0e7ff",
                        padding: "0 4px",
                        borderRadius: "3px",
                        fontSize: "0.85em",
                        marginRight: "4px"
                    }}
                >
                    {labelText}
                </span>
                {childRendered}
            </span>
        );
    } else {
        // Unknown projector type: show with brackets
        const clickable = (element: any) => clickable_node(controller, t, rerender, element);
        const projTypeRendered = render_location(controller, projTypeLocation, rerender);
        const childRendered = render_location(controller, childLocation, rerender);
        return <span>{clickable(<>⟨</>)}{projTypeRendered}{" "}{childRendered}{clickable(<>⟩</>)}</span>;
    }
}

export function render_node(controller : Controller, t : any, rerender : Function) {
    var contents = <span></span>;
    const tc : TermConstructor = controller.constructor_of_term(t);
    if ("Constructor" in tc) {
        const gc = tc.Constructor;
        if (gc === "Root") {
            const [child0] = controller.children_of_term(t);
            contents = render_location(controller, child0, rerender);
            contents = clickable_node(controller, t, rerender, contents);
        } else if ("Lang" in gc) {
            const c = gc.Lang;
            // Special handling for Proj nodes
            if (c === "Proj") {
                contents = render_proj(controller, t, rerender);
            } else {
                const clickable = (element: any) => clickable_node(controller, t, rerender, element);
                const render_children = () => {
                    const children = controller.children_of_term(t);
                    return children.map(child => render_location(controller, child, rerender));
                };
                contents = render_lang_term(clickable, c, render_children);
            }
        }
    } else if ("Reference" in tc) {
        contents = reference(controller, tc.Reference, rerender, "🌀");
    }
    contents = mark_span(contents, controller.marks_of_term(t));
    if (controller.cursor_at_term(t)) {
        cursor_found = true;
        // console.log(controller.syn_of_term(t));
        console.log(controller.marks_of_term(t));
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

export function render_root(controller : Controller, rerender : Function, scream : any) {
    sort_inspector = <>-</>;
    ana_inspector = <>-</>;
    syn_inspector = <>-</>;
    marks_inspector = undefined;
    const cursor_previously_found = cursor_found;
    cursor_found = false;
    const contents = render_location(controller, controller.root_location(), rerender);
    if(cursor_previously_found && !cursor_found) { scream.play() }
    // return <span style={{ cursor: "default", userSelect: "none" }}>{contents}</span>
    return [<span style={{ cursor: "default", userSelect: "none" }}>{contents}</span>, sort_inspector, ana_inspector, syn_inspector, marks_inspector]
}