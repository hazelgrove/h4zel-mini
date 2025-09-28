
import { WasmState } from "./pkg/rust";
import { type TermConstructor } from  './RustTypes'

const cursor_color = "rgb(72, 176, 194)";
const almost_cursor_color = "rgb(189, 233, 240)";
const clipboard_color = "rgb(213, 152, 62)";
const dirty_color = "rgb(213, 107, 62)";

var inspector = "-";
var cursor_found = true;

function cursor_span(contents : any) {
    return <span style={{ backgroundColor: cursor_color, color: "white"}}>{contents}</span>
}
function almost_cursor_span(contents : any) {
    return <span style={{ backgroundColor: almost_cursor_color, color: "black"}}>{contents}</span>
}
function clipboard_span(contents : any) {
    return <span style={{ backgroundColor: clipboard_color, color: "white"}}>{contents}</span>
}
function dirty_span(contents : any) {
    return <span style={{ backgroundColor: dirty_color, color: "white"}}>{contents}</span>
}

function render_size_of_term(a : number | undefined): string {
    if(a == null) {
        return "-"
    } else {
        return "" + a
    }
}

function render_location(controller : WasmState, location : any, rerender : Function) {
    const ns = controller.children_of_location(location);
    var contents = <span></span>;
    if (ns.length == 0) {
        contents = <span onClick={() => { controller.move_to_location(location); rerender()} }>?</span>;
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
        inspector = render_size_of_term(controller.size_of_location(location));
        return cursor_span(contents)
    } else if(controller.cursor_almost_at_location(location)) {
        return almost_cursor_span(contents)
    } else if(controller.clipboard_at_location(location)) {
        return clipboard_span(contents)
    } else if(controller.is_dirty_location(location)) {
        return dirty_span(contents)
    }
    return contents
}

function clickable_node(controller : WasmState, t : any, rerender : Function, contents : any) {
    return <span onClick={() => { controller.move_to_term(t); rerender()} }>{contents}</span>;
}

function reference(controller : WasmState, r : any, rerender : Function, contents : any) {
    return <span onClick={() => { controller.apply_serial_action({BlossomAction: {ForestAction : {OpenReference: r}}}); rerender()} }>{contents}</span>;
}

export function render_node(controller : WasmState, t : any, rerender : Function) {
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
            if (typeof c === "string") {
                switch (gc.Lang) {
                    case "Zero": {
                        contents = clickable_node(controller, t, rerender, 0);
                        break
                    }
                    case "Plus": {
                        const [child0, child1] = controller.children_of_term(t);
                        contents = (
                            <span>
                                ({render_location(controller, child0, rerender)}
                                {" "}{clickable_node(controller, t, rerender, <>+</>)}{" "}
                                {render_location(controller, child1, rerender)})
                            </span>
                        );
                        break
                    }
                    case "Pair": {
                        const [child0, child1] = controller.children_of_term(t);
                        contents = (
                            <span>
                                {clickable_node(controller, t, rerender, <>(</>)}
                                {render_location(controller, child0, rerender)}{", "}
                                {render_location(controller, child1, rerender)}
                                {clickable_node(controller, t, rerender, <>)</>)}
                            </span>
                        );
                        break
                    }
                    case "Fun": {
                        const [child0, child1] = controller.children_of_term(t);
                        contents = (
                            <span>
                                ({clickable_node(controller, t, rerender, <>fun</>)}{" "}
                                {render_location(controller, child0, rerender)}{" "}
                                {clickable_node(controller, t, rerender, <>→</>)}{" "}
                                {render_location(controller, child1, rerender)})
                            </span>
                        );
                        break
                    }
                    case "Ap": {
                        const [child0, child1] = controller.children_of_term(t);
                        contents = (
                            <span>
                                ({render_location(controller, child0, rerender)}
                                {" "}{clickable_node(controller, t, rerender, <>◁</>)}{" "}
                                {render_location(controller, child1, rerender)})
                            </span>
                        );
                        break
                    }
                    case "Let": {
                        const [child0, child1, child2] = controller.children_of_term(t);
                        contents = (
                            <span>
                                {clickable_node(controller, t, rerender, <>let</>)}{" "}
                                {render_location(controller, child0, rerender)}{" "}
                                {clickable_node(controller, t, rerender, <>=</>)}{" "}
                                {render_location(controller, child1, rerender)}{" "}
                                {clickable_node(controller, t, rerender, <>in</>)}<br />{" "}
                                {render_location(controller, child2, rerender)}
                            </span>
                        );
                        break
                    }
                    default: {
                        contents = <>{JSON.stringify(c)}</>;
                        contents = clickable_node(controller, t, rerender, contents);
                    }
                }
            } else if ("Identifier" in c) {
                const x = c.Identifier;
                contents = clickable_node(controller, t, rerender, x);
            }
        }
    } else if ("Reference" in tc) {
        contents = reference(controller, tc.Reference, rerender, "🌀");
    }
    if (controller.cursor_at_term(t)) {
        cursor_found = true;
        inspector = render_size_of_term(controller.size_of_term(t));
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

export function render_root(controller : WasmState, rerender : Function, scream : any) {
    const cursor_previously_found = cursor_found;
    cursor_found = false;
    const contents = render_location(controller, controller.root_location(), rerender);
    if(cursor_previously_found && !cursor_found) { scream.play() }
    // return <span style={{ cursor: "default", userSelect: "none" }}>{contents}</span>
    return [<span style={{ cursor: "default", userSelect: "none" }}>{contents}</span>, <span>{inspector}</span>]
}