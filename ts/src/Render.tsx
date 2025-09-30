
import { WasmState } from "./pkg/rust";
import { type Constructor, type TermConstructor } from  './RustTypes'

const cursor_color = "rgb(72, 176, 194)";
const almost_cursor_color = "rgb(189, 233, 240)";
const clipboard_color = "rgb(213, 152, 62)";
const dirty_color = "rgb(213, 107, 62)";

var ana_inspector = <>-</>;
var syn_inspector = <>-</>;
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

function render_opt_type_location(controller  : WasmState, type_location : any) {
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

function render_type(controller : WasmState, t : any) {
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

function render_hole(controller : WasmState, color : string | undefined, location : any, rerender : Function) {
    return <span onClick={() => { controller.move_to_location(location); rerender()} }>{hole(color)}</span>;
}

function render_location(controller : WasmState, location : any, rerender : Function) {
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
            const clickable = (element: any) => clickable_node(controller, t, rerender, element);
            const render_children = () => {
                const children = controller.children_of_term(t);
                return children.map(child => render_location(controller, child, rerender));
            };
            contents = render_lang_term(clickable, c, render_children);
        }
    } else if ("Reference" in tc) {
        contents = reference(controller, tc.Reference, rerender, "🌀");
    }
    if (controller.cursor_at_term(t)) {
        cursor_found = true;
        // console.log(controller.syn_of_term(t));
        ana_inspector = render_opt_type_location(controller, controller.ana_of_term(t));
        syn_inspector = render_opt_type_location(controller, controller.syn_of_term(t));
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

export function render_root(controller : WasmState, rerender : Function, scream : any) {
    const cursor_previously_found = cursor_found;
    cursor_found = false;
    const contents = render_location(controller, controller.root_location(), rerender);
    if(cursor_previously_found && !cursor_found) { scream.play() }
    // return <span style={{ cursor: "default", userSelect: "none" }}>{contents}</span>
    return [<span style={{ cursor: "default", userSelect: "none" }}>{contents}</span>, ana_inspector, syn_inspector]
}