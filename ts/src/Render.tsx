
import { WasmState } from "./pkg/rust";


function render_location(controller : WasmState, location : any) : string {
    const ns = controller.children_of_location(location);
    var contents = "";
    if (ns.length == 0) {
        contents = "?"
    } else if (ns.length == 1) {
        contents = render_node(controller, ns[0])
    } else {
        contents = "{" + ns.map(n => render_node(controller, n)).join(" ") + "}"
    }
    if (controller.cursor_at_location(location)) {
        return "👉" + contents + "👈"
    }
    return contents
}

export function render_node(controller : WasmState, t : any) : string {
    // console.log(controller.cursor())
    var contents = "";
    switch (controller.constructor_of_term(t)) {
        case "Root": {
            const [child0] = controller.children_of_term(t);
            contents = render_location(controller, child0);
            break
        }
        case "Zero": {
            contents = "0";
            break
        }
        case "Plus": {
            const [child0, child1] = controller.children_of_term(t);
            contents = "(+ " + render_location(controller, child0) + " " + render_location(controller, child1) + ")";
            break
        }
        default: throw Error("unrecognized constructor code: " + controller.constructor_of_term(t))
    }
    if (controller.cursor_at_term(t)) {
        return "👉" + contents + "👈"
    }
    return contents
}

export function render_root(controller : WasmState) : string {
    return render_location(controller, controller.top_root())
}