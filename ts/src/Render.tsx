
import { WasmState } from "./pkg/rust";

function render_nodelist(controller : WasmState, ns : Array<string>) : string {
    if (ns.length == 0) {
        return "?"
    } else if (ns.length == 1) {
        return render_node(controller, ns[0])
    } else {
         return "{" + ns.map(n => render_node(controller, n)).join(" ") + "}"
    }
}

export function render_node(controller : WasmState, n : string) : string {
    switch (controller.constructor_of_node(n)) {
        case "Root": {
            const [child] = controller.children(n);
            return render_nodelist(controller, child)
        }
        case "Zero": {
            return "0"
        }
        case "Plus": {
            const [child1, child2] = controller.children(n);
            return "(+ " + render_nodelist(controller, child1) + " " + render_nodelist(controller, child2) + ")"
        }
        default: throw Error("unrecognized constructor code: " + controller.constructor_of_node(n))
    }
}

export function render_root(controller : WasmState) : string {
    return render_node(controller, controller.root())
}