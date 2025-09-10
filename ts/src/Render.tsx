
import { WasmState } from "./pkg/rust";

type location = {node : string, position : number};

function render_nodelist(controller : WasmState, ns : Array<string>, location : location) : string {
    var contents = "";
    if (ns.length == 0) {
        contents = "?"
    } else if (ns.length == 1) {
        contents = render_node(controller, ns[0])
    } else {
        contents = "{" + ns.map(n => render_node(controller, n)).join(" ") + "}"
    }
    const cursor = controller.cursor();
    if (cursor.kind == "Location" && cursor.value.node === location.node && cursor.value.position === location.position) {
        return "👉" + contents + "👈"
    }
    return contents
}

export function render_node(controller : WasmState, n : string) : string {
    // console.log(controller.cursor())
    var contents = "";
    switch (controller.constructor_of_node(n)) {
        case "Root": {
            const [child] = controller.children(n);
            contents = render_nodelist(controller, child, {node : n, position : 0});
            break
        }
        case "Zero": {
            contents = "0";
            break
        }
        case "Plus": {
            const [child1, child2] = controller.children(n);
            contents = "(+ " + render_nodelist(controller, child1, {node : n, position : 0}) + " " + render_nodelist(controller, child2, {node : n, position : 2}) + ")";
            break
        }
        default: throw Error("unrecognized constructor code: " + controller.constructor_of_node(n))
    }
    const cursor = controller.cursor();
    if (cursor.kind == "Node" && cursor.value === n) {
        return "👉" + contents + "👈"
    }
    return contents
}

export function render_root(controller : WasmState) : string {
    return render_node(controller, controller.root())
}