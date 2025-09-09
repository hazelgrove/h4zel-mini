
import { WasmState } from "./pkg/rust";

export function render_node(s : WasmState, n : string) : string {
    return "hi"
}

export function render_root(s : WasmState) : string {
    return render_node(s, s.root())
}