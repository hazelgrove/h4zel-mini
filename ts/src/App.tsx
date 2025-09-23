import { useEffect, useState, useRef } from "react";
import { ImmutableString, DocHandle } from "@automerge/react";

import './App.css'
import { render_root } from  './Render'
import init, { WasmState } from "./pkg/rust";
import {
  amPatchToGrovePatch,
  grovePatchesFromDocHandle,
  type GroveDoc,
} from "./Automerge";

await init();

function App({ handle }: { handle: DocHandle<GroveDoc> }) {

  const controller = useRef(new WasmState());
  const [, forceUpdate] = useState(0);

  function apply_patches(patches : any[]) {
    for(const patch of patches) {
      // console.log("applying patch", patch);
      controller.current.apply_patch(patch);
    }
  }

  const initial_patches = grovePatchesFromDocHandle(handle);
  // console.log("init patches");
  apply_patches(initial_patches);

  function rerender() {
    forceUpdate(x => 1 - x);
  }

  handle.on("change", ({ patches }) => {
    // Something changed in the automerge document. Convert the incoming patches
    // to grove patches and apply them to the state, then update the rendered
    // state. Note that this will happen twice for local changes, once in
    // applyAction, and then once again here. That's fine, all events are
    // idempotent
    if (patches.length == 0) return;
    for (const amPatch of patches) {
      const patch = amPatchToGrovePatch(amPatch);
      if (patch != null) {
        console.log("document patch", patch);
        controller.current.apply_patch(patch);
      }
    }
    rerender();
  });

  useEffect(() => {
    function handleKeyDown(event: KeyboardEvent) {
      const keyMap: Record<string, string> = {
        Backspace: "delete",
        "0": "insert_zero",
        "+": "wrap_left_plus",
        "*": "wrap_left_times",
        ArrowUp: "move_up",
        ArrowDown: "move_down",
        ArrowRight: "move_right",
        x: "cut",
        v: "paste",
        u: "update",
      };

      const action = keyMap[event.key];
      if (action === undefined) return;

      const action_patches = controller.current.apply_action(action);
      apply_patches(action_patches);
      rerender();

      event.preventDefault();
    }

    window.addEventListener("keydown", handleKeyDown);
    return () => window.removeEventListener("keydown", handleKeyDown);
  });

  return (
    <>
      <div style={{
        width: "600px",
        maxWidth: "100%",
        overflowX: "auto",
        borderWidth: "1px",
        borderColor: "gray",
        borderStyle: "solid",
        padding: "20px"
      }}>
        <p>
          {render_root(controller.current, rerender)}
        </p>
      </div>
      <div>
        <p style={{ fontSize: "8pt", textAlign: "left" }}>
          click or arrow keys: move cursor<br />
          delete/backspace: delete<br />
          0: insert zero<br />
          +: wrap plus<br />
          x: cut<br />
          v: paste<br />
        </p>
      </div>
    </>
  )
}

export default App
