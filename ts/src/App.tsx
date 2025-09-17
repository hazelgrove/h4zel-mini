import { useEffect, useState } from "react";
import './App.css'
import { render_root } from  './Render'

import init, { WasmState } from "./pkg/rust";

await init();

function App() {

  const [controller, _setController] = useState(new WasmState());
  const [, forceUpdate] = useState(0);

  function rerender() {
    forceUpdate(x => 1 - x);
  }

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
      };

      const action = keyMap[event.key];
      if (action === undefined) return;

      controller.apply_action(action);
      rerender();

      event.preventDefault();
      // console.log(action);
      // console.log(controller.children(controller.root()));
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
          {render_root(controller, rerender)}
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
