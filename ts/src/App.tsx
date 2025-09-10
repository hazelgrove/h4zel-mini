import { useEffect, useState } from "react";
import './App.css'
import { render_root } from  './Render'

import init, { WasmState } from "./pkg/rust";

await init();

function App() {

  const [controller, _setController] = useState(new WasmState());
  const [rendered, setRendered] = useState(render_root(controller));

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
      setRendered(_ => render_root(controller))

      event.preventDefault();
      // console.log(action);
      // console.log(controller.children(controller.root()));
    }

    window.addEventListener("keydown", handleKeyDown);
    return () => window.removeEventListener("keydown", handleKeyDown);
  });

  return (
    <>
      <div className="card">
        <p>
          {rendered}
        </p>
      </div>
    </>
  )
}

export default App
