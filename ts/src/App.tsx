import { useEffect, useState } from "react";
import reactLogo from './assets/react.svg'
import viteLogo from '/vite.svg'
import './App.css'
import { render_root } from  './Render'

import init, { WasmState } from "./pkg/rust";

await init();

function App() {

  const [count, setCount] = useState(0);
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
      <div>
        <a href="https://vite.dev" target="_blank">
          <img src={viteLogo} className="logo" alt="Vite logo" />
        </a>
        <a href="https://react.dev" target="_blank">
          <img src={reactLogo} className="logo react" alt="React logo" />
        </a>
      </div>
      <h1>Vite + React</h1>
      <div className="card">
        <button onClick={() => {setCount((count) => count+1); console.log(controller.root())}}>
          count is {count}
        </button>
        <p>
          Edit <code>src/App.tsx</code> and save to test HMR
        </p>
        <p>
          {rendered}
        </p>
      </div>
      <p className="read-the-docs">
        Click on the Vite and React logos to learn more
      </p>
    </>
  )
}

export default App
