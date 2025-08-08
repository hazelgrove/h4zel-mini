import { useCallback, useEffect, useRef, useState } from "react";
import automergeLogo from "./assets/automerge.png";
import hazelLogo from "./assets/hazelnut.png";
import "./App.css";
import {
  apply_action,
  apply_patch,
  from_patches,
  string_of_state,
} from "./grove";
import type { action } from "./grove";
import { ImmutableString, DocHandle } from "@automerge/react";
import {
  amPatchToGrovePatch,
  grovePatchesFromDocHandle,
  type GroveDoc,
} from "./autogrove";

function App({ handle }: { handle: DocHandle<GroveDoc> }) {
  const initialState = from_patches(grovePatchesFromDocHandle(handle));
  // The state is mutated by the apply_action function so we use useRef rather
  // than useState as useState is designed for immutable values
  const stateRef = useRef(initialState);
  const [renderedState, setRenderedState] = useState(
    string_of_state(initialState),
  );

  // useCallback so that we can declare applyAction as a dependency of the
  // useEffect hook for the keydown event without causing an infinite loop
  const applyAction = useCallback(
    (action: action) => {
      // Whenever we apply an action, update the state ref, then add any new patches
      // to the Automerge document. Then update the rendered state
      const patches = apply_action(stateRef.current, action);

      handle.change((d) => {
        for (const patch of patches) {
          const patchId = `${patch.sign}-${patch.id}`;
          d.grovePatches[patchId] = new ImmutableString(JSON.stringify(patch));
        }
      });
      setRenderedState(string_of_state(stateRef.current));
    },
    [handle, stateRef],
  );

  handle.on("change", ({ patches }) => {
    // Something changed in the automerge document. Convert the incoming patches
    // to grove patches and apply them to the state, then update the rendered
    // state. Note that this will happen twice for local changes, once in
    // applyAction, and then once again here. That's fine, all events are
    // idempotent
    if (patches.length == 0) return;
    for (const amPatch of patches) {
      const grPatch = amPatchToGrovePatch(amPatch);
      if (grPatch != null) {
        apply_patch(stateRef.current.shared_state, grPatch);
      }
    }
    setRenderedState(string_of_state(stateRef.current));
  });

  useEffect(() => {
    function handleKeyDown(event: KeyboardEvent) {
      const keyMap: Record<string, action> = {
        Backspace: { kind: "delete" },
        "0": { kind: "insert", value: "zero" },
        "+": { kind: "wrap_left", value: "plus" },
        "*": { kind: "wrap_left", value: "times" },
        ArrowUp: { kind: "move", value: "up" },
        ArrowDown: { kind: "move", value: "down" },
        ArrowRight: { kind: "move", value: "right" },
        c: { kind: "copy" },
        v: { kind: "paste" },
      };

      // console.log(event.key)
      const action = keyMap[event.key];
      if (action === undefined) return;

      event.preventDefault();
      applyAction(action);
    }

    window.addEventListener("keydown", handleKeyDown);
    return () => window.removeEventListener("keydown", handleKeyDown);
  });

  return (
    <>
      <div>
        <a href="https://automerge.org" target="_blank">
          <img src={automergeLogo} className="logo" alt="Automerge logo" />
        </a>
        <a href="https://hazel.org" target="_blank">
          <img src={hazelLogo} className="logo" alt="Hazelnut logo" />
        </a>
      </div>
      <h1>Automerge + Grove</h1>
      <div className="card">
        <button
          onClick={() => {
            applyAction({ kind: "insert", value: "zero" });
          }}
        >
          Insert 0
        </button>
        <button
          onClick={() => {
            applyAction({ kind: "wrap_left", value: "plus" });
          }}
        >
          Wrap +
        </button>
        <button
          onClick={() => {
            applyAction({ kind: "wrap_left", value: "times" });
          }}
        >
          Wrap *
        </button>
        <button
          onClick={() => {
            applyAction({ kind: "delete" });
          }}
        >
          Delete
        </button>
        <br></br>
        <button
          onClick={() => {
            applyAction({ kind: "move", value: "up" });
          }}
        >
          Move Up
        </button>
        <button
          onClick={() => {
            applyAction({ kind: "move", value: "down" });
          }}
        >
          Move Down
        </button>
        <button
          onClick={() => {
            applyAction({ kind: "move", value: "right" });
          }}
        >
          Move Right
        </button>
        <br></br>
        <button
          onClick={() => {
            applyAction({ kind: "copy" });
          }}
        >
          Copy
        </button>
        <button
          onClick={() => {
            applyAction({ kind: "paste" });
          }}
        >
          Paste
        </button>
        <p>Program: {renderedState}</p>
      </div>
      <p className="read-the-docs">Click on the logos to learn more</p>
    </>
  );
}

export default App;
