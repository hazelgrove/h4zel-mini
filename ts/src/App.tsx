import { useCallback, useEffect, useRef, useState } from "react";
import { ImmutableString, DocHandle } from "@automerge/react";

import './App.css'
import { render_root } from  './Render'
import { type Action } from  './RustTypes'
import init, { WasmState } from "./pkg/rust";
import {
  amPatchToGrovePatch,
  grovePatchesFromDocHandle,
  id_of_patch,
  type GroveDoc,
  type AmPatch,
} from "./Automerge";

import scream from './assets/scream.mp3';

await init();

function App({ handle }: { handle: DocHandle<GroveDoc> }) {

  const controller = useRef(new WasmState());
  const [_forced, forceUpdate] = useState(0);
  const autoUpdate = useRef(true);
  const autoSync = useRef(true);
  const automergeOutqueue : {current: any[]} = useRef([]);
  const automergeInqueue : {current: any[]} = useRef([]);

  // console.log(autoUpdate);

  function emit_patches(d : GroveDoc, patches : any[]) {
    for (const patch of patches) {
      // console.log(patch);
      const patchId = id_of_patch(patch);
      d.grovePatches[patchId] = new ImmutableString(JSON.stringify(patch));
    }
  }

  function apply_grove_patches(patches : any[]) {
    for(const patch of patches) {
      // console.log("applying patch", patch);
      controller.current.apply_patch(patch);
    }
  }

  function apply_am_patches(patches : AmPatch[]) {
    for (const amPatch of patches) {
      const patch = amPatchToGrovePatch(amPatch);
      if (patch != null) {
        controller.current.apply_patch(patch);
      }
    }
  }

  function handle_emitted_patches(patches : any[]) {
    if (autoSync.current) {
      handle.change((d) => { emit_patches(d, patches) });
    } else {
      automergeOutqueue.current.push(...patches);
    }
  }

  function handle_incoming_patches(patches: any[]) {
    if (autoSync.current) {
      apply_am_patches(patches);
      rerender();
    } else {
      automergeInqueue.current.push(...patches);
    }
  }

  function resync() {
    handle.change((d) => {
      emit_patches(d, automergeOutqueue.current);
      automergeOutqueue.current = []
      apply_am_patches(automergeInqueue.current);
      automergeInqueue.current = []
    });
  }

  function apply_action(action : Action) : any[] {
    return controller.current.apply_serial_action(action)
  }

  const initial_patches = grovePatchesFromDocHandle(handle);
  // console.log("init patches");
  apply_grove_patches(initial_patches);

  if(autoUpdate.current) {
    apply_action({BlossomAction : "AllUpdateSteps"});
  }

  function rerender() {
    forceUpdate(x => x + 1);
  }

  handle.on("change", ({ patches }) => {
    // Something changed in the automerge document. Convert the incoming patches
    // to grove patches and apply them to the state, then update the rendered
    // state. Note that this will happen twice for local changes, once in
    // applyAction, and then once again here. That's fine, all events are
    // idempotent
    if (patches.length == 0) return;
    handle_incoming_patches(patches);
  });

  // useCallback so that we can declare applyAction as a dependency of the
  // useEffect hook for the keydown event without causing an infinite loop
  const applyAction = useCallback(
    (action: Action) => {
      // Whenever we apply an action, update the state ref, then add any new patches
      // to the Automerge document. Then update the rendered state
      const patches = apply_action(action);
      handle_emitted_patches(patches);
    },
    [handle, controller],
  );

  useEffect(() => {
    function handleKeyDown(event: KeyboardEvent) {

      const keyMap: Record<string, Action> = {
        Backspace: "Delete",
        "0": {Insert: "Zero"},
        "+": {WrapLeft: "Plus"},
        ",": {WrapLeft: "Pair"},
        " ": {WrapLeft: "Ap"},
        ArrowUp: {Move: "Up"},
        ArrowDown: {Move: "Down"},
        ArrowRight: {Move: "Right"},
      };

      // List of actions that require Control
      const ctrlActions: Record<string, Action> = {
        x: "Cut",
        v: "Paste",
        f: {WrapLeft: "Fun"},
        l: {WrapLeft: "Let"},
        u: {BlossomAction: "UpdateStep"},
      };

      let action: Action | undefined;

      if (event.ctrlKey && ctrlActions[event.key]) {
        action = ctrlActions[event.key];
      } 
      else if (/^[a-zA-Z]$/.test(event.key)) {
        action = {TextInsert: `${event.key}`};
      } 
      else if (keyMap[event.key]) {
        action = keyMap[event.key];
        if (action === "Delete" && event.shiftKey) {
          action = "TextBackspace";
        }
      }

      if (action === undefined) return;

      event.preventDefault();
      applyAction(action);

      if(typeof action === "object" && "WrapLeft" in action){
        applyAction({Move: "Down"});
      }

      rerender();
    }

    window.addEventListener("keydown", handleKeyDown);
    return () => window.removeEventListener("keydown", handleKeyDown);
  });

  var scream_audio = new Audio(scream);
  const [program, ana_inspector, syn_inspector] = render_root(controller.current, rerender, scream_audio);

  return (
    <>
      <div style={{
        width: "600px",
        maxWidth: "100%",
        overflowX: "auto",
        borderWidth: "1px",
        borderColor: "gray",
        borderStyle: "solid",
        padding: "0px"
      }}>
        <div style={{
          padding: "10px",
          overflowX: "auto",
          flex: "1"
        }}>
          <p>{program}</p>
        </div>

        <div style={{
          borderTop: "1px solid gray",
          padding: "4px 8px",
          fontSize: "12px",
          height: "1em",
          lineHeight: "1em",
          overflow: "hidden",
          whiteSpace: "nowrap",
        }}>
          Expected type: {ana_inspector},
          Found type: {syn_inspector}
        </div>
      </div>
      <div>
        <p style={{ fontSize: "8pt", textAlign: "left" }}>
          click or arrow keys: move cursor<br />
          alphabet keys: type identifier<br />
          delete/backspace: delete<br />
          shift+delete/backspace: backspace on identifier<br />
          0: insert zero<br />
          +: wrap plus<br />
          comma: wrap pair<br />
          space: wrap ap<br />
          ctrl+f: wrap fun<br />
          ctrl+l: wrap let<br />
          ctrl+x: cut<br />
          ctrl+v: paste<br />
          ctrl+u: update propagation step (auto <input
            type="checkbox"
            checked={autoUpdate.current}
            onChange={() => {autoUpdate.current = !autoUpdate.current; rerender()}}
            style={{ transform: "scale(0.85)",  marginLeft: "0px", marginRight: "0px", verticalAlign: "-3px" }}
            tabIndex={-1}
            />)
          <br />
        </p>
        <p>automerge sync <input
            type="checkbox"
            checked={autoSync.current}
            onChange={() => {autoSync.current = !autoSync.current; if (autoSync.current) { resync(); }; rerender(); console.log("swithing") }}
            style={{ transform: "scale(0.85)",  marginLeft: "0px", marginRight: "0px", verticalAlign: "-3px" }}
            tabIndex={-1}
            />
          </p>
      </div>
    </>
  )
}

export default App
