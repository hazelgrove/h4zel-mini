import { useCallback, useEffect, useRef, useState } from "react";
import { ImmutableString, DocHandle } from "@automerge/react";

import './App.css'
import { render_root } from  './Render'
import { type Action } from  './RustTypes'
import init from "./pkg/rust";
import { Controller } from "./Controller";
import {
  amPatchToGrovePatch,
  grovePatchesFromDocHandle,
  id_of_patch,
  type GroveDoc,
  type AmPatch,
} from "./Automerge";

await init();

function App({ handle }: { handle: DocHandle<GroveDoc> }) {

  const controller = useRef(new Controller());
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
      // Auto-propagate type updates if enabled
      if (autoUpdate.current) {
        controller.current.runAllUpdates();
      }
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
    // Auto-propagate type updates if enabled
    if (autoUpdate.current) {
      controller.current.runAllUpdates();
    }
  }

  function apply_action(action : Action) : any[] {
    return controller.current.apply_serial_action(action)
  }

  // Track if we've initialized (persists across renders via ref)
  const initialized = useRef(false);

  if (!initialized.current) {
    initialized.current = true;

    const initial_patches = grovePatchesFromDocHandle(handle);
    apply_grove_patches(initial_patches);

    if(autoUpdate.current) {
      controller.current.runAllUpdates();
    }

    // Create initial cursor in Grove if this is a fresh session
    let existingCursors = controller.current.findAllCursors();
    console.log("Initial findAllCursors:", existingCursors);
    console.log("My cursor identity:", controller.current.getCursorIdentity());
    let myCursor = existingCursors.find(c => c.identity === controller.current.getCursorIdentity());
    console.log("Found myCursor:", myCursor);
    if (!myCursor) {
      console.log("Creating new cursor");
      const cursorPatches = controller.current.getInitialCursorPatches();
      console.log("Cursor patches:", cursorPatches);
      for (const p of cursorPatches) {
        controller.current.apply_patch(p);
      }
      handle_emitted_patches(cursorPatches);
      // Run updates after cursor creation
      if (autoUpdate.current) {
        controller.current.runAllUpdates();
      }
      // Re-find cursors after creation to get our new cursor
      existingCursors = controller.current.findAllCursors();
      console.log("After creation findAllCursors:", existingCursors);
      myCursor = existingCursors.find(c => c.identity === controller.current.getCursorIdentity());
      console.log("After creation myCursor:", myCursor);
    }
    // Initialize stable identity node reference (only traversal happens here, at startup)
    if (myCursor) {
      controller.current.initializeIdentityNode(myCursor);
    } else {
      console.log("WARNING: myCursor is still undefined after creation!");
    }

    // Final update pass after all initialization
    if (autoUpdate.current) {
      controller.current.runAllUpdates();
    }
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
      // Auto-propagate type updates if enabled
      if (autoUpdate.current) {
        controller.current.runAllUpdates();
      }
    },
    [handle, controller],
  );

  useEffect(() => {
    function handleKeyDown(event: KeyboardEvent) {

      const keyMap: Record<string, Action> = {
        Backspace: "Delete",
        "0": {Insert: "Zero"},
        "+": {WrapLeft: "Plus"},
        "*": {WrapLeft: "Prod"},
        ",": {WrapLeft: "Pair"},
        "-": {WrapLeft: "Arrow"},
        " ": {WrapLeft: "Ap"},
        ":": {WrapLeft: "Asc"},
        "[": {WrapLeft: "Proj"},
        ArrowUp: {Move: "Up"},
        ArrowDown: {Move: "Down"},
        ArrowRight: {Move: "Right"},
      };

      // List of actions that require Control
      const ctrlActions: Record<string, Action> = {
        x: "Cut",
        v: "Paste",
        t: {Insert: "Typ"},
        n: {Insert: "Num"},
        p: {WrapLeft: "Prod"},
        f: {WrapLeft: "Fun"},
        l: {WrapLeft: "Let"},
        u: {BlossomAction: "UpdateStep"},
      };

      // List of actions that require Control+Shift
      const ctrlShiftActions: Record<string, Action> = {
        S: {Insert: "Structural"},
        C: {Insert: "Collapsed"},
        G: {Insert: "Canvas"},  // G for Graph view
      };

      let action: Action | undefined;

      if (event.ctrlKey && event.shiftKey && ctrlShiftActions[event.key]) {
        action = ctrlShiftActions[event.key];
      }
      else if (event.ctrlKey && ctrlActions[event.key]) {
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

  const [program, sort_inspector, ana_inspector, syn_inspector, marks_inspector] = render_root(controller.current, rerender, handle_emitted_patches, applyAction);

  // Wrap current selection with a projector of the given type
  // Uses direct location access to bypass cursor navigation restrictions on Proj internals
  function wrapWithProjector(projectorType: "Structural" | "Collapsed" | "Canvas") {
    // WrapRight creates Proj with: position 0 = empty (for type), position 1 = wrapped content
    applyAction({ WrapRight: "Proj" });

    // Try to get the Proj term - first try Edge cursor, then Location cursor
    let projTerm = controller.current.get_term_at_cursor();

    // If cursor is at a Location (hole), check if there's a term in that location
    if (!projTerm) {
      const cursorLoc = controller.current.get_location_at_cursor();
      if (cursorLoc) {
        const children = controller.current.children_of_location(cursorLoc);
        if (children.length === 1) {
          projTerm = children[0];
        }
      }
    }

    if (projTerm) {
      // Verify this is actually a Proj node before modifying
      const tc = controller.current.constructor_of_term(projTerm);
      const isProj = "Constructor" in tc &&
        tc.Constructor !== "Root" &&
        "Lang" in tc.Constructor &&
        tc.Constructor.Lang === "Proj";

      if (isProj && "Node" in projTerm) {
        // Get position 0 (projector type slot) directly (don't rely on children_of_term array indices)
        const projTypeLocation = { node: projTerm.Node, position: 0 };
        applyAction({ MoveToLocation: projTypeLocation });
        applyAction({ Insert: projectorType });
        // Move back to select the Proj node
        controller.current.move_to_term(projTerm);
      }
    }
    rerender();
  }

  // Wrap current selection with a Labeled projector (includes default label)
  // Uses direct location access to bypass cursor navigation restrictions on Proj internals
  function wrapWithLabeled() {
    // WrapRight creates Proj with: position 0 = empty (for type), position 1 = wrapped content
    applyAction({ WrapRight: "Proj" });

    // Try to get the Proj term - first try Edge cursor, then Location cursor
    let projTerm = controller.current.get_term_at_cursor();
    if (!projTerm) {
      const cursorLoc = controller.current.get_location_at_cursor();
      if (cursorLoc) {
        const children = controller.current.children_of_location(cursorLoc);
        if (children.length === 1) {
          projTerm = children[0];
        }
      }
    }

    if (projTerm && "Node" in projTerm) {
      // Get position 0 (projector type slot) directly (don't rely on children_of_term array indices)
      const projTypeLocation = { node: projTerm.Node, position: 0 };
      applyAction({ MoveToLocation: projTypeLocation });
      // Insert Labeled (which has arity 1 for the label)
      applyAction({ Insert: "Labeled" });
      // Get the newly created Labeled term and its label slot
      const labeledTerms = controller.current.children_of_location(projTypeLocation);
      if (labeledTerms.length >= 1 && "Node" in labeledTerms[0]) {
        // Labeled has position 0 = label
        const labelLocation = { node: labeledTerms[0].Node, position: 0 };
        applyAction({ MoveToLocation: labelLocation });
        applyAction({ Insert: { Identifier: "label" } });
      }
      // Move back to select the Proj node
      controller.current.move_to_term(projTerm);
    }
    rerender();
  }


  return (
    <>
      {/* Floating projector buttons */}
      <div style={{
        position: "fixed",
        bottom: "20px",
        right: "20px",
        display: "flex",
        flexDirection: "column",
        gap: "8px",
        zIndex: 1000,
      }}>
        <button
          onClick={() => wrapWithProjector("Structural")}
          style={{
            padding: "8px 12px",
            fontSize: "12px",
            cursor: "pointer",
            border: "1px solid #ccc",
            borderRadius: "4px",
            backgroundColor: "#f5f5f5",
          }}
          title="Wrap selection with Structural projector"
        >
          📐 Structural
        </button>
        <button
          onClick={() => wrapWithProjector("Collapsed")}
          style={{
            padding: "8px 12px",
            fontSize: "12px",
            cursor: "pointer",
            border: "1px solid #ccc",
            borderRadius: "4px",
            backgroundColor: "#f5f5f5",
          }}
          title="Wrap selection with Collapsed projector"
        >
          📦 Collapsed
        </button>
        <button
          onClick={() => wrapWithLabeled()}
          style={{
            padding: "8px 12px",
            fontSize: "12px",
            cursor: "pointer",
            border: "1px solid #ccc",
            borderRadius: "4px",
            backgroundColor: "#f5f5f5",
          }}
          title="Wrap selection with Labeled projector"
        >
          🏷️ Labeled
        </button>
        <button
          onClick={() => wrapWithProjector("Canvas")}
          style={{
            padding: "8px 12px",
            fontSize: "12px",
            cursor: "pointer",
            border: "1px solid #ccc",
            borderRadius: "4px",
            backgroundColor: "#e8f4f8",
          }}
          title="Wrap selection with Canvas projector (visual graph view)"
        >
          🎨 Canvas
        </button>
      </div>

      <div style={{
        width: "900px",
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
          <div>{program}</div>
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
          {sort_inspector}.
          Expected type: {ana_inspector},
          Found type: {syn_inspector}, 
          Marks: {marks_inspector}
        </div>
      </div>
      <div>
        <p style={{ fontSize: "8pt", textAlign: "left" }}>
          click or arrow keys: move cursor<br />
          alphabet keys: type identifier<br />
          delete/backspace: delete<br />
          shift+delete/backspace: backspace on identifier<br />
          ctrl+n: insert num type<br />
          0: insert zero<br />
          +: wrap plus<br />
          */ctrl+p: wrap product<br />
          comma: wrap pair<br />
          ctrl+f: wrap fun<br />
          space: wrap ap<br />
          colon: wrap asc<br />
          ctrl+l: wrap let<br />
          [: wrap proj (projector)<br />
          ctrl+shift+s: insert structural projector<br />
          ctrl+shift+c: insert collapsed projector<br />
          ctrl+shift+g: insert canvas projector (graph view)<br />
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
            onChange={() => {autoSync.current = !autoSync.current; if (autoSync.current) { resync(); }; rerender() }}
            style={{ transform: "scale(0.85)",  marginLeft: "0px", marginRight: "0px", verticalAlign: "-3px" }}
            tabIndex={-1}
            />
          </p>
      </div>
    </>
  )
}

export default App
