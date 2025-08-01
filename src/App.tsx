import { useState, useEffect } from 'react'
import automergeLogo from './assets/automerge.png'
import hazelLogo from './assets/hazelnut.png'
import './App.css'
import {apply_action, initial_client_state, string_of_state} from './grove'
import type {action} from './grove'

function App() {
  const [state, setState] = useState(initial_client_state())

  function handleKeyDown(event: KeyboardEvent) {
    const keyMap: Record<string, action> = {
      Backspace: { kind: "delete" },
      "0": { kind: "insert", value: "zero" },
      "+": { kind: "wrap_left", value: "plus" },
      "*": { kind: "wrap_left", value: "times"},
      ArrowUp:   { kind: "move", value: "up" },
      ArrowDown: { kind: "move", value: "down" },
      ArrowRight:{ kind: "move", value: "right" },
    };
    
    // console.log(event.key)
    const action = keyMap[event.key];
    if (action === undefined) return;

    event.preventDefault();
    const newState = apply_action(state, action);
    setState(newState);
  }

  function keyboard_effect() {
    window.addEventListener("keydown", handleKeyDown)
    return () => window.removeEventListener("keydown", handleKeyDown)
  }

  useEffect(keyboard_effect, [state])


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
        <button onClick={() => {var ls = apply_action(state, {kind: "insert", value : "zero"}); setState(ls)}}>
          Insert 0
        </button>
        <button onClick={() => {var ls = apply_action(state, {kind: "wrap_left", value : "plus"}); setState(ls)}}>
          Wrap +
        </button>
        <button onClick={() => {var ls = apply_action(state, {kind: "wrap_left", value : "times"}); setState(ls)}}>
          Wrap *
        </button>
        <button onClick={() => {var ls = apply_action(state, {kind: "delete"}); setState(ls)}}>
          Delete
        </button>
        <br></br>
        <button onClick={() => {var ls = apply_action(state, {kind: "move", value : "up"}); setState(ls)}}>
          Move Up
        </button>
        <button onClick={() => {var ls = apply_action(state, {kind: "move", value : "down"}); setState(ls)}}>
          Move Down
        </button>
        <button onClick={() => {var ls = apply_action(state, {kind: "move", value : "right"}); setState(ls)}}>
          Move Right
        </button>
        <p>
          Program: {string_of_state(state)}
        </p>
      </div>
      <p className="read-the-docs">
        Click on the logos to learn more
      </p>
    </>
  )
}

export default App
