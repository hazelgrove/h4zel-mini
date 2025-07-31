import { useState } from 'react'
import automergeLogo from './assets/automerge.png'
import hazelLogo from './assets/hazelnut.png'
import './App.css'
import {apply_action, initial_client_state, string_of_state} from './grove'

function App() {
  const [state, setState] = useState(initial_client_state())

  return (
    <>
      <div>
        <a href="https://automerge.org" target="_blank">
          <img src={automergeLogo} className="logo" alt="Automerge logo" />
        </a>
        <a href="https://hazel.org" target="_blank">
          <img src={hazelLogo} className="logo react" alt="Hazelnut logo" />
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
