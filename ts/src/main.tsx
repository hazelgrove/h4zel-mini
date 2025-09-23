import { StrictMode } from "react";
import { createRoot } from "react-dom/client";
import "./index.css";
import App from "./App.tsx";
import {
  DocHandle,
  ImmutableString,
  IndexedDBStorageAdapter,
  isValidAutomergeUrl,
  Repo,
  RepoContext,
  WebSocketClientAdapter,
} from "@automerge/react";
// import { initial_client_state } from "./grove.ts";
// import { groveToAutomerge } from "./Automerge";

const repo = new Repo({
  storage: new IndexedDBStorageAdapter(),
  network: [new WebSocketClientAdapter("wss://sync.automerge.org")],
});

// @ts-expect-error for debugging
window.repo = repo;

let handle: DocHandle<{ grovePatches: Record<string, ImmutableString> }>;
// Check the URL for a document to load
const locationHash = document.location.hash.substring(1);
// Depending if we have an AutomergeUrl, either find or create the document
if (isValidAutomergeUrl(locationHash)) {
  handle = await repo.find(locationHash);
} else {
  // Initialize the document with the genesis patch
  handle = repo.create({ grovePatches: {} });
  // const { patches } = initial_client_state();
  // groveToAutomerge(patches, handle);
  // Set the location hash to the new document we just made.
  document.location.hash = handle.url;
}
// @ts-expect-error for debugging
window.handle = handle;

createRoot(document.getElementById("root")!).render(
  <RepoContext.Provider value={repo}>
    <StrictMode>
      <App handle={handle} />
    </StrictMode>
  </RepoContext.Provider>,
);