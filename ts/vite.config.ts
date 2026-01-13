import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'
import wasm from "vite-plugin-wasm";
import topLevelAwait from "vite-plugin-top-level-await";
import path from 'path';

// https://vite.dev/config/
export default defineConfig({
  plugins: [react(),
    wasm(),
    topLevelAwait()
  ],
  resolve: {
    alias: {
      // Reference Rust WASM pkg directly without copying
      './pkg': path.resolve(__dirname, '../rust/pkg'),
    }
  },
  server: {
    fs: {
      // Allow serving files from rust/pkg
      allow: ['..']
    }
  }
  // optimizeDeps: { exclude: ["fsevents"] },
})
