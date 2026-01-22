import { defineConfig } from 'vitest/config'
import wasm from "vite-plugin-wasm";
import topLevelAwait from "vite-plugin-top-level-await";
import path from 'path';

export default defineConfig({
  plugins: [
    wasm(),
    topLevelAwait()
  ],
  resolve: {
    alias: {
      './pkg': path.resolve(__dirname, '../rust/pkg'),
    }
  },
  test: {
    environment: 'node',
    include: ['src/**/*.test.ts'],
  },
})
