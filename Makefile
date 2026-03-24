.PHONY: all wasm install dev build test clean

all: wasm install build

# Build Rust to WASM
wasm:
	cd rust && wasm-pack build --target web

# Install TypeScript dependencies
install:
	cd ts && npm install

# Start dev server (builds WASM first if needed)
dev: wasm
	cd ts && npm run dev

# Production build
build: wasm
	cd ts && npm run build

# Run all tests
test: test-rust test-ts

test-rust:
	cd rust && cargo test

test-ts:
	cd ts && npm test

# Type check only
check:
	cd rust && cargo check
	cd ts && npx tsc --noEmit

# Clean build artifacts
clean:
	cd rust && cargo clean
	rm -rf rust/pkg
	rm -rf ts/dist
	rm -rf ts/node_modules
