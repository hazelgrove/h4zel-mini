all: test build

deps:
	cd ts && npm install

test:
	cd rust && cargo test

build:
	cd rust && wasm-pack build --target web
	cd ts && npm run dev