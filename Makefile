all: test build

deps:
	cd ts && npm install

test:
	cd rust && cargo test

build:
	cd rust && wasm-pack build --target web && cp -r pkg ../ts/src
	cd ts && npm run dev