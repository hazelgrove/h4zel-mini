.all:
	make build

build:
	cd rust && wasm-pack build --target web && cp -r pkg ../ts/src
	cd ts && npm run dev