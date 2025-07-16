HTML_FILE=$(shell pwd)/_build/default/bin/index.html
TEST_DIR="$(shell pwd)/_build/default/test"


all: fmt build

fmt:
	refmt */*.re --in-place
	refmt */*.rei --in-place

build:
	dune build grove/

url:
	@echo "file://$(HTML_FILE)"

clean:
	dune clean

deps:
	opam install dune reason incr_dom ocaml-lsp-server ppx_deriving bisect_ppx junit_alcotest ptmap ppx_yojson_conv

.PHONY: test

test:
	dune fmt --auto-promote || true
	dune build @grove/fmt @test/fmt --auto-promote grove test --profile dev
	node $(TEST_DIR)/h4zeltest.bc.js

