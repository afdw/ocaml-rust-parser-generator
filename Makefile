.PHONY: build example clean

default: build

syn/syn.json:
	git clone https://github.com/dtolnay/syn

build: syn/syn.json
	(cd generator && cargo run)
	(cd rust-lib && cargo build --release)
	cp target/release/libocaml_rust_parser_generator.a ocaml-lib/libocaml_rust_parser_generator.a
	cp target/release/libocaml_rust_parser_generator.so ocaml-lib/dllocaml_rust_parser_generator.so
	(cd ocaml-lib && dune build)

example: build
	(cd example && dune build)
	_build/default/example/example.exe

clean:
	rm -rf syn
	cargo clean
	dune clean
	rm -f rust-lib/src/generated.rs
	rm -f ocaml-lib/ocaml_rust_parser_generator.mli
	rm -f ocaml-lib/ocaml_rust_parser_generator.ml
	rm -f ocaml-lib/dllocaml_rust_parser_generator.so
	rm -f ocaml-lib/libocaml_rust_parser_generator.a
