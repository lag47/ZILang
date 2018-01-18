all:
	ocamlbuild -use-ocamlfind repl.byte

docs:
	ocamlbuild -use-ocamlfind repl.byte
	ocamldoc -html -I _build -d doc ast.ml astfactory.mli church_numeral.mli equiv.mli eval.mli main.mli parse.mli prettyprint.mli

test:
	ocamlbuild -clean
	ocamlbuild -use-ocamlfind test.byte && ./test.byte

repl:
	ocamlbuild -use-ocamlfind repl.byte && ./repl.byte



clean:
	ocamlbuild -clean
	rm -f a4src.zip
