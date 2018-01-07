test:
	ocamlbuild -clean
	ocamlbuild -use-ocamlfind test.byte && ./test.byte

repl:
	ocamlbuild -use-ocamlfind repl.byte && ./repl.byte

clean:
	ocamlbuild -clean
	rm -f a4src.zip
