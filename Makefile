all:
	ocamlbuild -use-menhir -menhir 'menhir --explain --infer' -cflags '-g' -lflags '-g'  minic.native
clean:
	-rm -r _build minic.native
