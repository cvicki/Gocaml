MODULES=$(addprefix src/, authors game command main gui util)
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
MAIN=main.byte
TEST=test.byte
GUI=gui.byte
PKGS=ounit2,str,yojson,graphics,ANSITerminal
TERMINAL=n
OCAMLBUILD=ocamlbuild -use-ocamlfind

bisect: clean test
	bisect-ppx-report html

build:
	$(OCAMLBUILD) $(OBJECTS)

clean:
	ocamlbuild -clean
	rm -rf gocaml.zip ./docs.public _coverage bisect*.coverage

docs:
	mkdir -p docs.public
	ocamlfind ocamldoc -I _build/src -package $(PKGS) \
		-html -stars -d docs.public $(MLIS)

gocaml:
	if [ "$(TERMINAL)" = "y" ]; \
	  then $(OCAMLBUILD) ./$(MAIN) && ./$(MAIN); \
  else \
	  $(OCAMLBUILD) ./$(GUI) && ./$(GUI); \
  fi

test:
	BISECT_COVERAGE=YES $(OCAMLBUILD) -tag 'debug' ./$(TEST) && ./$(TEST) -runner sequential

zip:
	zip gocaml.zip ./games/* ./src/* ./tests/* ./tests/supporting/* _tags Makefile ./*.md ./.merlin
