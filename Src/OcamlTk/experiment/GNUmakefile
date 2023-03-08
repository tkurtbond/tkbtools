OCAMLC=ocamlfind ocamlopt
LINKFLAGS=-linkpkg -package labltk

PROGRAMS=\
	getfilename

all: $(PROGRAMS)

%: %.ml
	$(OCAMLC) -o $@ $(LINKFLAGS) $<

clean:
	-rm -v *.o *.cmi *.cmx *.cmo $(PROGRAMS)
