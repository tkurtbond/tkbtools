OCAMLC=ocamlfind ocamlopt
LINKFLAGS=-linkpkg -package labltk

countdown: LINKFLAGS+=-package str

PROGRAMS=\
	animgif animxbm countdown demo getsel startbrowser

all: $(PROGRAMS)

%: %.ml
	$(OCAMLC) -o $@ $(LINKFLAGS) $<

clean:
	-rm -v *.o *.cmi *.cmx *.cmo $(PROGRAMS)
