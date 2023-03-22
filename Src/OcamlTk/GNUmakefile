OCAMLC=ocamlfind ocamlopt
LINKFLAGS=-linkpkg -package labltk
CP=cp

BINDIR=$(HOME)/local/bin

PROGRAMS=\
	animgif animxbm countdown demo getsel startbrowser


all: $(PROGRAMS)

countdown: LINKFLAGS+=-package str

install: $(foreach e,$(PROGRAMS),$(BINDIR)/$(e))

$(BINDIR)/% : %
	[ -d $(BINDIR) ] || (mkdir -p $(BINDIR) && echo built $(BINDIR))
	$(CP) $< $@

%: %.ml
	$(OCAMLC) -o $@ $(LINKFLAGS) $<

clean:
	-rm -v *.o *.cmi *.cmx *.cmo $(PROGRAMS)
