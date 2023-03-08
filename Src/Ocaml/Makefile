##############################################################################
#### Makefile for tools that generate or work with Ocaml source.
##############################################################################
### $Id: Makefile 1.6 Mon, 02 Apr 2001 21:49:08 -0400 tkb $
###
##############################################################################

include .what-sys.dat

# Override this on the command line with WNT if needed.
#OPSYS=unix
ifeq ($(OPSYS),UNIX)
EXE=
OBJ=.o
else
EXE=.exe
OBJ=.o
endif

CP=cp
RM=rm

CAMLC=ocamlc
CAMLCOMP=ocamlc -c

COMPFLAGS=$(XFLAGS)
LINKFLAGS=unix.cma -cclib -lunix str.cma -cclib -lstr

.PHONY: clean realclean

EXES=binmod$(EXE) ocamlize$(EXE) xpmtoocaml$(EXE)

ALL_EXES=$(EXES)

all: $(EXES)


binmod$(EXE): binmod.cmo
	$(CAMLC) -o $@ binmod.cmo


ocamlize$(EXE): ocamlize.cmo
	$(CAMLC) -o $@ ocamlize.cmo

xpmtoocaml$(EXE): xpmtoocaml.ml
	$(CAMLC) -custom -o $@ unix.cma str.cma $<



.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmo

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLCOMP) $(COMPFLAGS) $<


BINDIR=~/local/bin

$(BINDIR)/% : %
	$(CP) $< $@


install: $(foreach e,$(EXES),$(BINDIR)/$(e))

clean:
	-$(RM) *.cmo *.cmi *$(OBJ)

realclean: clean
	-$(RM) $(EXES) *~ .what-sys.dat

ignores: 
	mkignores $(ALL_EXES) >.ignores


.what-sys.dat: 
	what-sys

#### End of Makefile
