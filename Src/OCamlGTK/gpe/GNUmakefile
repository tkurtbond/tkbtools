include .what-sys.dat

RM=rm
MV=mv
CP=cp
ifeq ($(OPSYS),UNIX)
EXE=
OBJ=o
else
EXE=.exe
# Warning: using gcc!
OBJ=o
endif

.PHONY: clean realclean install install-variant

BINDIR=~/local/bin
ALL_EXES=gpe$(EXE)
PACKAGES=-package str -package unix -package threads -package lablgtk2

all: $(ALL_EXES)

CAMLC = ocamlfind ocamlc -custom $(XMLFLAGS) $(PACKAGES) -thread
CAMLOPT = ocamlopt $(XMLFLAGS) $(PACKAGES) -thread
COMPILER = $(CAMLC) $(MLFLAGS) -c
LINKER = $(CAMLC) $(MLFLAGS) -linkpkg
COMPOPT = $(CAMLOPT) $(MLFLAGS) -c
LINKOPT = $(CAMLOPT) $(MLFLAGS) -linkpkg

TOPLEVEL = olablmktop $(MLFLAGS)
RANLIB = ranlib

# include $(LABLGTKDIR)/config.make

MLFLAGS = -w s -labels -g 

MYPKGCONFIG=pkg-config
GTKCFLAGS=$(shell ${MYPKGCONFIG} gtk+-2.0 --cflags)
GTKLIBS=$(shell ${MYPKGCONFIG} gtk+-2.0 --libs)

ifdef DEBUG
CFLAGS = -g $(GTKCFLAGS)
LDFLAGS = $(GTKLIBS) -lcamlrund
MLFLAGS += -g
else
CFLAGS = -O -DGTK_NO_CHECK_CASTS -DGTK_DISABLE_COMPAT_H $(GTKCFLAGS)
LDFLAGS = $(GTKLIBS)
endif

ifneq "$(OPSYS)" "UNIX"
CFLAGS += -mno-cygwin -mms-bitfields
endif

ifdef USE_CC
CCOMPILER = $(CC) -c -I$(LABLLIB) $(CFLAGS)
else
CCOMPILER = $(CAMLC) -c -ccopt "$(CFLAGS)"
endif


# Rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx .c .o .var .h .opt .def
.c.o:
	$(CCOMPILER) $<
.ml.cmo:
	$(COMPILER) $<
.mli.cmi:
	$(COMPILER) $<
.ml.cmx:
	$(COMPOPT) $<


# Ocaml always puts the .cmo in the same directery as the .ml.
ifeq ($(OPSYS),UNIX)
SYSOPS_SRC=
PRIM_OBJS=
else
SYSOPS_SRC=mingw_sysops.ml
PRIM_OBJS=mingw_primops.o
endif
SYSOPS_OBJ=$(SYSOPS_SRC:.ml=.cmo)



# gpathedit.cmo: 

gpe$(EXE): ops.cmo $(SYSOPS_OBJ) gpe_icons.cmo gpe.cmo $(PRIM_OBJS) 
	$(LINKER) $(MLFLAGS) -o $@ $^

ops.cmo: ops.cmi

gpe_icons.ml: gpe.xpm gpe-cropped.xpm
	xpmtoocaml $^ >$@

%.mli %.ml: %.mly
	ocamlyacc $<

%.ml: %.mll
	ocamllex $<


$(BINDIR)/% : %
	[ -d $(BINDIR) ] || (mkdir -p $(BINDIR) && echo built $(BINDIR))
	$(CP) $< $@


install: $(foreach e,$(ALL_EXES),$(BINDIR)/$(e))

clean:
	-$(RM) -f *.cm* *.o *.a *_parser.ml *_parser.mli *_lexer.ml
	-$(RM) $(ALL_EXES)
	-$(RM) mingw_sysops.cmi mingw_sysops.cmo
	-$(RM) mingw_primops.o

realclean: clean
	-$(RM) .what-sys.dat .depend

.depend: .what-sys.dat $(SYSOPS_SRC)
	ocamldep *.mli *.ml >.depend

include .depend

.what-sys.dat:
	what-sys

debug: 
	echo opsys: $(OPSYS) opsys_dir: $(OPSYS_DIR)
