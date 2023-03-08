# Makefile for MS Windows 95/98/NT related programs.
# 
# This Makefile and these programs are built using Cygwin.

include .what-sys.dat

CC=gcc
CFLAGS=-g
#CPPFLAGS=-g

CP=cp
RM=rm

OCAMLC=ocamlc
# Set OCAMLDEBUG on the command line or in the environment to get debugging.
OCAMLCFLAGS=$(OCAMLDEBUG)
LIBSTR=-cclib -lstr
LIBUNIX=-cclib -lunix

## The things that differ from Unix to Windows NT.
ifeq ($(OPSYS),UNIX)
EXE=
OBJ=o
else
EXE=.exe
# Warning: using gcc!
OBJ=o
endif

BINDIR=$(HOME)/local/bin
CYGWIN_BINDIR=$(HOME)/local/cygwin/bin
ifeq ($(OPVAR),CYGWIN)
VARIANT_BINDIR=$(CYGWIN_BINDIR)
endif
ifeq ($(OPVAR),MINGW32)
VARIANT_BINDIR=$(HOME)/local/mingw32/bin
endif
ifeq ($(OPVAR),UNIX)
VARIANT_BINDIR=$(HOME)/local/unix/bin
endif
BATDIR=$(HOME)/local/msw32/bin
MSDOS_BINDIR=$(BATDIR)

$(BINDIR)/% : %
	[ -d $(BINDIR) ] || (mkdir -p $(BINDIR) && echo built $(BINDIR))
	$(CP) $< $@

$(VARIANT_BINDIR)/% : %
	[ -d $(VARIANT_BINDIR) ] || (mkdir -p $(VARIANT_BINDIR) && echo built $(VARIANT_BINDIR))
	$(CP) $< $@

$(CYGWIN_BINDIR)/% : %
	[ -d $(CYGWIN_BINDIR) ] || (mkdir -p $(CYGWIN_BINDIR) && echo built $(CYGWIN_BINDIR))
	$(CP) $< $@

$(BATDIR)/% : %
	[ -d $(BATDIR) ] || (mkdir -p $(BATDIR) && echo built $(BATDIR))
	$(CP) $< $@

%.cmo: %.ml
	$(OCAMLC) $(OCAMLCFLAGS) -c $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLCFLAGS) -c $<

PROGRAMS=lowerfilename$(EXE) shortname$(EXE) msenv$(EXE) reslash$(EXE) binstdout$(EXE)
SPECIAL_PROGRAMS=uniplink$(EXE)
SCRIPTS=mapnspcm mapnspunclass mapnspdev sss sqlpath.sh gtk-dev-setup.sh maparchdrives.sh blog2html
BATFILES=svjpedal.bat msw32-setup.bat mymerge.bat psync.cmd tkb-plink-sync.cmd\
	savew32path.cmd whackapig.cmd newdns.cmd olddns.cmd dyndns.cmd

CYGWIN_SCRIPTS=vsvars32.sh vs8vars32.sh

all: $(PROGRAMS) $(SPECIAL_PROGRAMS)

msenv$(EXE): msenv.cmo
	$(OCAMLC) $(OCAMLCFLAGS) -o $@ $<

lowerfilename$(EXE): lowerfilename.$(OBJ)
	$(CC) -o $@ $<

binstdout$(EXE): binstdout.$(OBJ)
	$(CC) -o $@ $<

binstdoutexec$(EXE): binstdoutexe.$(OBJ)
	$(CC) -o $@ $<


shortname$(EXE): shortname.cc
	$(CXX) -o $@ $<

reslash$(EXE): reslash.cmo
	$(OCAMLC) $(OCAMLFLAGS) -o $@ $<

uniplink$(EXE): CFLAGS += -mno-cygwin
uniplink$(EXE): uniplink.c
	$(CC) $(CFLAGS) -o $@ $<

whackapig$(EXE): whackapig.o tkbmswoe.a
	$(CC) -o $@ $^

whackapig.c: tkbmswoe.h

tkbmswoe.o: tkbmswoe.h

tkbmswoe.a: tkbmswoe.o
	$(AR) rvs $@ $^

$(BATDIR)/%.cmd : %.py
	cp $< $@

install: $(foreach e,$(PROGRAMS),$(VARIANT_BINDIR)/$(e))
install: $(foreach e,$(SCRIPTS),$(VARIANT_BINDIR)/$(e))
ifeq ($(OPSYS),WNT)
install: $(foreach e,$(BATFILES),$(BATDIR)/$(e))
install: $(foreach e,$(SPECIAL_PROGRAMS),$(MSDOS_BINDIR)/$(e))
install: $(foreach e,$(CYGWIN_SCRIPTS),$(CYGWIN_BINDIR)/$(e))
endif


.PHONY: clean realclean

clean:
	-rm -f $(bin_programs)
	-rm -f *.$(OBJ) *.cmi *.cmo *.a

realclean: clean
	-rm .what-sys.dat

.what-sys.dat:
	what-sys

whatswhat: .what-sys.dat
	-echo opsys: $(OPSYS) opvar: $(OPVAR)
