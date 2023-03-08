# Makefile for MS Windows 95/98/NT related programs.
# 
# This Makefile and these programs are built using Cygwin or Ming32.

ifneq ($(MAKECMDGOALS),clean)
include .what-sys.dat
endif



CC=gcc
CFLAGS=-g

CP=cp
RM=rm

OCAMLC=ocamlc
# Set OCAMLDEBUG on the command line or in the environment to get debugging.
OCAMLCFLAGS=$(OCAMLDEBUG)
LIBSTR=-cclib -lstr
LIBUNIX=-cclib -lunix

## The things that differ from Unix to Windows NT.
OPSYS=unix
ifeq ($(OPSYS),UNIX)
EXE=
OBJ=.o
else
EXE=.exe
# Warning: using gcc!
OBJ=.o
endif

ifeq ($(OPVAR),CYGWIN)
BINDIR=$(HOME)/local/cygwin/bin
endif
ifeq ($(OPVAR),MINGW32)
BINDIR=$(HOME)/local/mingw32/bin
endif
ifeq ($(OPVAR),UNIX)
BINDIR=$(HOME)/local/unix/bin
error what do you think you are doing?
endif


$(BINDIR)/% : %
	$(CP) $< $@

%.cmo: %.ml
	$(OCAMLC) $(OCAMLCFLAGS) -c $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLCFLAGS) -c $<

PROGRAMS=shell2$(EXE) woeerr$(EXE)
ADA_PROGRAMS=s1$(EXE) s1-1$(EXE) s1-2$(EXE)
ALL_PROGRAMS=$(PROGRAMS) 
ifdef ADA
ALL_PROGRAMS+=$(ADA_PROGRAMS)
endif

all: $(ALL_PROGRAMS)
	echo $(MAKECMDGOALS)

shell2$(EXE): shell2$(OBJ)
	$(CC) $(CFLAGS) -o $@ $<

woeerr$(EXE): woeerr$(OBJ)
	$(CC) $(CFLAGS) -o $@ $<

enumwindows$(EXE): enumwindows$(OBJ)
	$(CC) $(CFLAGS) -o $@ $<

enumwindows2$(EXE): enumwindows2$(OBJ)
	$(CC) $(CFLAGS) -o $@ $<

s1$(EXE): s1.adb
	gnatmake $<

s1-1$(EXE): s1-1.adb
	gnatmake $<

s1-2$(EXE): s1-2.adb
	gnatmake $<

install: $(foreach e,$(PROGRAMS),$(BINDIR)/$(e))


.PHONY: clean realclean

clean:
	-rm -f $(PROGRAMS) $(ADA_PROGRAMS)

realclean: clean
	-rm -f *.$(OBJ) *.cmi *.cmo .what-sys.dat *.o *.ali *~

.what-sys.dat:
	what-sys >$@
