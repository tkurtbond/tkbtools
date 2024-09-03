##############################################################################
#### GNUmakefile -- makefile for my tools.  
##############################################################################
### 
### The executables in $(HOME)/local/bin are made from sources in this directory
### and installed in $(HOME)/local/bin by this Makefile.
### 
### Note: If you want to build MingW with Cygwin, use
### 	  make CC='gcc -mno-cygwin'
###
### Note: This Makefile is not (yet?) recursive.  Do the Makefiles
### in the subdirectory by hand.
##############################################################################

OS=$(shell uname)
include .what-sys.dat

# 2020-12-22:
#CUSTOM=-custom

CP=cp
RM=rm

CC=gcc
CXX=c++
CFLAGS=-g
CXXFLAGS=-g

NATIVE=true
ifdef NATIVE
OCAMLC=ocamlopt
CMO=cmx
CMA=cmxa
CUSTOM=
else
OCAMLC=ocamlc
CMO=cmo
CMA=cma
endif
# Set OCAMLDEBUG on the command line or in the environment to get debugging.
OCAMLCFLAGS+=$(OCAMLDEBUG)
LIBSTR=#-cclib -lstr
LIBUNIX=#-cclib -lunix

ifndef ICONT
ICONT=unicont
endif

## The things that differ from Unix to Windows NT.
#OPSYS=unix
ifeq ($(OPSYS),UNIX)
EXE=
OBJ=o
else
EXE=.exe
# Warning: using gcc!
OBJ=o
endif

BINDIR=$(HOME)/local/bin
ifeq ($(OPVAR),CYGWIN)
VARIANT_BINDIR=$(HOME)/local/cygwin/bin
endif
ifeq ($(OPVAR),MINGW32)
VARIANT_BINDIR=$(HOME)/local/mingw32/bin
endif
ifeq ($(OPVAR),UNIX)
VARIANT_BINDIR=$(HOME)/local/unix/bin
endif

.PHONY: clean tidy realclean zipit install install-normal install-variant variantclean

$(BINDIR)/% : build/%
	[ -d $(BINDIR) ] || (mkdir -p $(BINDIR) && echo built $(BINDIR))
	$(CP) $< $@

ifdef VARIANT_BINDIR
$(VARIANT_BINDIR)/% : build/%
	[ -d $(VARIANT_BINDIR) ] || (mkdir -p $(VARIANT_BINDIR) && echo built $(VARIANT_BINDIR))
	$(CP) $< $@
endif

build/%.$(CMO): %.ml
	$(OCAMLC) $(OCAMLCFLAGS) -c -o $@ $<

build/%.$(CMO): build/%.ml
	$(OCAMLC) $(OCAMLCFLAGS) -c -o $@ $<

build/%.cmi: %.mli
	$(OCAMLC) $(OCAMLCFLAGS) -c -o $@ $<

build/%.ml: %.mll
	ocamllex -o $@ $<

build/%$(EXE) : %.c
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS)

build/%.o : %.c
	$(CC) $(CFLAGS) -c -o $@ $<

build/%.o : %.cc
	$(CC) $(CFLAGS) -c -o $@ $<


# 2009-01-27: gack: match-anything-rules restrictions.
build/%$(EXE):: %.ml
	$(OCAMLC) $(OCAMLCFLAGS) -o $@ $<



EXES=	\
	changecase$(EXE) \
	chopext$(EXE) \
	cwipe$(EXE) \
	datetime$(EXE) \
	filldisk$(EXE) \
	fromunits$(EXE) \
	gethostnames$(EXE) \
	grepgroup$(EXE) \
	increment$(EXE) \
	iso8601$(EXE) \
	ll$(EXE) \
	md5tree$(EXE) \
	nicepath$(EXE) \
	nospaces$(EXE) \
	ocaml_su$(EXE) \
	ordinalday$(EXE) \
	printerr$(EXE) \
	randomn$(EXE) \
	randomline$(EXE) \
	snooze$(EXE) \
	timer$(EXE) \
	tkbsetup$(EXE) \
	topath$(EXE) \
	tounits$(EXE) \
	unixtime$(EXE) \
	unod-b$(EXE) \
	wipe$(EXE) \
	words$(EXE)

OBSOLETE_EXES= \
	nxt$(EXE)


# Programs that require a non-MSWoe runtime.
NONMSWOE_EXES=\
	pad$(EXE) \
	re$(EXE) \
	re2$(EXE) \
	split$(EXE) \
	spwn$(EXE) \
	truncfile$(EXE) \
	# end


# Programs that need to have different behaviour under Cygwin and MinGW.
VARIANT_EXES=\
	cygmodpath$(EXE)\
	modpath$(EXE) \
	what-sys$(EXE) \
	where$(EXE)

BUILD_EXES=$(EXES)

ifneq ($(OPVAR),MSVC)
ifneq ($(OPVAR),MINGW32)
BUILD_EXES += $(NONMSWOE_EXES)
endif
endif

#ifeq ($(OPSYS),WNT)
VARIANT_BUILD_EXES += $(VARIANT_EXES)
#endif

ifneq ($(OPVAR),MSVC)
ifneq ($(OPVAR),MINGW32)
NONMSWOE_BUILD_EXES += $(NONMSWOE_EXES)
endif
endif

# TEST_EXES=re$(EXE) re2$(EXE)

ALL_EXES=$(BUILD_EXES:%=build/%) $(VARIANT_BUILD_EXES:%=build/%)

NOTSRC=build/words.ml

#tmp: 
#	@echo OPSYS=$(OPSYS) OPVAR=$(OPVAR) >x.1 QQQ=$(QQQ)
#	@echo NEQMSVC=$(NEQMSVC) NEQMINGW32=$(NEQMINGW32) >x.2
#	@echo MINGSYS=$(MINGSYS) MSVCSYS=$(MSVCSYS) >x.3
#ifeq (X,X)
#	@echo X,X is equal
#endif
#ifneq (X,Y)
#	@echo X,Y is not equal
#endif

all: build $(ALL_EXES)

build:
	mkdir build

test_exes: $(TEST_EXES)

build/% : %.icn
	$(ICONT) -o $@ $<


build/%$(EXE): %.ml
	$(OCAMLC) $(OCAMLCFLAGS) -o $@ $<

build/tounits$(EXE): LDLIBS+=-lm

build/tounits$(EXE): build/tounits.o
	$(CC) -g -o $@ $< $(LDLIBS)

build/changecase$(EXE): build/changecase.$(CMO)
	$(OCAMLC) $(OCAMLCFLAGS) $(CUSTOM) -o $@  $<

build/chopext$(EXE): build/chopext.$(CMO)
	$(OCAMLC) $(OCAMLCFLAGS) $(CUSTOM) -o $@  $<

build/cygmodpath$(EXE): build/cygmodpath.$(OBJ)
	$(CXX) $(CXXFLAGS) -o $@ $<

build/gethostnames$(EXE): build/gethostnames.$(CMO)
	$(OCAMLC) $(OCAMLCFLAGS) $(CUSTOM) -o $@  \
		unix.$(CMA) build/gethostnames.$(CMO) $(LIBUNIX)

build/grepgroup$(EXE): build/grepgroup.$(CMO)
	$(OCAMLC) $(OCAMLCFLAGS) $(CUSTOM) -o $@ str.$(CMA) $<

build/increment$(EXE): build/increment.$(CMO)
	$(OCAMLC) $(OCAMLCFLAGS) $(CUSTOM) -o $@  \
		unix.$(CMA) build/increment.$(CMO) $(LIBUNIX)

build/modpath$(EXE):  build/modpath.$(CMO)
	$(OCAMLC) $(OCAMLCFLAGS) $(CUSTOM) -o $@  \
		str.$(CMA) build/modpath.$(CMO) $(LIBSTR)

build/md5tree$(EXE): build/md5tree.$(CMO)
	$(OCAMLC) $(OCAMLCFLAGS) $(CUSTOM) -o $@  \
		unix.$(CMA) build/md5tree.$(CMO) $(LIBUNIX)

build/nicepath$(EXE):  build/nicepath.$(CMO)
	$(OCAMLC) $(OCAMLCFLAGS) $(CUSTOM) -o $@  \
		str.$(CMA) build/nicepath.$(CMO) $(LIBSTR)

build/nxt$(EXE):  build/nxt.$(CMO)
	$(OCAMLC) $(OCAMLCFLAGS) $(CUSTOM) -o $@ unix.$(CMA) str.$(CMA)  \
		build/nxt.$(CMO) $(LIBUNIX) $(LIBSTR)

build/nospaces$(EXE):  build/nospaces.$(CMO)
	$(OCAMLC) $(OCAMLCFLAGS) $(CUSTOM) -o $@ unix.$(CMA) str.$(CMA)  \
		build/nospaces.$(CMO) $(LIBUNIX) $(LIBSTR)

# This is always bytecode.
build/ocaml_su$(EXE):
	ocamlmktop $(CUSTOM) -o $@ unix.cma str.cma -cclib -lunix

build/pad$(EXE): build/pad.$(OBJ)
	$(CC) -o $@ $<

build/randomn$(EXE): build/randomn.$(CMO)
	$(OCAMLC) $(OCAMLCFLAGS) -o $@ $<

build/randomline$(EXE): build/randomline.$(CMO)
	$(OCAMLC) $(OCAMLCFLAGS) -o $@ $<

build/spawn$(EXE): build/spawn.$(CMO)
	$(OCAMLC) $(OCAMLCFLAGS) -o $@ unix.$(CMA) $<

build/split$(EXE): build/split.$(OBJ)
	$(CC) -o $@ $<

build/spwn$(EXE): build/spwn.$(OBJ)
	$(CC) -o $@ $<

build/timer$(EXE):  build/timer.$(CMO)
	$(OCAMLC) $(OCAMLCFLAGS) $(CUSTOM) -o $@  unix.$(CMA) str.$(CMA) \
		build/timer.$(CMO) $(LIBUNIX) $(LIBSTR)

build/tkbsetup$(EXE): build/tkbsetup.$(CMO)
	$(OCAMLC) $(OCAMLCFLAGS) $(CUSTOM) -o $@ str.$(CMA) unix.$(CMA) $< \
		$(LIBSTR) $(LIBUNIX)

build/topath$(EXE):  build/topath.$(CMO)
	$(OCAMLC) $(OCAMLCFLAGS) $(CUSTOM) -o $@  \
		str.$(CMA) build/topath.$(CMO) $(LIBSTR)

# truncate is not implemented under windows.
#truncfile$(EXE):  truncfile.$(CMO)
#	$(OCAMLC) $(OCAMLCFLAGS) $(CUSTOM) -o truncfile$(EXE)  \
#		unix.$(CMA) truncfile.$(CMO) $(LIBSTR)

build/unixtime$(EXE):  build/unixtime.$(CMO)
	$(OCAMLC) $(OCAMLCFLAGS) $(CUSTOM) -o $@  \
		unix.$(CMA) build/unixtime.$(CMO) $(LIBSTR)

# MingW Sys.getenv doesn't pick up Cygwin environment variables of course,
# so this is variant.
build/where$(EXE):  build/where.$(CMO)
	$(OCAMLC) $(OCAMLCFLAGS) $(CUSTOM) -o $@  \
		str.$(CMA) build/where.$(CMO) $(LIBSTR)

build/wipe$(EXE):  build/wipe.$(CMO)
	$(OCAMLC) $(OCAMLCFLAGS) $(CUSTOM) -o $@  unix.$(CMA) \
		build/wipe.$(CMO) $(LIBUNIX)

build/words$(EXE):  build/words.$(CMO)
	$(OCAMLC) $(OCAMLCFLAGS) $(CUSTOM) -o $@  str.$(CMA) \
		build/words.$(CMO) $(LIBSTR)



install: install-normal install-variant install-nonmswoe
install-normal: $(foreach e,$(EXES),$(BINDIR)/$(e)) 
install-variant: $(foreach e,$(VARIANT_BUILD_EXES),$(VARIANT_BINDIR)/$(e))
install-nonmswoe: $(foreach e,$(NONMSWOE_BUILD_EXES),$(BINDIR)/$(e))



clean:
	-$(RM) build/*.cmo $(wildcard build/*.cmx) build/*.cmi build/*.$(OBJ) \
		$(NOTSRC)

variantclean:
	-$(RM) $(VARIANT_EXES) $(VARIANT_EXES:.exe=.o) build/what-sys$(EXE) .what-sys.dat

tidy:
	-$(RM) camlprim0.ob

realclean: clean tidy
	-$(RM) $(ALL_EXES) $(TEST_EXES) *~ .depend .what-sys.dat build/what-sys$(EXE)
	-$(RM) *.cmi *.cmo *.cmxa *.cmx

ign.ores: 
	mkignores $(ALL_EXES) >.ignores

.depend: $(filter-out \*.mli,$(wildcard *.mli)) *.ml
	ocamldep $(filter-out \*.mli,$(wildcard *.mli)) *.ml >.depend
include .depend

.what-sys.dat: build build/what-sys
	./build/what-sys $@

# Should I add an export target to create a tar file of the current version
# of the project?  With the project version???

print-%: ; @echo $*=$($*)

# End of Makefile
