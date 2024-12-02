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

$(BINDIR)/% : %
	[ -d $(BINDIR) ] || (mkdir -p $(BINDIR) && echo built $(BINDIR))
	$(CP) $< $@

ifdef VARIANT_BINDIR
$(VARIANT_BINDIR)/% : %
	[ -d $(VARIANT_BINDIR) ] || (mkdir -p $(VARIANT_BINDIR) && echo built $(VARIANT_BINDIR))
	$(CP) $< $@
endif

XFLAGS=

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
	llc$(EXE) \
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

ALL_EXES=$(BUILD_EXES) $(VARIANT_BUILD_EXES)

NOTSRC=words.ml

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

all: $(ALL_EXES)

build:
	mkdir build

test_exes: $(TEST_EXES)

tounits$(EXE): LDLIBS+=-lm

tounits$(EXE): tounits.o
	$(CC) -g -o $@ $< $(LDLIBS)

cygmodpath$(EXE): cygmodpath.$(OBJ)
	$(CXX) $(CXXFLAGS) -o $@ $<


# This is always bytecode.
ocaml_su$(EXE):
	ocamlmktop $(CUSTOM) -o $@ -I +unix unix.cma -I +str str.cma

pad$(EXE): pad.$(OBJ)
	$(CC) -o $@ $<

split$(EXE): split.$(OBJ)
	$(CC) -o $@ $<

spwn$(EXE): spwn.$(OBJ)
	$(CC) -o $@ $<

# truncate is not implemented under windows.
#truncfile$(EXE):  truncfile.$(CMO)
#	$(OCAMLC) $(OCAMLCFLAGS) $(CUSTOM) -o truncfile$(EXE)  \
#		unix.$(CMA) truncfile.$(CMO) $(LIBSTR)

# MingW Sys.getenv doesn't pick up Cygwin environment variables of course,
# so this is variant.

%$(EXE) : %.c
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS)

# %.o : %.c
# 	$(CC) $(CFLAGS) -c -o $@ $<

# %.o : %.cc
# 	$(CC) $(CFLAGS) -c -o $@ $<

%$(EXE): %.ml
	$(OCAMLC) $(OCAMLCFLAGS) $(XFLAGS) -o $@ $<

%.ml: %.mll
	ocamllex -o $@ $<

%.$(CMO): %.ml
	$(OCAMLC) $(OCAMLCFLAGS) $(XFLAGS) -c -o $@ $<

gethostnames$(EXE): XFLAGS+=-I +unix unix.$(CMA)
grepgroup$(EXE): XFLAGS+=-I +str str.$(CMA)
increment$(EXE): XFLAGS+=-I +unix unix.$(CMA)
md5tree$(EXE): XFLAGS+=-I +unix unix.$(CMA)
modpath$(EXE): XFLAGS+=-I +str str.$(CMA)
nicepath$(EXE): XFLAGS+=-I +str str.$(CMA)
timer$(EXE): XFLAGS+=-I +unix unix.$(CMA) -I +str str.$(CMA)
tkbsetup$(EXE): XFLAGS+=-I +str str.$(CMA) -I +unix unix.$(CMA)
truncfile$(EXE): XFLAGS+=-I +unix unix.$(CMA)
unixtime$(EXE): XFLAGS+=-I +unix unix.$(CMA)
wipe$(EXE): XFLAGS+=-I +unix unix.$(CMA)
where$(EXE): XFLAGS+=-I +str str.$(CMA)


words$(EXE):  words.$(CMO)
	$(OCAMLC) $(OCAMLCFLAGS) $(CUSTOM) -o $@ -I +str str.$(CMA) \
		words.$(CMO)

install: install-normal install-variant install-nonmswoe
install-normal: $(foreach e,$(EXES),$(BINDIR)/$(e)) 
install-variant: $(foreach e,$(VARIANT_BUILD_EXES),$(VARIANT_BINDIR)/$(e))
install-nonmswoe: $(foreach e,$(NONMSWOE_BUILD_EXES),$(BINDIR)/$(e))



clean:
	-$(RM) *.cmo $(wildcard *.cmx) *.cmi *.$(OBJ) \
		$(NOTSRC)

variantclean:
	-$(RM) $(VARIANT_EXES) $(VARIANT_EXES:.exe=.o) what-sys$(EXE) .what-sys.dat

tidy:
	-$(RM) camlprim0.ob

realclean: clean tidy
	-$(RM) $(ALL_EXES) $(TEST_EXES) *~ .depend .what-sys.dat what-sys$(EXE)
	-$(RM) *.cmi *.cmo *.cmxa *.cmx

ign.ores: 
	mkignores $(ALL_EXES) >.ignores

.depend: $(filter-out \*.mli,$(wildcard *.mli)) *.ml
	ocamldep $(filter-out \*.mli,$(wildcard *.mli)) *.ml >.depend
include .depend

.what-sys.dat: build what-sys
	./what-sys $@

# Should I add an export target to create a tar file of the current version
# of the project?  With the project version???

print-%: ; @echo $*=$($*)

# End of Makefile
