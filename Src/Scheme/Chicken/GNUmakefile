SHELL=bash
include .what-sys.dat

STATIC=-static

ifeq ($(OPSYS),UNIX)
EXE=
OBJ=o
else
EXE=.exe
# Warning: using gcc!
OBJ=o
endif

ifndef CSC
CSC=$(shell type -p csc || type -p chicken-csc || echo 'echo "CSC does not exist; "')
endif

BINDIR=$(HOME)/local/bin

INSTALL_PROGRAMS=\
	date-diff directory droller extension \
	get-pipermail-archives json2scm kcf list-chicken-imports \
	longestline parse-eggs-list rst-incl rstsection smodpath snewver \
	strip-extension swipe todeltatime toseconds yaml2scm

PROGRAMS=$(INSTALL_PROGRAMS:%=build/%$(EXE))

all: build $(PROGRAMS)

build:
	mkdir build


build/smodpath$(EXE) \
build/list-chicken-imports$(EXE) \
build/snewver$(EXE): CSCFLAGS+=$(STATIC)

build/%$(EXE) : %.scm
	$(TIME) $(CSC) $(CSCFLAGS) -o $@ $^

$(BINDIR)/% : build/%
	[ -d $(BINDIR) ] || (mkdir -p $(BINDIR) && echo built $(BINDIR))
	cp $< $@


install: build $(foreach e,$(PROGRAMS:%=%$(EXE)),$(BINDIR)/$(notdir $(e)))

clean: 
	-rm build/*.so build/*.link $(PROGRAMS:%=%$(EXE)) build/*.out

realclean: clean
	-rm .what-sys.dat

.what-sys.dat:
	what-sys
