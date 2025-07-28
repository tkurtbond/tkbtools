##############################################################################
#### Makefile -- makefile for tools I got form others.  
##############################################################################

include .what-sys.dat

CP=cp
RM=rm

## The things that differ from Unix to Windows NT.
#OPSYS=unix
ifeq ($(OPSYS),UNIX)
EXE=
OBJ=o
else
EXE=.exe
OBJ=obj
endif

BINDIR=~/local/bin

.PHONEY: clean tidy realclean zipit

$(BINDIR)/% : %
	$(CP) $< $@


# There is a C implementation of ordinalday in ../Src which gets installed.

SCRIPTS= \
	monocle
	# Nothing Here

NOTSRC=

all: $(SCRIPTS)





install: $(foreach e,$(SCRIPTS) $(EXES),$(BINDIR)/$(e))

clean:
	-$(RM) $(NOTSRC)

.what-sys.dat:
	what-sys

# End of Makefile
