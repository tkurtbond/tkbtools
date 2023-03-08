BINDIR=~/local/MacOSX/bin
SCRIPTS=lockscreen
CP=cp

all: $(SCRIPTS)

install: $(foreach e,$(SCRIPTS) $(EXES),$(BINDIR)/$(e))

$(BINDIR)/% : %
	$(CP) $< $@
