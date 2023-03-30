CP=cp
RM=rm
SCRIPTS=git-jump
BINDIR=~/local/bin

install: $(foreach e,$(SCRIPTS),$(BINDIR)/$(e))

$(BINDIR)/% : %
	$(CP) $< $@


