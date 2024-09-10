CP=cp
BINDIR=$(HOME)/local/bin

ifndef ICONT
ICONT=unicont
endif

PROGRAMS=betags ll

all: $(PROGRAMS)

install: $(foreach e,$(PROGRAMS),$(BINDIR)/$(e)) 

# We don't need this if unicon is instaled properly with the IPL.
# betags: options.u1

%: %.icn
	$(ICONT) -o $@ $<

%.u1 %.u2 : %.icn
	$(ICONT) -c $<

%.u1 %.u2: %.icn


clean: 
	-rm -v $(PROGRAMS) *.u1 *.u2

$(BINDIR)/% : %
	[ -d $(BINDIR) ] || (mkdir -p $(BINDIR) && echo built $(BINDIR))
	$(CP) $< $@
