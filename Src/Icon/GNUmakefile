PROGRAMS=betags

all: $(PROGRAMS)

betags: options.u1

%: %.icn
	icont -o $@ $<

%.u1 %.u2: %.icn


clean: 
	-rm -v $(PROGRAMS) *.u1 *.u2
