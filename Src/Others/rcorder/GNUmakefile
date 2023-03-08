rcorder: hash.o rcorder.o util.o fparseln.o fgetln.o

doc: rcorder.8.cat rcorder.8.txt rcorder.8.ps rcorder.8.pdf


%.8.cat : %.8
	nroff -mandoc $< >$@
%.8.txt : %.8.cat
	col -b <$< >$@
%.8.ps : %.8
	groff -mandoc $< >$@
%.8.pdf : %.8.ps
	ps2pdf $< $@

clean: 
	-rm -f *.o hash.ln rcorder.ln
	-rm -f a.out [Ee]rrs mklog core *.core .gdbinit rcorder
	-rm *.8.*
