##############################################################################
#### Makefile -- makefile for my tools.  
##############################################################################
### $Id: Makefile 1.8 Wed, 03 Apr 2002 20:16:32 -0500 tkb $
### 
### The scripts in ~/local/bin are in this directory
### and installed in ~/local/bin by this Makefile.
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

$(BINDIR)/% : NetBSD/%
	$(CP) $< $@

$(BINDIR)/% : Linux/%
	$(CP) $< $@

%.cmo: %.ml
	$(OCAMLC) $(OCAMLCFLAGS) -c $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLCFLAGS) -c $<

%.ml: %.mll
	ocamllex $<


# There is a C implementation of ordinalday in ../Src which gets installed.

SCRIPTS= \
	accumulate \
	bldmyblog \
	blogbot \
	books-per-day \
	check-eggs \
	code2pandoc \
	collapse \
	combine-dicts \
	context-depends \
	copy-to-minimal \
	count-books-read \
	cvs-update \
	darcs-inventory \
	dedos.py \
	deslash \
	devolvepdf \
	domail \
	ebook \
	embed-fonts \
	endsound \
	export-tools \
	fg-backup-campaigns \
	filedate \
	filetime \
	find-rst-footnotes \
	gensite.sh \
	getremotemail.sh \
	gfind-moddate \
	git-check-unison \
	git-main \
	git-needs-checkin \
	git-outofdate \
	git-setmeup \
	git-setup-repos \
	git-state \
	git-undo-last-commit \
	git-uptodate \
	hatch-eggs \
	hero-designer \
	htmlindex.sh \
	hyphen-groff \
	incf \
	incf.py \
	instmyblog \
	ion-clients.sh \
	ion-eth0.sh \
	ion-ppp0.sh \
	kpdrop \
	kpsave \
	latex-depends \
	list-fixed-fonts.sh \
	list-mono-fonts \
	list-imports \
	list-my-macports \
	lnhere \
	logf \
	loop \
	lpbook \
	mac-unquarantine \
	machine-characteristics \
	mfcleanup \
	mkignores \
	mrp \
	music-report \
	mygitrepos \
	mzr \
	nikola-categories \
	nikola-install \
	nikola-roles \
	nikola-tags \
	nisttime \
	nsf \
	nsmail \
	nsremote \
	op \
	osinfo \
	pbook \
	pddigest \
	prefix \
	print_vars.sh \
	printduplex-landscape-longedge \
	printduplex-landscape-shortedge \
	printduplex-portrait-longedge \
	printduplex-portrait-shortedge \
	printlongest \
	pslp \
	pstopcl \
	pybloxrst \
	ratstart \
	rmail \
	rst \
	rst-depends \
	rst-lists-latex \
	rst-names \
	rstfrag \
	rstfrag2html \
	rstfragtxt \
	run-chickadee \
	run-discord \
	run-duckling-run \
	run-spotify \
	scream \
	setup-gerbil \
	signature-size \
	sgpod \
	showtzs \
	sort-backslash-words \
	sort-versions \
	sort-with-commas \
	sort-words \
	spotify-flatpack \
	ssh-xauth.sh \
	ssh2repo \
	sshenv \
	sshgo \
	start-gpodder \
	start-sway \
	strip-leading-hash \
	sum-bp \
	sum-cp \
	sum-sp \
	sumlines \
	tarit \
	test-email.sh \
	tkb-sync \
	tkbput.sh \
	troff-depends \
	uhome \
	unbuilt-programs \
	uncomment-caml \
	uncomment-perl \
	unpack-mail \
	unpack-mail.alt \
	unwrap \
	unwrap-paras \
	usatime \
	uwork \
	winbox-in-wine \
	worldographer \
	worldtime \
	# Nothing Here

HOME_SCRIPTS=\
	dofetchmail \
	ssh-ftp-green \
	start-ppp \
	stop-ppp \
	tryppp \
	# Nothing Here

OLD_SCRIPTS=\
	mmm-hotlist \
	mmm_hotlist \
	webramp \
	# Nothing Here

NETBSD_SCRIPTS=\
	NetBSD/setpkgpath.sh \
	NetBSD/open \
	# Nothing here

LINUX_SCRIPTS=\
	Linux/linux-capabilities \
	# Nothing here

EXES=
TEST_EXES=
NOTSRC=

all: $(SCRIPTS) $(EXES)

test_exes: $(TEST_EXES)





install: $(foreach e,$(SCRIPTS) $(EXES),$(BINDIR)/$(e))
install-home: $(foreach e,$(HOME_SCRIPTS),$(BINDIR)/$(e))
install-old: $(foreach e,$(OLD_SCRIPTS),$(BINDIR)/$(e))
install-netbsd: $(foreach e,$(NETBSD_SCRIPTS),$(BINDIR)/$(notdir $(e)))
install-linux: $(foreach e,$(LINUX_SCRIPTS),$(BINDIR)/$(notdir $(e)))



clean:
	-$(RM) *.cmo *.cmi *.$(OBJ) $(NOTSRC)

tidy:
	-$(RM) camlprim0.$(OBJ)

realclean: clean tidy
	-$(RM) $(EXES) $(TEST_EXES) *~

.what-sys.dat:
	what-sys

# End of Makefile
