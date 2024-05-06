BINDIR=$(HOME)/local/bin
CP=cp
MYGFORTHS=gforth-tkb

all: build $(foreach f,$(MYGFORTHS),build/$(f))

build:
	mkdir build

build/gforth-tkb: anew.fs
	gforthmi build/gforth-tkb anew.fs


install: $(foreach f,$(MYGFORTHS),$(BINDIR)/$(f))

$(BINDIR)/% : build/%
	[ -d $(BINDIR) ] || (mkdir -p $(BINDIR) && echo built $(BINDIR))
	$(CP) $< $@

clean:
	-rm -v build/*
