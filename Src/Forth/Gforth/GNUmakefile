BINDIR=$(HOME)/local/bin
CP=cp
MYGFORTHS=gforth-tkb-0.7.3

all: build $(foreach f,$(MYGFORTHS),build/$(f))

build:
	mkdir build

build/gforth-tkb-0.7.3: tkb.fs
	gforthmi-0.7.3 build/gforth-tkb-0.7.3 tkb.fs


install: all $(foreach f,$(MYGFORTHS),$(BINDIR)/$(f))

$(BINDIR)/% : build/%
	[ -d $(BINDIR) ] || (mkdir -p $(BINDIR) && echo built $(BINDIR))
	$(CP) $< $@

clean:
	-rm -v build/*
