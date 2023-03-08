EXE=
OBJ=o
CP=cp -v
BINDIR=$(HOME)/local/bin
EXES=	\
	fmtroff$(EXE)

all: build $(EXES:%=build/%)

build:
	mkdir build

$(BINDIR)/% : build/%
	[ -d $(BINDIR) ] || (mkdir -p $(BINDIR) && echo built $(BINDIR))
	$(CP) $< $@
build/%$(EXE) : %.c
	$(CC) $(CFLAGS) -o $@ $< $(LDFLAGS)

install: $(foreach e,$(EXES),$(BINDIR)/$(e)) 

clean:
	-$(RM) build/*.$(OBJ) $(EXES:%=build/%)
