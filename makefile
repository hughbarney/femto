#
# makefile
#

CC      = cc
CPP     = cpp
CPPFLAGS += -D_DEFAULT_SOURCE -D_BSD_SOURCE
# Note: lisp still needs assertions
#CPPFLAGS += -DNDEBUG
CFLAGS += -O2 -std=c11 -Wall -pedantic -g
LD      = cc
LDFLAGS =
LIBS    = -lncursesw
CP      = cp
MV      = mv
RM      = rm
MKDIR	= mkdir
PREFIX  = /usr/local
BINDIR  = $(PREFIX)/bin
DATADIR = $(PREFIX)/share
SCRIPTDIR = "$(DATADIR)/femto"
INITFILE = "$(SCRIPTDIR)/femto.rc"

OBJ     = command.o display.o complete.o data.o gap.o key.o search.o buffer.o replace.o window.o undo.o funcmap.o utils.o hilite.o femto_lisp.o main.o

FLISP_OBJ = flisp.o lisp.o
BINARIES = femto flisp
RC_FILES = femto.rc flisp.rc

LISPFILES = femto.rc lisp/defmacro.lsp lisp/bufmenu.lsp lisp/dired.lsp lisp/grep.lsp lisp/git.lsp lisp/oxo.lsp \
	lisp/flisp.lsp lisp/femto.lsp lisp/info.lsp

all: femto docs/flisp.md

femto: $(OBJ) femto.rc
	$(LD) $(LDFLAGS) -o femto $(OBJ) $(LIBS)

flisp: $(FLISP_OBJ) flisp.rc
	$(LD) $(LDFLAGS) -o $@ $(FLISP_OBJ)

complete.o: complete.c header.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c complete.c

command.o: command.c header.h lisp.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c command.c

data.o: data.c header.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c data.c

display.o: display.c header.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c display.c

gap.o: gap.c header.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c gap.c

key.o: key.c header.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c key.c

search.o: search.c header.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c search.c

replace.o: replace.c header.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c replace.c

window.o: window.c header.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c window.c

buffer.o: buffer.c header.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c buffer.c

undo.o: undo.c header.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c undo.c

femto_lisp.o: lisp.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -D FLISP_FEMTO_EXTENSION -c lisp.c -o $@

funcmap.o: funcmap.c header.h lisp.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c funcmap.c

utils.o: utils.c header.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c utils.c

hilite.o: hilite.c header.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c hilite.c

lisp.o: lisp.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -D FLISP_FILE_EXTENSION -c lisp.c

main.o: main.c header.h lisp.h
	$(CC) $(CPPFLAGS) $(CFLAGS) \
	  -D E_SCRIPTDIR=$(SCRIPTDIR) \
	  -D E_INITFILE=$(INITFILE) \
	  -c main.c

flisp.o: flisp.c lisp.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $<

.SUFFIXES: .rc .sht
.sht.rc:
	./sht $*.sht >$@

femto.rc: femto.sht lisp/core.lsp

flisp.rc: flisp.sht lisp/core.lsp

measure: strip FORCE
	@echo Total
	@echo binsize: $$(set -- $$(ls -l femto); echo $$5)
	@echo linecount: $$(cat *.c *.h *.rc lisp/*.lsp | wc -l)
	@echo sloccount: $$(set -- $$(which sloccount >/dev/null && { sloccount *.c *.h *.rc lisp/*.lsp | grep ansic=; }); echo $$3)
	@echo files: $$(ls *.c *.h *.rc lisp/*.lsp | wc -l)
	@echo C-files: $$(ls *.c *.h | wc -l)
	@echo Minimum
	@echo linecount: $$(cat *.c *.h $(LISPFILES) | wc -l) 
	@echo sloccount: $$(set -- $$(which sloccount >/dev/null && { sloccount *.c *.h *.rc $(LISPFILES) | grep ansic=; }); echo $$3)
	@echo files: $$(ls *.c *.h $(LISPFILES) | wc -l) 

strip: femto FORCE
	strip femto

docs/flisp.md: pdoc/flisp.html pdoc/h2m.lua
	pandoc -o $@ -t gfm -L pdoc/h2m.lua $<

README.html: README.md
	pandoc -o $@ -f gfm $<

doc: docs/flisp.md README.html

doxygen: FORCE
	doxygen

test: femto FORCE
	(cd test && ./run)

run: femto FORCE
	FEMTORC=femto.rc FEMTOLIB=lisp FEMTO_DEBUG=1  ./femto

val: femto FORCE
	FEMTORC=femto.rc FEMTOLIB=lisp FEMTO_DEBUG=1 valgrind ./femto 2> val.log

clean: FORCE
	-$(RM) -f $(OBJ) $(FLISP_OBJ) $(BINARIES) $(RC_FILES)
	-$(RM) -rf doxygen
	-$(RM) -f docs/flisp.md README.html
	-$(RM) -f val.log debug.out

install: femto femto.rc FORCE
	-$(MKDIR) -p $$DESTDIR$(BINDIR)
	-$(CP) femto $$DESTDIR$(BINDIR)
	-$(MKDIR) -p $$DESTDIR$(DATADIR)/femto/examples
	-$(CP) lisp/*.lsp femto.rc $$DESTDIR$(DATADIR)/femto
	-$(CP) lisp/examples/*.lsp femto.rc $$DESTDIR$(DATADIR)/femto/examples

uninstall: FORCE
	-$(RM) -f $$DESTDIR$(BINDIR)/femto
	-$(RM) -rf $$DESTDIR$(DATADIR)/femto

# Used as dependency forces rebuild, aka .PHONY in GNU make
FORCE: ;
