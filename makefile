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

OBJ     = command.o display.o complete.o data.o gap.o key.o search.o buffer.o replace.o window.o undo.o funcmap.o utils.o hilite.o lisp.o main.o

femto: $(OBJ)
	$(LD) $(LDFLAGS) -o femto $(OBJ) $(LIBS)

complete.o: complete.c header.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c complete.c

command.o: command.c header.h
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

funcmap.o: funcmap.c header.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c funcmap.c

utils.o: utils.c header.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c utils.c

hilite.o: hilite.c header.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c hilite.c

lisp.o: lisp.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -c lisp.c

main.o: main.c header.h
	$(CC) $(CPPFLAGS) $(CFLAGS) \
	  -D E_SCRIPTDIR=$(SCRIPTDIR) \
	  -D E_INITFILE=$(INITFILE) \
	  -c main.c

docs/flisp.md: pdoc/flisp.html pdoc/h2m.lua
	pandoc -o $@ -t gfm -L pdoc/h2m.lua $<

README.html: README.md
	pandoc -o $@ -f gfm $<

doc: docs/flisp.md README.html

doxygen: FORCE
	doxygen

test: femto FORCE
	(cd test && ./run)

clean: FORCE
	-$(RM) -f $(OBJ) femto
	-$(RM) -rf doxygen
	-$(RM) -f docs/flisp.md README.html

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
