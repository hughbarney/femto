#
# makefile
#

CC      = cc
CPP     = cpp
#CPPFLAGS += -D_DEFAULT_SOURCE -D_BSD_SOURCE -DNDEBUG
CPPFLAGS += -D_DEFAULT_SOURCE -D_BSD_SOURCE
#CFLAGS += -O2 -std=c11 -Wall -pedantic -pedantic-errors
CFLAGS += -O0 -std=c11 -Wall -pedantic -pedantic-errors -Werror=format-security -Wformat -g
LD      = cc
LDFLAGS =
LIBS    = -lncursesw -lm
CP      = cp
MV      = mv
RM      = rm
MKDIR	= mkdir
PREFIX  = /usr/local
BINDIR  = $(PREFIX)/bin
DATADIR = $(PREFIX)/share
DOCDIR  = $(DATADIR)/doc
PACKAGE = femto
FLISP_PACKAGE = flisp

# Defaults in C-source
SCRIPTDIR = $(DATADIR)/femto
INITFILE = $(SCRIPTDIR)/femto.rc

OBJ = command.o display.o complete.o data.o gap.o key.o search.o	\
	buffer.o replace.o window.o undo.o funcmap.o utils.o hilite.o	\
	femto_lisp.o file.o double.o main.o

FLISP_OBJ = flisp.o lisp.o double.o file.o
FLISP_LIBS = -lm
BINARIES = femto flisp
RC_FILES = femto.rc flisp.rc

LISPFILES = femto.rc lisp/startup.lsp lisp/defmacro.lsp			\
	lisp/bufmenu.lsp lisp/dired.lsp lisp/grep.lsp lisp/git.lsp	\
	lisp/oxo.lsp lisp/flisp.lsp lisp/femto.lsp lisp/info.lsp        \
	lisp/string.lsp

FLISPFILES = flisp.rc lisp/flisp.lsp
FLISPSOURCES = lisp.c lisp.h double.c double.h file.c file.h

DOCFILES = BUGS CHANGE.LOG.md README.md pdoc/flisp.html
MOREDOCS = README.html docs/flisp.md docs/develop.md docs/femto.md 	\
	docs/editor.md
FLISP_DOCFILES = README.flisp.md docs/flisp.md pdoc/flisp.html 		\
	docs/develop.md pdoc/develop.html docs/editor.md 		\
	pdoc/editor.html


.SUFFIXES: .rc .sht  .md .html
.sht.rc:
	./sht $*.sht >$@


# Artifacts
all: femto

buffer.o: buffer.c header.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c buffer.c

complete.o: complete.c header.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c complete.c

command.o: command.c header.h lisp.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c command.c

data.o: data.c header.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c data.c

debug: CPPFLAGS += -UNDEBUG -g
debug: femto flisp

display.o: display.c header.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c display.c

double.o: double.c double.h lisp.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -D FLISP_DOUBLE_EXTENSION -c $<

femto: $(OBJ) femto.rc
	$(LD) $(LDFLAGS) -o femto $(OBJ) $(LIBS)

femto.rc: femto.sht lisp/core.lsp

femto_lisp.o: lisp.c femto.register.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -D FLISP_FEMTO_EXTENSION -D FLISP_FILE_EXTENSION -D FLISP_DOUBLE_EXTENSION -c lisp.c -o $@

file.o: file.c file.h lisp.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -D FLISP_DOUBLE_EXTENSION -c $<

flisp: $(FLISP_OBJ) flisp.rc
	$(LD) $(LDFLAGS) -o $@ $(FLISP_OBJ) $(FLISP_LIBS)

#flisp.o: flisp.c lisp.h
#	$(CC) $(CPPFLAGS) $(CFLAGS) -c $<

flisp.o: flisp.c lisp.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -D FLISP_DOUBLE_EXTENSION -c $<

flisp.rc: flisp.sht lisp/core.lsp

funcmap.o: funcmap.c header.h lisp.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c funcmap.c

gap.o: gap.c header.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c gap.c

hilite.o: hilite.c header.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c hilite.c

key.o: key.c header.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c key.c

lisp.o: lisp.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -D FLISP_FILE_EXTENSION  -D FLISP_DOUBLE_EXTENSION -c lisp.c

main.o: main.c header.h lisp.h
	$(CC) $(CPPFLAGS) $(CFLAGS) \
	  -D E_SCRIPTDIR=$(SCRIPTDIR) \
	  -D E_INITFILE=$(INITFILE) \
	  -c main.c
replace.o: replace.c header.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c replace.c

search.o: search.c header.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c search.c

undo.o: undo.c header.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c undo.c

utils.o: utils.c header.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c utils.c

window.o: window.c header.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c window.c

# Additional documentation formats

# require pandoc
doc: $(MOREDOCS)

docs/flisp.md: pdoc/flisp.html pdoc/h2m.lua
	pandoc -o $@ -t gfm -L pdoc/h2m.lua $<

docs/develop.md: pdoc/develop.html pdoc/h2m.lua
	pandoc -o $@ -t gfm -L pdoc/h2m.lua $<

docs/editor.md: pdoc/editor.html pdoc/h2m.lua
	pandoc -o $@ -t gfm -L pdoc/h2m.lua $<

docs/femto.md: pdoc/femto.html pdoc/h2m.lua
	pandoc -o $@ -t gfm -L pdoc/h2m.lua $<


README.html: README.md
	pandoc -o $@ -f gfm $<

# require doxygen
doxygen: FORCE
	doxygen

# Development
fl: flisp FORCE
	FLISPRC=flisp.rc FLISPLIB=lisp FLISP_DEBUG=f.log  ./flisp
fld: flisp FORCE
	FLISPRC=flisp.rc FLISPLIB=lisp FLISP_DEBUG=f.log gdb ./flisp
flv: flisp FORCE
	FLISPRC=flisp.rc FLISPLIB=lisp FLISP_DEBUG=f.log valgrind ./flisp
frama-c: FORCE
	frama-c -c11 -cpp-extra-args="-I$(frama-c -print-path)/libc -I/usr/include -I." -kernel-msg-key pp -metrics *.c

measure: $(RC_FILES) $(BINARIES) strip FORCE
	@ln -s femto.rc femto.lsp
	@ln -s flisp.rc flisp.lsp
	@echo Total
	@echo "binsize:      " $$(set -- $$(ls -l femto); echo $$5)
	@echo "C/Lisp lines: " $$(cat *.c *.h | wc -l) / $$(cat $(LISPFILES) | wc -l)
	@echo "Total lines:  " $$(cat *.c *.h $(LISPFILES) | wc -l)
	@echo "Total slocs:  " $$(set -- $$(which sloccount >/dev/null && { sloccount *.c *.h femto.lsp $(LISPFILES) | grep ansic=; }); echo $$3)
	@echo "C/Lisp files: " $$(ls *.c *.h | wc -l) / $$(echo $(LISPFILES) | wc -w)
	@echo "Total files:  " $$(ls *.c *.h $(LISPFILES) | wc -l)
	@echo Minimum
	@echo "binsize:      " $$(set -- $$(ls -l flisp); echo $$5)
	@echo "flisp:        " $$(cat flisp.c | wc -l)
	@echo "flispsloc:    " $$(set -- $$(which sloccount >/dev/null && { sloccount flisp.c | grep ansic=; }); echo $$3)
	@echo "C-lines:      " $$(cat $(FLISPSOURCES) | wc -l)
	@echo "Lisp-lines:   " $$(cat $(FLISPFILES) | wc -l)
	@echo "sloccount:    " $$(set -- $$(which sloccount >/dev/null && { sloccount flisp.lsp $(FLISPSOURCES) $(FLISPFILES) | grep ansic=; }); echo $$3)
	@echo "C-files:      " $$(ls $(FLISPSOURCES) | wc -l)
	@echo "Lisp-files:   " $$(ls $(FLISPFILES) | wc -l)
	@rm femto.lsp flisp.lsp

run: femto FORCE
	FEMTORC=femto.rc FEMTOLIB=lisp FEMTO_DEBUG=1  ./femto

splint: FORCE
	splint +posixlib -macrovarprefix "M_" *.c *.h

test: flisp femto test/test.rc FORCE
	@(cd test && ./test -as)

# Exit 1 if any testsuite fails
check: flisp femto test/test.rc FORCE
	@(cd test && ./test -sa | grep tests, | \
	while read RESULT; do \
	   RESULT=$${RESULT#* tests, }; \
	   RESULT=$${RESULT% failures*}; \
	   [ "$$RESULT" = 0 ] || { echo failed >&2; exit 1; } \
        done )

test/test.rc: test/test.sht lisp/core.lsp


# Manually test femto invocation and review syntax highlighting
ftest: femto FORCE
	FEMTORC=femto.rc FEMTOLIB=lisp FEMTO_DEBUG=1  ./femto
	FEMTORC=femto.rc FEMTOLIB=lisp FEMTO_DEBUG=1  ./femto _no_file_
	FEMTORC=femto.rc FEMTOLIB=lisp FEMTO_DEBUG=1  ./femto +3 test/five_lines.txt
	FEMTORC=femto.rc FEMTOLIB=lisp FEMTO_DEBUG=1  ./femto +6 test/circle.py
	FEMTORC=femto.rc FEMTOLIB=lisp FEMTO_DEBUG=1  ./femto +8 lisp.c
	FEMTORC=femto.rc FEMTOLIB=lisp FEMTO_DEBUG=1  ./femto +6 lisp/core.lsp

val: femto FORCE
	FEMTORC=femto.rc FEMTOLIB=lisp FEMTO_DEBUG=1 valgrind ./femto 2> val.log

# Install/package
strip: femto flisp FORCE
	strip femto flisp

clean: FORCE
	-$(RM) -f $(OBJ) $(FLISP_OBJ) $(BINARIES) $(RC_FILES)
	-$(RM) -rf doxygen
	-$(RM) -f $(MOREDOCS)
	-$(RM) -f val.log debug.out f.log
	-$(RM) -rf debian/femto debian/files \
		debian/femto.debhelper.log debian/femto.substvars
	-$(RM) -f test/test.rc  test/debug.out

deb: FORCE
	dpkg-buildpackage -b -us -uc

# fLisp standalone
flisp-install: flisp-install-bin flisp-install-lib flisp-install-doc

flisp-install-bin: flisp FORCE
	-$(MKDIR) -p $(DESTDIR)$(BINDIR)
	-$(CP) flisp $(DESTDIR)$(BINDIR)

flisp-install-doc: FORCE
	-$(MKDIR) -p $(DESTDIR)$(DOCDIR)/$(FLISP_PACKAGE)/examples
	-$(CP) $(FLISP_DOCFILES) $(DESTDIR)$(DOCDIR)/$(FLISP_PACKAGE)
	-$(CP) lisp/examples/*.lsp $(DESTDIR)$(DOCDIR)/$(PACKAGE)/examples

flisp-install-lib: $(FLISPFILES) FORCE
	-$(MKDIR) -p $(DESTDIR)$(DATADIR)/$(FLISP_PACKAGE)
	-$(CP) $(FLISPFILES) $(DESTDIR)$(DATADIR)/$(FLISP_PACKAGE)

flisp-uninstall: FORCE
	-$(RM) -f $(DESTDIR)$(BINDIR)/$(FLISP_PACKAGE)
	-$(RM) -rf $(DESTDIR)$(DATADIR)/$(FLISP_PACKAGE)
	-$(RM) -rf $(DESTDIR)$(DOCDIR)/$(FLISP_PACKAGE)


# Femto
install: install-bin install-lib install-doc FORCE

install-bin: femto FORCE
	-$(MKDIR) -p $(DESTDIR)$(BINDIR)
	-$(CP) femto $(DESTDIR)$(BINDIR)

install-doc: FORCE
	-$(MKDIR) -p $(DESTDIR)$(DOCDIR)/$(PACKAGE)/examples
	-$(CP) $(DOCFILES) $(DESTDIR)$(DOCDIR)/$(PACKAGE)
	-$(CP) lisp/examples/*.lsp $(DESTDIR)$(DOCDIR)/$(PACKAGE)/examples
	-for f in $(MOREDOCS); do [ -f $$f ] && $(CP) $$f $(DESTDIR)$(DOCDIR)/$(PACKAGE); done; true

install-lib: $(LISPFILES) FORCE
	-$(MKDIR) -p $(DESTDIR)$(DATADIR)/$(PACKAGE)
	-$(CP) $(LISPFILES) $(DESTDIR)$(DATADIR)/$(PACKAGE)

uninstall: FORCE
	-$(RM) -f $(DESTDIR)$(BINDIR)/$(PACKAGE)
	-$(RM) -rf $(DESTDIR)$(DATADIR)/$(PACKAGE)
	-$(RM) -rf $(DESTDIR)$(DOCDIR)/$(PACKAGE)

# Used as dependency forces rebuild, aka .PHONY in GNU make
FORCE: ;
