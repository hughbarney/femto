#
# makefile
#

CC      = cc
CPPFLAGS += -D_DEFAULT_SOURCE -D_BSD_SOURCE -DNDEBUG
CFLAGS += -O2 -std=c11 -Wall -pedantic -g
LD      = cc
LDFLAGS = -o
LIBS    = -lncursesw
CP      = cp
MV      = mv
RM      = rm

OBJ     = command.o display.o complete.o data.o gap.o key.o search.o buffer.o replace.o window.o undo.o funcmap.o utils.o hilite.o lisp.o main.o

femto: $(OBJ)
	$(LD) $(LDFLAGS) femto $(OBJ) $(LIBS)

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
	$(CC) $(CPPFLAGS) $(CFLAGS) -c main.c



clean:
	-$(RM) $(OBJ) femto

install:
	-$(MV) femto $(HOME)/bin

