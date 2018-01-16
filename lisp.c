/*
 * tiny-lisp interpretter based on
 * https://github.com/matp/tiny-lisp
 *
 * with modifications and extensions to enable it to be embedded and called
 * by an application.
 *
 *   int init_lisp()
 *   char *call_lisp(char *input)
 *   char *load_file(int fd)
 *   void reset_output_stream()
 *
 * Input and Output is a Stream abstraction layer.
 * There is 1 and only 1 output stream which is only cleared
 * after calling reset_output_stream().
 * Each call to call_lisp() or load_file() results in the creation of
 * a new input stream instance so that these calls do not fight over
 * the input stream.
 *
 */


#include <sys/mman.h>
#include <sys/stat.h>
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <curses.h>

extern void debug(char *,...);
#define F_NONE          0
#define F_CLEAR         1
typedef long point_t;

#if !defined(MAP_ANONYMOUS) && defined(MAP_ANON)
#define MAP_ANONYMOUS        MAP_ANON
#endif

//#define MEMORY_SIZE          131072UL
#define MEMORY_SIZE          262144UL  /* 256k */

typedef struct Object Object;



typedef enum Type {
	TYPE_NUMBER,
	TYPE_STRING,
	TYPE_SYMBOL,
	TYPE_CONS,
	TYPE_LAMBDA,
	TYPE_MACRO,
	TYPE_PRIMITIVE,
	TYPE_ENV
} Type;

struct Object {
	Type type;
	size_t size;
	union {
		struct { double number; };                      // number
		struct { char string[sizeof (Object *[3])]; };  // string, symbol
		struct { Object *car, *cdr; };                  // cons
		struct { Object *params, *body, *env; };        // lambda, macro
		struct { int primitive; char *name; };          // primitive
		struct { Object *parent, *vars, *vals; };       // env
		struct { Object *forward; };                    // forwarding pointer
  };
};

static Object *symbols = NULL;
static Object *nil = &(Object) { TYPE_SYMBOL,.string = "nil" };
static Object *t = &(Object) { TYPE_SYMBOL,.string = "t" };

typedef enum StreamType {
	STREAM_TYPE_STRING,
	STREAM_TYPE_FILE
} StreamType;

typedef struct Stream {
	StreamType type;
	char *buffer;
	int fd;
	size_t length, capacity;
	off_t offset, size;
} Stream;

//Stream istream = { .type = STREAM_TYPE_FILE,.fd = STDIN_FILENO };
Stream ostream = { .type = STREAM_TYPE_FILE, .buffer = NULL, .fd = STDOUT_FILENO };

typedef struct Memory {
	size_t capacity, fromOffset, toOffset;
	void *fromSpace, *toSpace;
} Memory;

static Memory *memory = &(Memory) { MEMORY_SIZE };

static jmp_buf exceptionEnv;

// EXCEPTION HANDLING /////////////////////////////////////////////////////////

#define exception(...)       exceptionWithObject(NULL, __VA_ARGS__)

#ifdef __GNUC__
void exceptionWithObject(Object * object, char *format, ...)
    __attribute__ ((noreturn, format(printf, 2, 3)));

void writeFmt(Stream *, char *format, ...)
    __attribute__ ((format(printf, 2, 3)));
#endif

void writeObject(Object * object, bool readably, Stream *);
#define WRITE_FMT_BUFSIZ 2048

void writeString(char *str, Stream *stream)
{
   int len;
   char *new;

   switch (stream->type) {
   case STREAM_TYPE_FILE:
        len = strlen(str);
        len = write(stream->fd, str, len);
	return;

    case STREAM_TYPE_STRING:
    default:		
        len = strlen(str);
	new = realloc(stream->buffer, stream->length + len + 1);
	assert(new != NULL);
	memcpy(new + stream->length, str, len);
	stream->buffer = new;
	stream->length += len;
	new[stream->length] = '\0';
	return;
    }
}

void writeFmt(Stream *stream, char *format, ...)
{
    static char buf[WRITE_FMT_BUFSIZ];
    int nbytes;

    va_list args;
    va_start(args, format);
    nbytes = vsnprintf(buf, WRITE_FMT_BUFSIZ, format, args);
    va_end(args);

    switch (stream->type) {
    case STREAM_TYPE_FILE:
	nbytes = write(stream->fd, buf, nbytes);
	return;

    case STREAM_TYPE_STRING:
    default:
	writeString(buf, stream);
	return;
    }
}

void writeChar(char ch, Stream *stream)
{
    char str[2];
    int i = 1;

    str[0] = ch;
    str[1] = '\0';
    
    switch (stream->type) {
    case STREAM_TYPE_FILE:
	i = write(stream->fd, str, i);
	return;

    case STREAM_TYPE_STRING:
    default:		
	writeString(str, stream);
	return;
    }
}

void exceptionWithObject(Object * object, char *format, ...)
{
	static char buf[WRITE_FMT_BUFSIZ];
	
	writeString("error: ", &ostream);

	if (object) {
		writeObject(object, true, &ostream);
		writeChar(' ', &ostream);
	}

	va_list args;
	va_start(args, format);
	vsnprintf(buf, WRITE_FMT_BUFSIZ, format, args);
	va_end(args);

	writeString(buf, &ostream);
	writeChar('\n', &ostream);

	longjmp(exceptionEnv, 1);
}

// GARBAGE COLLECTION /////////////////////////////////////////////////////////

/* This implements Cheney's copying garbage collector, with which memory is
 * divided into two equal halves (semispaces): from- and to-space. From-space
 * is where new objects are allocated, whereas to-space is used during garbage
 * collection.
 *
 * When garbage collection is performed, objects that are still in use (live)
 * are copied from from-space to to-space. To-space then becomes the new
 * from-space and vice versa, thereby discarding all objects that have not
 * been copied.
 *
 * Our garbage collector takes as input a list of root objects. Objects that
 * can be reached by recursively traversing this list are considered live and
 * will be moved to to-space. When we move an object, we must also update its
 * pointer within the list to point to the objects new location in memory.
 *
 * However, this implies that our interpreter cannot use raw pointers to
 * objects in any function that might trigger garbage collection (or risk
 * causing a SEGV when accessing an object that has been moved). Instead,
 * objects must be added to the list and then only accessed through the
 * pointer inside the list.
 *
 * Thus, whenever we would have used a raw pointer to an object, we use a
 * pointer to the pointer inside the list instead:
 *
 *   function:              pointer to pointer inside list (Object **)
 *                                  |
 *                                  v
 *   list of root objects:  pointer to object (Object *)
 *                                  |
 *                                  v
 *   semispace:             object in memory
 *
 * GC_ROOTS and GC_PARAM are used to pass the list from function to function.
 *
 * GC_TRACE adds an object to the list and declares a variable which points to
 * the objects pointer inside the list.
 *
 *   GC_TRACE(gcX, X):  add object X to the list and declare Object **gcX
 *                      to point to the pointer to X inside the list.
 */

#define GC_ROOTS             gcRoots
#define GC_PARAM             Object *GC_ROOTS

#define GC_PASTE1(name, id)  name ## id
#define GC_PASTE2(name, id)  GC_PASTE1(name, id)
#define GC_UNIQUE(name)      GC_PASTE2(name, __LINE__)

#define GC_TRACE(name, init)                                                 \
  Object GC_UNIQUE(GC_ROOTS) = { TYPE_CONS, .car = init, .cdr = GC_ROOTS };  \
  Object **name = &GC_UNIQUE(GC_ROOTS).car;                                  \
  GC_ROOTS = &GC_UNIQUE(GC_ROOTS)

Object *gcMoveObject(Object * object)
{
	// skip object if it is not within from-space (i.e. on the stack)
	if (object < (Object *) memory->fromSpace || object >= (Object *) ((char *)memory->fromSpace + memory->fromOffset))
		return object;

	// if the object has already been moved, return its new location
	if (object->type == (Type) - 1)
		return object->forward;

	// copy object to to-space
	Object *forward = (Object *) ((char *)memory->toSpace + memory->toOffset);
	memcpy(forward, object, object->size);
	memory->toOffset += object->size;

	// mark object as moved and set forwarding pointer
	object->type = (Type) - 1;
	object->forward = forward;

	return object->forward;
}

void gc(GC_PARAM)
{
	memory->toOffset = 0;

	// move symbols and root objects
	symbols = gcMoveObject(symbols);

	for (Object * object = GC_ROOTS; object != nil; object = object->cdr)
		object->car = gcMoveObject(object->car);

	// iterate over objects in to-space and move all objects they reference
	for (Object * object = memory->toSpace; object < (Object *) ((char *)memory->toSpace + memory->toOffset); object = (Object *) ((char *)object + object->size)) {

		switch (object->type) {
		case TYPE_NUMBER:
		case TYPE_STRING:
		case TYPE_SYMBOL:
		case TYPE_PRIMITIVE:
			break;
		case TYPE_CONS:
			object->car = gcMoveObject(object->car);
			object->cdr = gcMoveObject(object->cdr);
			break;
		case TYPE_LAMBDA:
		case TYPE_MACRO:
			object->params = gcMoveObject(object->params);
			object->body = gcMoveObject(object->body);
			object->env = gcMoveObject(object->env);
			break;
		case TYPE_ENV:
			object->parent = gcMoveObject(object->parent);
			object->vars = gcMoveObject(object->vars);
			object->vals = gcMoveObject(object->vals);
			break;
		}
	}

	// swap from- and to-space
	void *swap = memory->fromSpace;
	memory->fromSpace = memory->toSpace;
	memory->toSpace = swap;
	memory->fromOffset = memory->toOffset;
}

// MEMORY MANAGEMENT //////////////////////////////////////////////////////////

size_t memoryAlign(size_t size, size_t alignment)
{
	return (size + alignment - 1) & ~(alignment - 1);
}

Object *memoryAllocObject(Type type, size_t size, GC_PARAM)
{
	size = memoryAlign(size, sizeof(void *));

	// allocate from- and to-space
	if (!memory->fromSpace) {
		if (!(memory->fromSpace = mmap(NULL, memory->capacity * 2, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0)))
			exception("mmap() failed, %s", strerror(errno));

		memory->toSpace = (char *)memory->fromSpace + memory->capacity;
	}
	// run garbage collection if capacity exceeded
	if (memory->fromOffset + size >= memory->capacity)
		gc(GC_ROOTS);
	if (memory->fromOffset + size >= memory->capacity)
		exception("out of memory, %lu bytes", (unsigned long)size);

	// allocate object in from-space
	Object *object = (Object *) ((char *)memory->fromSpace + memory->fromOffset);
	object->type = type;
	object->size = size;
	memory->fromOffset += size;

	return object;
}

// CONSTRUCTING OBJECTS ///////////////////////////////////////////////////////

Object *newObject(Type type, GC_PARAM)
{
	return memoryAllocObject(type, sizeof(Object), GC_ROOTS);
}

Object *newObjectFrom(Object ** from, GC_PARAM)
{
	Object *object = memoryAllocObject((*from)->type, (*from)->size, GC_ROOTS);
	memcpy(object, *from, (*from)->size);
	return object;
}

Object *newNumber(double number, GC_PARAM)
{
	Object *object = newObject(TYPE_NUMBER, GC_ROOTS);
	object->number = number;
	return object;
}

Object *newObjectWithString(Type type, size_t size, GC_PARAM)
{
	size = (size > sizeof(((Object *) NULL)->string))
	    ? size - sizeof(((Object *) NULL)->string)
	    : 0;
	return memoryAllocObject(type, sizeof(Object) + size, GC_ROOTS);
}

Object *newStringWithLength(char *string, size_t length, GC_PARAM)
{
	int nEscapes = 0;

	for (int i = 1; i < length; ++i)
		if (string[i - 1] == '\\' && strchr("\\\"trn", string[i]))
			++i, ++nEscapes;

	Object *object = newObjectWithString(TYPE_STRING,
					     length - nEscapes + 1, GC_ROOTS);

	for (int r = 1, w = 0; r <= length; ++r) {
		if (string[r - 1] == '\\' && r < length) {
			switch (string[r]) {
			case '\\':
				object->string[w++] = '\\';
				r++;
				break;
			case '"':
				object->string[w++] = '"';
				r++;
				break;
			case 't':
				object->string[w++] = '\t';
				r++;
				break;
			case 'r':
				object->string[w++] = '\r';
				r++;
				break;
			case 'n':
				object->string[w++] = '\n';
				r++;
				break;
			default:
				object->string[w++] = '\\';
				break;
			}
		} else
			object->string[w++] = string[r - 1];
	}

	object->string[length - nEscapes] = '\0';
	return object;
}

Object *newString(char *string, GC_PARAM)
{
	return newStringWithLength(string, strlen(string), GC_ROOTS);
}

Object *newCons(Object ** car, Object ** cdr, GC_PARAM)
{
	Object *object = newObject(TYPE_CONS, GC_ROOTS);
	object->car = *car;
	object->cdr = *cdr;
	return object;
}

Object *newSymbolWithLength(char *string, size_t length, GC_PARAM)
{
	for (Object * object = symbols; object != nil; object = object->cdr)
		if (memcmp(object->car->string, string, length) == 0 && object->car->string[length] == '\0')
			return object->car;

	GC_TRACE(gcObject, newObjectWithString(TYPE_SYMBOL, length + 1, GC_ROOTS));
	memcpy((*gcObject)->string, string, length);
	(*gcObject)->string[length] = '\0';

	symbols = newCons(gcObject, &symbols, GC_ROOTS);

	return *gcObject;
}

Object *newSymbol(char *string, GC_PARAM)
{
	return newSymbolWithLength(string, strlen(string), GC_ROOTS);
}

Object *newObjectWithClosure(Type type, Object ** params, Object ** body, Object ** env, GC_PARAM)
{
	Object *list;

	for (list = *params; list->type == TYPE_CONS; list = list->cdr) {
		if (list->car->type != TYPE_SYMBOL)
			exceptionWithObject(list->car, "is not a symbol");
		if (list->car == nil || list->car == t)
			exceptionWithObject(list->car, "cannot be used as a parameter");
	}

	if (list != nil && list->type != TYPE_SYMBOL)
		exceptionWithObject(list, "is not a symbol");

	Object *object = newObject(type, GC_ROOTS);
	object->params = *params;
	object->body = *body;
	object->env = *env;
	return object;
}

Object *newLambda(Object ** params, Object ** body, Object ** env, GC_PARAM)
{
	return newObjectWithClosure(TYPE_LAMBDA, params, body, env, GC_ROOTS);
}

Object *newMacro(Object ** params, Object ** body, Object ** env, GC_PARAM)
{
	return newObjectWithClosure(TYPE_MACRO, params, body, env, GC_ROOTS);
}

Object *newPrimitive(int primitive, char *name, GC_PARAM)
{
	Object *object = newObject(TYPE_PRIMITIVE, GC_ROOTS);
	object->primitive = primitive;
	object->name = name;
	return object;
}

Object *newEnv(Object ** func, Object ** vals, GC_PARAM)
{
	Object *object = newObject(TYPE_ENV, GC_ROOTS);

	if ((*func) == nil)
		object->parent = object->vars = object->vals = nil;
	else {
		Object *param = (*func)->params, *val = *vals;

		for (int nArgs = 0;; param = param->cdr, val = val->cdr, ++nArgs) {
			if (param == nil && val == nil)
				break;
			else if (param != nil && param->type == TYPE_SYMBOL)
				break;
			else if (val != nil && val->type != TYPE_CONS)
				exceptionWithObject(val, "is not a list");
			else if (param == nil && val != nil)
				exceptionWithObject(*func, "expects at most %d arguments", nArgs);
			else if (param != nil && val == nil) {
				for (; param->type == TYPE_CONS; param = param->cdr, ++nArgs);
				exceptionWithObject(*func, "expects at least %d arguments", nArgs);
			}
		}

		object->parent = (*func)->env;
		object->vars = (*func)->params;
		object->vals = *vals;
	}

	return object;
}

// STREAM INPUT ///////////////////////////////////////////////////////////////

/* The purpose of the stream functions is to provide an abstraction over file
 * and string inputs. In order to accommodate the REPL, we need to be able to
 * process character special files (such as stdin) character by character and
 * evaluate expressions as they are being entered.
 */

int streamGetc(Stream * stream)
{
	if (stream->offset >= stream->length) {
		switch (stream->type) {
		case STREAM_TYPE_STRING:
			// set length if a string was given but its length has not been set
			if (!stream->length && stream->buffer && *stream->buffer) {
				stream->length = strlen(stream->buffer);
				return streamGetc(stream);
			}
			return EOF;

		case STREAM_TYPE_FILE:
			// if this is the first read, try to find the size of the file
			if (!stream->buffer) {
				struct stat st;

				if (fstat(stream->fd, &st) == -1)
					exception("fstat() failed, %s", strerror(errno));

				if (S_ISREG(st.st_mode)) {
					stream->size = st.st_size;

					if (!(stream->buffer = malloc(stream->size)))
						exception("out of memory, %ld bytes", (long)stream->size);

					stream->capacity = stream->size;
				} else
					stream->size = -1;
			}
			// resize buffer to nearest multiple of BUFSIZ if capacity exceeded
			if (stream->offset >= stream->capacity) {
				char *buffer;
				size_t capacity = stream->offset ? (stream->offset / BUFSIZ + 1) * BUFSIZ : BUFSIZ;

				if (!(buffer = realloc(stream->buffer, capacity)))
					exception("out of memory, %ld bytes", (long)capacity);

				stream->buffer = buffer;
				stream->capacity = capacity;
			}
			// read until offset reached
			while (stream->length <= stream->offset) {
				ssize_t nbytes = read(stream->fd, stream->buffer + stream->length,
						      stream->capacity - stream->length);

				if (nbytes > 0)
					stream->length += nbytes;
				else if (nbytes < 0 && errno != EINTR)
					exception("read() failed, %s", strerror(errno));

				if (nbytes == 0 || stream->length == stream->size) {
					stream->type = STREAM_TYPE_STRING;
					return streamGetc(stream);
				}
			}

			break;
		}
	}

	return (unsigned char)stream->buffer[stream->offset++];
}

Stream *streamSeek(Stream * stream, int offset)
{
	if (offset < 0 && -offset >= stream->offset)
		stream->offset = 0;
	else
		stream->offset += offset;
	return stream;
}

int streamPeek(Stream * stream)
{
	int ch = streamGetc(stream);
	if (ch != EOF)
		streamSeek(stream, -1);
	return ch;
}

// READING S-EXPRESSIONS //////////////////////////////////////////////////////

Object *readExpr(Stream * stream, GC_PARAM);

int readNext(Stream * stream)
{
	for (;;) {
		int ch = streamGetc(stream);
		if (ch == ';')
			while ((ch = streamGetc(stream)) != EOF && ch != '\n');
		if (isspace(ch))
			continue;
		return ch;
	}
}

int peekNext(Stream * stream)
{
	int ch = readNext(stream);
	if (ch != EOF)
		streamSeek(stream, -1);
	return ch;
}

int readWhile(Stream * stream, int (*predicate) (int ch))
{
	for (;;) {
		int ch = streamPeek(stream);
		if (!predicate(ch))
			return ch;
		streamGetc(stream);
	}
}

Object *readUnary(Stream * stream, char *symbol, GC_PARAM)
{
	if (peekNext(stream) == EOF)
		exception("unexpected end of stream in %s", symbol);

	GC_TRACE(gcSymbol, newSymbol(symbol, GC_ROOTS));
	GC_TRACE(gcObject, readExpr(stream, GC_ROOTS));

	*gcObject = newCons(gcObject, &nil, GC_ROOTS);
	*gcObject = newCons(gcSymbol, gcObject, GC_ROOTS);

	return *gcObject;
}

Object *readString(Stream * stream, GC_PARAM)
{
	size_t offset = stream->offset;

	for (bool isEscaped = false;;) {
		int ch = streamGetc(stream);
		if (ch == EOF)
			exception("unexpected end of stream in string literal \"%.*s\"", (int)(stream->offset - offset), stream->buffer + offset);
		if (ch == '"' && !isEscaped)
			return newStringWithLength(stream->buffer + offset, stream->offset - offset - 1, GC_ROOTS);

		isEscaped = (ch == '\\' && !isEscaped);
	}
}

int isSymbolChar(int ch)
{
	static const char *valid = "!#$%&*+-./:<=>?@^_~";
	return isalnum(ch) || strchr(valid, ch);
}

Object *readNumberOrSymbol(Stream * stream, GC_PARAM)
{
	size_t offset = stream->offset;
	int ch = streamPeek(stream);

	// skip optional leading sign
	if (ch == '+' || ch == '-') {
		streamGetc(stream);
		ch = streamPeek(stream);
	}
	// try to read a number in integer or decimal format
	if (ch == '.' || isdigit(ch)) {
		if (isdigit(ch))
			ch = readWhile(stream, isdigit);
		if (!isSymbolChar(ch))
			return newNumber(strtol(stream->buffer + offset, NULL, 10), GC_ROOTS);
		if (ch == '.') {
			ch = streamGetc(stream);
			if (isdigit(streamPeek(stream))) {
				ch = readWhile(stream, isdigit);
				if (!isSymbolChar(ch))
					return newNumber(strtod(stream->buffer + offset, NULL), GC_ROOTS);
			}
		}
	}
	// non-numeric character encountered, read a symbol
	readWhile(stream, isSymbolChar);
	return newSymbolWithLength(stream->buffer + offset, stream->offset - offset, GC_ROOTS);
}

Object *reverseList(Object * list)
{
	Object *object = nil;

	while (list != nil) {
		Object *swap = list;
		list = list->cdr;
		swap->cdr = object;
		object = swap;
	}

	return object;
}

Object *readList(Stream * stream, GC_PARAM)
{
	GC_TRACE(gcList, nil);
	GC_TRACE(gcLast, nil);

	for (;;) {
		int ch = readNext(stream);
		if (ch == EOF)
			exception("unexpected end of stream in list");
		else if (ch == ')')
			return reverseList(*gcList);
		else if (ch == '.' && !isSymbolChar(streamPeek(stream))) {
			if (*gcLast == nil)
				exception("unexpected dot at start of list");
			if ((ch = peekNext(stream)) == ')')
				exception("expected object at end of dotted list");
			if (!(*gcLast = readExpr(stream, GC_ROOTS)))
				exception("unexpected end of stream in dotted list");
			if ((ch = peekNext(stream)) != ')')
				exception("unexpected object at end of dotted list");
			readNext(stream);
			Object *list = reverseList(*gcList);
			(*gcList)->cdr = *gcLast;

			return list;
		} else {
			*gcLast = readExpr(streamSeek(stream, -1), GC_ROOTS);
			*gcList = newCons(gcLast, gcList, GC_ROOTS);
		}
	}
}

Object *readExpr(Stream * stream, GC_PARAM)
{
	for (;;) {

		int ch = readNext(stream);

		if (ch == EOF)
			return NULL;
		else if (ch == '\'')
			return readUnary(stream, "quote", GC_ROOTS);
		else if (ch == '"')
			return readString(stream, GC_ROOTS);
		else if (ch == '(')
			return readList(stream, GC_ROOTS);
		else if (isSymbolChar(ch)
			 && (ch != '.' || isSymbolChar(streamPeek(stream))))
			return readNumberOrSymbol(streamSeek(stream, -1), GC_ROOTS);
		else
			exception("unexpected character, `%c'", ch);
	}
}

// WRITING OBJECTS ////////////////////////////////////////////////////////////

void writeObject(Object * object, bool readably, Stream *stream)
{
	switch (object->type) {
#define CASE(type, ...)                                                      \
  case type:                                                                 \
    writeFmt(stream, __VA_ARGS__);                                              \
    break
		CASE(TYPE_NUMBER, "%g", object->number);
		CASE(TYPE_SYMBOL, "%s", object->string);
		CASE(TYPE_PRIMITIVE, "#<Primitive %s>", object->name);
#undef CASE
	case TYPE_STRING:
		if (readably) {
			writeChar('"', stream);
			for (char *string = object->string; *string; ++string) {
				switch (*string) {
				case '"':
					writeString("\\\"", stream);
					break;
				case '\t':
					writeString("\\t", stream);
					break;
				case '\r':
					writeString("\\r", stream);
					break;
				case '\n':
					writeString("\\n", stream);
					break;
				case '\\':
					writeString("\\\\", stream);
					break;
				default:
					writeChar(*string, stream);
					break;
				}
			}
			writeChar('"', stream);
		} else
			writeFmt(stream, "%s", object->string);
		break;
	case TYPE_CONS:
		writeChar('(', stream);
		writeObject(object->car, readably, stream);
		while (object->cdr != nil) {
			object = object->cdr;
			if (object->type == TYPE_CONS) {
				writeChar(' ', stream);
				writeObject(object->car, readably, stream);
			} else {
				writeString(" . ", stream);
				writeObject(object, readably, stream);
				break;
			}
		}
		writeChar(')', stream);
		break;
#define CASE(type, name, object)                                             \
  case type:                                                                 \
    writeFmt(stream, "#<%s ", name);                                            \
    writeObject(object, readably, stream);                                     \
    writeChar('>', stream);                                                    \
    break
		CASE(TYPE_LAMBDA, "Lambda", object->params);
		CASE(TYPE_MACRO, "Macro", object->params);
		CASE(TYPE_ENV, "Env", object->vars);
#undef CASE
	}
}

// ENVIRONMENT ////////////////////////////////////////////////////////////////

/* An environment consists of a pointer to its parent environment (if any) and
 * two parallel lists - vars and vals.
 *
 * Case 1 - vars is a regular list:
 *   vars: (a b c), vals: (1 2 3)        ; a = 1, b = 2, c = 3
 *
 * Case 2 - vars is a dotted list:
 *   vars: (a b . c), vals: (1 2)        ; a = 1, b = 2, c = nil
 *   vars: (a b . c), vals: (1 2 3)      ; a = 1, b = 2, c = (3)
 *   vars: (a b . c), vals: (1 2 3 4 5)  ; a = 1, b = 2, c = (3 4 5)
 *
 * Case 3 - vars is a symbol:
 *   vars: a, vals: nil                  ; a = nil
 *   vars: a, vals: (1)                  ; a = (1)
 *   vars: a, vals: (1 2 3)              ; a = (1 2 3)
 *
 * Case 4 - vars and vals are both nil:
 *   vars: nil, vals: nil
 */

Object *envLookup(Object * var, Object * env)
{
	for (; env != nil; env = env->parent) {
		Object *vars = env->vars, *vals = env->vals;

		for (; vars->type == TYPE_CONS; vars = vars->cdr, vals = vals->cdr)
			if (vars->car == var)
				return vals->car;

		if (vars == var)
			return vals;
	}

	exceptionWithObject(var, "has no value");
}

Object *envAdd(Object ** var, Object ** val, Object ** env, GC_PARAM)
{
	GC_TRACE(gcVars, newCons(var, &nil, GC_ROOTS));
	GC_TRACE(gcVals, newCons(val, &nil, GC_ROOTS));

	(*gcVars)->cdr = (*env)->vars, (*env)->vars = *gcVars;
	(*gcVals)->cdr = (*env)->vals, (*env)->vals = *gcVals;

	return *val;
}

Object *envSet(Object ** var, Object ** val, Object ** env, GC_PARAM)
{
	GC_TRACE(gcEnv, *env);

	for (;;) {
		Object *vars = (*gcEnv)->vars, *vals = (*gcEnv)->vals;

		for (; vars->type == TYPE_CONS; vars = vars->cdr, vals = vals->cdr) {
			if (vars->car == *var)
				return vals->car = *val;
			if (vars->cdr == *var)
				return vals->cdr = *val;
		}

		if ((*gcEnv)->parent == nil)
			return envAdd(var, val, gcEnv, GC_ROOTS);
		else
			*gcEnv = (*gcEnv)->parent;
	}
}

// PRIMITIVES /////////////////////////////////////////////////////////////////

Object *primitiveAtom(Object ** args, GC_PARAM)
{
	return ((*args)->car->type != TYPE_CONS) ? t : nil;
}

Object *primitiveEq(Object ** args, GC_PARAM)
{
	Object *first = (*args)->car, *second = (*args)->cdr->car;

	if (first->type == TYPE_NUMBER && second->type == TYPE_NUMBER)
		return (first->number == second->number) ? t : nil;
	else if (first->type == TYPE_STRING && second->type == TYPE_STRING)
		return !strcmp(first->string, second->string) ? t : nil;
	else
		return (first == second) ? t : nil;
}

Object *primitiveCar(Object ** args, GC_PARAM)
{
	Object *first = (*args)->car;

	if (first == nil)
		return nil;
	else if (first->type == TYPE_CONS)
		return first->car;
	else
		exceptionWithObject(first, "is not a list");
}

Object *primitiveCdr(Object ** args, GC_PARAM)
{
	Object *first = (*args)->car;

	if (first == nil)
		return nil;
	else if (first->type == TYPE_CONS)
		return first->cdr;
	else
		exceptionWithObject(first, "is not a list");
}

Object *primitiveCons(Object ** args, GC_PARAM)
{
	GC_TRACE(gcFirst, (*args)->car);
	GC_TRACE(gcSecond, (*args)->cdr->car);

	return newCons(gcFirst, gcSecond, GC_ROOTS);
}

Object *primitivePrint(Object ** args, GC_PARAM)
{
	writeChar('\n', &ostream);
	writeObject((*args)->car, true, &ostream);
	writeChar(' ', &ostream);
	return (*args)->car;
}

Object *primitivePrinc(Object ** args, GC_PARAM)
{
	writeObject((*args)->car, false, &ostream);
	return (*args)->car;
}

/************************* Editer Extensions **************************************/

#define DEFINE_EDITOR_FUNC(name) \
extern void name(); \
Object *e_##name(Object ** args, GC_PARAM) \
{ \
	name(); \
	return t; \
}

DEFINE_EDITOR_FUNC(beginning_of_buffer)
DEFINE_EDITOR_FUNC(end_of_buffer)
DEFINE_EDITOR_FUNC(left)
DEFINE_EDITOR_FUNC(right)
DEFINE_EDITOR_FUNC(forward_word)
DEFINE_EDITOR_FUNC(backward_word)
DEFINE_EDITOR_FUNC(up)
DEFINE_EDITOR_FUNC(down)
DEFINE_EDITOR_FUNC(lnbegin)
DEFINE_EDITOR_FUNC(lnend)
DEFINE_EDITOR_FUNC(yank)
DEFINE_EDITOR_FUNC(update_display)
DEFINE_EDITOR_FUNC(clear_message_line)
DEFINE_EDITOR_FUNC(copy_region)
DEFINE_EDITOR_FUNC(set_mark)
DEFINE_EDITOR_FUNC(kill_region)
DEFINE_EDITOR_FUNC(delete)
DEFINE_EDITOR_FUNC(backspace)
DEFINE_EDITOR_FUNC(forward_page)
DEFINE_EDITOR_FUNC(backward_page)
DEFINE_EDITOR_FUNC(quit)
DEFINE_EDITOR_FUNC(eval_block)
DEFINE_EDITOR_FUNC(delete_other_windows)
DEFINE_EDITOR_FUNC(list_buffers)
DEFINE_EDITOR_FUNC(split_window)
DEFINE_EDITOR_FUNC(other_window)
DEFINE_EDITOR_FUNC(execute_key)

extern int set_key(char *, char *);
extern int getinput(char *, char *, int, int);
extern char *get_char(void);
extern char *get_input_key(void);
extern char *get_key_name(void);
extern char *get_key_funcname(void);
extern char *get_clipboard(void);
extern char *get_current_bufname(void);
extern void set_scrap(unsigned char *);
extern void execute_key(void);
extern int select_buffer(char *);
extern int delete_buffer_byname(char *);
extern void rename_current_buffer(char *);
extern int save_buffer_byname(char *);
extern int count_buffers(void);
extern void display_prompt_and_response(char *, char *);
extern void msg(char *,...);
extern void clear_message_line(void);
extern void log_message(char *);
extern void insert_string(char *);
extern void move_to_search_result(point_t);
extern point_t search_forward(char *);
extern point_t search_backwards(char *);
extern void readfile(char *);
extern void shell_command(char *);
extern int goto_line(int);
extern int add_mode_global(char *);
extern char *get_version_string(void);
extern char *get_temp_file(void);

Object *e_get_char(Object **args, GC_PARAM) { return newStringWithLength(get_char(), 1, GC_ROOTS); }
Object *e_get_key(Object **args, GC_PARAM) { return newString(get_input_key(), GC_ROOTS); }
Object *e_get_key_name(Object **args, GC_PARAM) { return newString(get_key_name(), GC_ROOTS); }
Object *e_get_key_funcname(Object **args, GC_PARAM) { return newString(get_key_funcname(), GC_ROOTS); }
Object *e_get_clipboard(Object **args, GC_PARAM) { return newString(get_clipboard(), GC_ROOTS); }
Object *e_get_buffer_count(Object **args, GC_PARAM) { return newNumber(count_buffers(), GC_ROOTS); }

#define TWO_STRING_ARGS()                                    \
	Object *first = (*args)->car;                        \
	Object *second = (*args)->cdr->car;                  \
	if (first->type != TYPE_STRING)                      \
	    exceptionWithObject(first, "is not a string");   \
	if (second->type != TYPE_STRING)                     \
	    exceptionWithObject(second, "is not a string");  

#define ONE_STRING_ARG()                                    \
	Object *first = (*args)->car;                        \
	if (first->type != TYPE_STRING)                      \
	    exceptionWithObject(first, "is not a string");


Object *e_refresh(Object ** args, GC_PARAM)
{
	refresh();
	return t;
}

Object *e_set_key(Object ** args, GC_PARAM)
{
	TWO_STRING_ARGS();
	return (1 == set_key(first->string, second->string) ? t : nil);
}

Object *e_add_mode_global(Object ** args, GC_PARAM)
{
	ONE_STRING_ARG();
	return (1 == add_mode_global(first->string) ? t : nil);
}

Object *e_set_clipboard(Object ** args, GC_PARAM)
{
	Object *first = (*args)->car;

	if (first->type != TYPE_STRING)
	    exceptionWithObject(first, "is not a string (set-clipboard)");

	/* gets freed by next call to set_clipboard */
	char *sub = strdup(first->string);
	set_scrap((unsigned char *)sub);
	return t;
}

Object *e_get_temp_file(Object ** args, GC_PARAM)
{
	char *fn = get_temp_file();
	return newStringWithLength(fn, strlen(fn), GC_ROOTS);	
}

Object *e_shell_command(Object ** args, GC_PARAM)
{
	Object *first = (*args)->car;

	if (first->type != TYPE_STRING)
	    exceptionWithObject(first, "is not a string (set-clipboard)");

	shell_command(first->string);
	return t;
}

Object *stringAppend(Object ** args, GC_PARAM)
{
	TWO_STRING_ARGS();

	int len1 = strlen(first->string);
	int len2 = strlen(second->string);
	char *new = strdup(first->string);
	new = realloc(new, len1 + len2 + 1);
	assert(new != NULL);
	memcpy(new + len1, second->string, len2);
	new[len1 + len2] = '\0';

	Object *obj = newStringWithLength(new, len1 + len2, GC_ROOTS);	
	free(new);

	return obj;
}

Object *stringRef(Object ** args, GC_PARAM)
{
	char ch[2];
	Object *first = (*args)->car;
	Object *second = (*args)->cdr->car;

	if (first->type != TYPE_STRING)
	    exceptionWithObject(first, "is not a string (string.ref)");
	if (second->type != TYPE_NUMBER)
	    exceptionWithObject(second, "is not a number (string.ref)");

	char *str = first->string;
	int ref = (int)(second->number);
	int len = strlen(str);

	if (ref < 0 || ref >= len)
	    exceptionWithObject(second, "is out of bounds (string.ref)");

	ch[0] = str[ref];
	ch[1] = '\0';

	return newStringWithLength(ch, 1, GC_ROOTS);
}

Object *stringSubstring(Object ** args, GC_PARAM)
{
	Object *first = (*args)->car;
	Object *second = (*args)->cdr->car;
	Object *third = (*args)->cdr->cdr->car;

	if (first->type != TYPE_STRING)
	    exceptionWithObject(first, "is not a string (string.substring)");
	if (second->type != TYPE_NUMBER)
	    exceptionWithObject(second, "is not a number");  
	if (third->type != TYPE_NUMBER)
	    exceptionWithObject(third, "is not a number");  

	int start = (int)(second->number);
	int end = (int)(third->number);
	int len = strlen(first->string);

	if (start < 0 || start > len -1)
	    exceptionWithObject(second, "is out of bounds");
	if (end < 0 || end > len -1)
	    exceptionWithObject(third, "is out of bounds");
	if (start > end)
	    exceptionWithObject(second, "start index greater than end index");

	char *sub = strdup(first->string);
	int newlen = end - start + 1;

	memcpy(sub, (first->string + start), newlen);
	*(sub + newlen) = '\0';
	Object *obj = newStringWithLength(sub, newlen, GC_ROOTS);
	free(sub);

	return obj;
}

Object *stringLength(Object ** args, GC_PARAM)
{
	Object *first = (*args)->car;
	if (first->type != TYPE_STRING)
	    exceptionWithObject(first, "is not a string (string.length)");

	return newNumber(strlen(first->string), GC_ROOTS);	
}

Object *e_show_prompt(Object ** args, GC_PARAM)
{
	TWO_STRING_ARGS();
	display_prompt_and_response(first->string, second->string);
	return t;
}

Object *e_prompt(Object ** args, GC_PARAM)
{
	TWO_STRING_ARGS();

	char response[81];
	strncpy(response, second->string, 80);
	response[80] = '\0';

	(void) ! getinput(first->string, response, 80, F_NONE);
	return newStringWithLength(response, strlen(response), GC_ROOTS);
}

Object *primitiveStringQ(Object ** args, GC_PARAM)
{
	Object *first = (*args)->car;
	return (first != nil && first->type == TYPE_STRING) ? t : nil;
}

Object *primitiveNumberQ(Object ** args, GC_PARAM)
{
	Object *first = (*args)->car;
	return (first != nil && first->type == TYPE_NUMBER) ? t : nil;
}

Object *stringToNumber(Object ** args, GC_PARAM)
{
	Object *first = (*args)->car;

	if (first->type != TYPE_STRING)
	    exceptionWithObject(first, "is not a string");

	double num = strtod(first->string, NULL);
	return newNumber(num, GC_ROOTS);
}

/* 
 * XXX could be improved to handle integers and decimals better 
 * for example 121323.000000 (%f) is ugly but so is 1.213230e+05 (%g)
 */
Object *numberToString(Object ** args, GC_PARAM)
{
	char buf[40];
	Object *first = (*args)->car;

	if (first->type != TYPE_NUMBER)
	    exceptionWithObject(first, "is not a number");

	if (first->number == (long)first->number)
		sprintf(buf, "%ld", (long)first->number);
	else
		sprintf(buf, "%lf", first->number);

	return newStringWithLength(buf, strlen(buf), GC_ROOTS);
}

Object *os_getenv(Object ** args, GC_PARAM)
{
	ONE_STRING_ARG();

	char *e = getenv(first->string);
	if (e == NULL) return nil;
	return newStringWithLength(e, strlen(e), GC_ROOTS);
}

Object *e_get_version_string(Object ** args, GC_PARAM)
{
	char *ver = get_version_string();
	return newStringWithLength(ver, strlen(ver), GC_ROOTS);
}

Object *e_goto_line(Object ** args, GC_PARAM)
{
	Object *first = (*args)->car;

	if (first->type != TYPE_NUMBER)
	    exceptionWithObject(first, "is not a number");

	int result = goto_line(first->number);
	return (result == 1 ? t : nil);
}

Object *e_select_buffer(Object ** args, GC_PARAM)
{
	ONE_STRING_ARG();
	int result = select_buffer(first->string);
	return (result ? t : nil);
}

Object *e_rename_buffer(Object ** args, GC_PARAM)
{
	ONE_STRING_ARG();
	rename_current_buffer(first->string);
	char *bname = get_current_bufname();
	return newStringWithLength(bname, strlen(bname), GC_ROOTS);
}

Object *e_save_buffer(Object ** args, GC_PARAM)
{
	ONE_STRING_ARG();
	int result = save_buffer_byname(first->string);
	return (result ? t : nil);
}

Object *e_kill_buffer(Object ** args, GC_PARAM)
{
	ONE_STRING_ARG();
	int result = delete_buffer_byname(first->string);
	return (result ? t : nil);
}

Object *e_find_file(Object ** args, GC_PARAM)
{
	ONE_STRING_ARG();
	readfile(first->string);
	return t;
}

Object *e_search_forward(Object ** args, GC_PARAM)
{
	ONE_STRING_ARG();
	point_t founded = search_forward(first->string);
	move_to_search_result(founded);
	return (founded == -1 ? nil : t);
}

Object *e_search_backward(Object ** args, GC_PARAM)
{
	ONE_STRING_ARG();
	point_t founded = search_backwards(first->string);
	move_to_search_result(founded);
	return (founded == -1 ? nil : t);
}

Object *e_getch(Object ** args, GC_PARAM)
{
	char ch[2];
	ch[0] = (unsigned char)getch();
	ch[1] = '\0';
	return newStringWithLength(ch, 1, GC_ROOTS);
}

Object *e_get_buffer_name(Object ** args, GC_PARAM)
{
	char buf[20];
	strcpy(buf, get_current_bufname());
	return newStringWithLength(buf, strlen(buf), GC_ROOTS);
}

Object *asciiToString(Object ** args, GC_PARAM)
{
	char ch[2];
	Object *first = (*args)->car;

	if (first->type != TYPE_NUMBER)
	    exceptionWithObject(first, "is not a number");
	if (first->type < 0 || first->type > 255)
	    exceptionWithObject(first, "is not in range 0-255");

	ch[0] = (unsigned char)first->number;
	ch[1] = '\0';
	return newStringWithLength(ch, 1, GC_ROOTS);
}

Object *asciiToNumber(Object ** args, GC_PARAM)
{
	Object *first = (*args)->car;

	if (first->type != TYPE_STRING)
	    exceptionWithObject(first, "is not a number");
	if (strlen(first->string) < 1)
	    exceptionWithObject(first, "is empty");

	return newNumber((double)*first->string, GC_ROOTS);
}

char *load_file(int);

Object *e_load(Object ** args, GC_PARAM)
{
	int fd;
	char ebuf[81];
	Object *first = (*args)->car;
	if (first->type != TYPE_STRING)
	    exceptionWithObject(first, "is not a string");

	if ((fd = open(first->string, O_RDONLY)) == -1) {
		snprintf(ebuf, 80, "failed to open %s\n", first->string);
		ebuf[80] ='\0';
		writeString(ebuf, &ostream);
		close(fd);
		return nil;
	}

	char *out = load_file(fd);
	close(fd);
	return (NULL == strstr(out, "error:")) ? t : nil;
}

Object *e_message(Object ** args, GC_PARAM)
{
	ONE_STRING_ARG();
	msg(first->string);
	return t;
}

Object *e_log_message(Object ** args, GC_PARAM)
{
	ONE_STRING_ARG();
	log_message(first->string);
	return t;
}

Object *e_log_debug(Object ** args, GC_PARAM)
{
	ONE_STRING_ARG();
	debug(first->string);
	return t;
}

Object *e_insert_string(Object ** args, GC_PARAM)
{
	ONE_STRING_ARG();
	insert_string(first->string);
	return t;
}

extern void set_point(point_t);
extern point_t get_mark(void);
extern point_t get_point(void);
extern point_t get_point_max(void);

Object *e_set_point(Object ** args, GC_PARAM)
{
	Object *first = (*args)->car;
	if (first->type != TYPE_NUMBER)
	    exceptionWithObject(first, "is not a number");
	set_point(first->number);
	return t;
}

Object *e_get_mark(Object ** args, GC_PARAM)
{
	return newNumber(get_mark(), GC_ROOTS);
}

Object *e_get_point(Object ** args, GC_PARAM)
{
	return newNumber(get_point(), GC_ROOTS);
}

Object *e_get_point_max(Object ** args, GC_PARAM)
{
	return newNumber(get_point_max(), GC_ROOTS);
}

#define DEFINE_PRIMITIVE_ARITHMETIC(name, op, init)                          \
Object *name(Object **args, GC_PARAM) {                                      \
  if (*args == nil)                                                          \
    return newNumber(init, GC_ROOTS);                                        \
  else if ((*args)->car->type != TYPE_NUMBER)                                \
    exceptionWithObject((*args)->car, "is not a number");             \
  else {                                                                     \
    Object *object, *rest;                                                   \
                                                                             \
    if ((*args)->cdr == nil) {                                               \
      object = newNumber(init, GC_ROOTS);                                    \
      rest = *args;                                                          \
    } else {                                                                 \
      GC_TRACE(gcFirst, (*args)->car);                                       \
      object = newObjectFrom(gcFirst, GC_ROOTS);                             \
      rest = (*args)->cdr;                                                   \
    }                                                                        \
                                                                             \
    for (; rest != nil; rest = rest->cdr) {                                  \
      if (rest->car->type != TYPE_NUMBER)                                    \
        exceptionWithObject(rest->car, "is not a number");            \
                                                                             \
      object->number = object->number op rest->car->number;                  \
    }                                                                        \
                                                                             \
    return object;                                                           \
  }                                                                          \
}

Object *primitiveMod(Object **args, GC_PARAM) {
  if (*args == nil)
    return newNumber(1, GC_ROOTS);
  else if ((*args)->car->type != TYPE_NUMBER)
    exceptionWithObject((*args)->car, "is not a number");
  else {
    Object *object, *rest;

    if ((*args)->cdr == nil) {
      object = newNumber(1, GC_ROOTS);
      rest = *args;
    } else {
      GC_TRACE(gcFirst, (*args)->car);
      object = newObjectFrom(gcFirst, GC_ROOTS);
      rest = (*args)->cdr;
    }

    for (; rest != nil; rest = rest->cdr) {
      if (rest->car->type != TYPE_NUMBER)
        exceptionWithObject(rest->car, "is not a number");

      object->number = (int)object->number % (int)rest->car->number;
    }

    return object;
  }
}


DEFINE_PRIMITIVE_ARITHMETIC(primitiveAdd, +, 0)
DEFINE_PRIMITIVE_ARITHMETIC(primitiveSubtract, -, 0)
DEFINE_PRIMITIVE_ARITHMETIC(primitiveMultiply, *, 1)
DEFINE_PRIMITIVE_ARITHMETIC(primitiveDivide, /, 1)

#define DEFINE_PRIMITIVE_RELATIONAL(name, op)                                \
Object *name(Object **args, GC_PARAM) {                                      \
  if ((*args)->car->type != TYPE_NUMBER)                                     \
    exceptionWithObject((*args)->car, "is not a number");             \
  else {                                                                     \
    Object *rest = *args;                                                    \
    bool result = true;                                                      \
                                                                             \
    for (; result && rest->cdr != nil; rest = rest->cdr) {                   \
      if (rest->cdr->car->type != TYPE_NUMBER)                               \
        exceptionWithObject(rest->cdr->car, "is not a number");       \
                                                                             \
      result &= rest->car->number op rest->cdr->car->number;                 \
    }                                                                        \
                                                                             \
    return result ? t : nil;                                                 \
  }                                                                          \
}

DEFINE_PRIMITIVE_RELATIONAL(primitiveEqual, ==)
DEFINE_PRIMITIVE_RELATIONAL(primitiveLess, <)
DEFINE_PRIMITIVE_RELATIONAL(primitiveLessEqual, <=)
DEFINE_PRIMITIVE_RELATIONAL(primitiveGreater, >)
DEFINE_PRIMITIVE_RELATIONAL(primitiveGreaterEqual, >=)

typedef struct Primitive {
	char *name;
	int nMinArgs, nMaxArgs;
	Object *(*eval) (Object ** args, GC_PARAM);
} Primitive;

Primitive primitives[] = {
	{"quote", 1, 1 /* special form */ },
	{"setq", 0, -2 /* special form */ },
	{"progn", 0, -1 /* special form */ },
	{"if", 2, 3 /* special form */ },
	{"cond", 0, -1 /* special form */ },
	{"lambda", 1, -1 /* special form */ },
	{"macro", 1, -1 /* special form */ },
	{"atom", 1, 1, primitiveAtom},
	{"eq", 2, 2, primitiveEq},
	{"car", 1, 1, primitiveCar},
	{"cdr", 1, 1, primitiveCdr},
	{"cons", 2, 2, primitiveCons},
	{"print", 1, 1, primitivePrint},
	{"princ", 1, 1, primitivePrinc},
	{"+", 0, -1, primitiveAdd},
	{"-", 1, -1, primitiveSubtract},
	{"*", 0, -1, primitiveMultiply},
	{"/", 1, -1, primitiveDivide},
	{"%", 1, -1, primitiveMod},
	{"=", 1, -1, primitiveEqual},
	{"<", 1, -1, primitiveLess},
	{"<=", 1, -1, primitiveLessEqual},
	{">", 1, -1, primitiveGreater},
	{">=", 1, -1, primitiveGreaterEqual},
	{"number?", 1, 1, primitiveNumberQ},
	{"string?", 1, 1, primitiveStringQ},
	{"string.length", 1, 1, stringLength},
	{"string.append", 2, 2, stringAppend},
	{"string.substring", 3, 3, stringSubstring},
	{"string.ref", 2, 2, stringRef},
	{"string->number", 1, 1, stringToNumber},
	{"number->string", 1, 1, numberToString},
	{"ascii", 1, 1, asciiToString},
	{"ascii->number", 1, 1, asciiToNumber},
	{"load", 1, 1, e_load},
	{"os.getenv", 1, 1, os_getenv},
	{"get-temp-file", 0, 0, e_get_temp_file},

	{"add-mode-global", 1, 1, e_add_mode_global},
	{"message", 1, 1, e_message},
	{"log-message", 1, 1, e_log_message},
	{"log-debug", 1, 1, e_log_debug},
	{"insert-string", 1, 1, e_insert_string},
	{"set-point", 1, 1, e_set_point},
	{"get-mark", 0, 0, e_get_mark},
	{"get-point", 0, 0, e_get_point},
	{"get-point-max", 0, 0, e_get_point_max},
	{"set-key", 2, 2, e_set_key},
	{"prompt", 2, 2, e_prompt},
	{"show-prompt", 2, 2, e_show_prompt},
	{"eval-block", 0, 0, e_eval_block},
	{"get-buffer-name", 0, 0, e_get_buffer_name},
	{"get-char", 0, 0, e_get_char},
	{"get-key", 0, 0, e_get_key},
	{"get-key-name", 0, 0, e_get_key_name},
	{"get-key-funcname", 0, 0, e_get_key_funcname},
	{"goto-line", 1, 1, e_goto_line},
	{"getch", 0, 0, e_getch},
	{"get-version-string", 0, 0, e_get_version_string},
	{"save-buffer", 1, 1, e_save_buffer},
	{"search-forward", 1, 1, e_search_forward},
	{"search-backward", 1, 1, e_search_backward},
	{"select-buffer", 1, 1, e_select_buffer},
	{"rename-buffer", 1, 1, e_rename_buffer},
	{"kill-buffer", 1, 1, e_kill_buffer},
	{"find-file", 1, 1, e_find_file},
	{"update-display", 0, 0, e_update_display},
	{"clear-message-line", 0, 0, e_clear_message_line},
	{"refresh", 0, 0, e_refresh},

	{"beginning-of-buffer", 0, 0, e_beginning_of_buffer},
	{"end-of-buffer", 0, 0, e_end_of_buffer},
	{"beginning-of-line", 0, 0, e_lnbegin},
	{"end-of-line", 0, 0, e_lnend},
	{"forward-char", 0, 0, e_right},
	{"forward-page", 0, 0, e_forward_page},
	{"forward-word", 0, 0, e_forward_word},
	{"backward-char", 0, 0, e_left},
	{"backward-page", 0, 0, e_backward_page},
	{"backward-word", 0, 0, e_backward_word},
	{"next-line", 0, 0, e_down},
	{"previous-line", 0, 0, e_up},
	{"set-mark", 0, 0, e_set_mark},
	{"set-clipboard", 1, 1, e_set_clipboard},
	{"shell-command", 1, 1, e_shell_command},
	{"delete", 0, 0, e_delete},
	{"copy-region", 0, 0, e_copy_region},
	{"kill-region", 0, 0, e_kill_region},
	{"yank", 0, 0, e_yank},
	{"backspace", 0, 0, e_backspace},
	{"delete-other-windows", 0, 0, e_delete_other_windows},
	{"execute-key", 0, 0, e_execute_key},
	{"list-buffers", 0, 0, e_list_buffers},
	{"split-window", 0, 0, e_split_window},
	{"other-window", 0, 0, e_other_window},
	{"get-clipboard", 0, 0, e_get_clipboard},
	{"get-buffer-count", 0, 0, e_get_buffer_count},
	{"exit", 0, 0, e_quit}
};

// Special forms handled by evalExpr. Must be in the same order as above.
enum {
	PRIMITIVE_QUOTE,
	PRIMITIVE_SETQ,
	PRIMITIVE_PROGN,
	PRIMITIVE_IF,
	PRIMITIVE_COND,
	PRIMITIVE_LAMBDA,
	PRIMITIVE_MACRO
};

// EVALUATION /////////////////////////////////////////////////////////////////

/* Scheme-style tail recursive evaluation. evalProgn, evalIf and evalCond
 * return the object in the tail recursive position to be evaluated by
 * evalExpr. Macros are expanded in-place the first time they are evaluated.
 */

Object *evalExpr(Object ** object, Object ** env, GC_PARAM);

Object *evalSetq(Object ** args, Object ** env, GC_PARAM)
{
	if (*args == nil)
		return nil;
	else {
		GC_TRACE(gcVar, (*args)->car);
		GC_TRACE(gcVal, (*args)->cdr->car);

		if ((*gcVar)->type != TYPE_SYMBOL)
			exceptionWithObject(*gcVar, "is not a symbol");
		if (*gcVar == nil || *gcVar == t)
			exceptionWithObject(*gcVar, "is a constant and cannot be set");

		*gcVal = evalExpr(gcVal, env, GC_ROOTS);
		envSet(gcVar, gcVal, env, GC_ROOTS);

		if ((*args)->cdr->cdr == nil)
			return *gcVal;
		else {
			GC_TRACE(gcArgs, (*args)->cdr->cdr);
			return evalSetq(gcArgs, env, GC_ROOTS);
		}
	}
}

Object *evalProgn(Object ** args, Object ** env, GC_PARAM)
{
	if (*args == nil)
		return nil;
	else if ((*args)->cdr == nil)
		return (*args)->car;
	else {
		GC_TRACE(gcObject, (*args)->car);
		GC_TRACE(gcArgs, (*args)->cdr);

		evalExpr(gcObject, env, GC_ROOTS);
		return evalProgn(gcArgs, env, GC_ROOTS);
	}
}

Object *evalIf(Object ** args, Object ** env, GC_PARAM)
{
	GC_TRACE(gcObject, (*args)->car);

	if (evalExpr(gcObject, env, GC_ROOTS) != nil)
		return (*args)->cdr->car;
	else if ((*args)->cdr->cdr != nil)
		return (*args)->cdr->cdr->car;
	else
		return nil;
}

Object *evalCond(Object ** args, Object ** env, GC_PARAM)
{
	if (*args == nil)
		return nil;
	else if ((*args)->car->type != TYPE_CONS)
		exceptionWithObject((*args)->car, "is not a list");
	else {
		GC_TRACE(gcCar, (*args)->car->car);
		GC_TRACE(gcCdr, (*args)->car->cdr);

		if ((*gcCar = evalExpr(gcCar, env, GC_ROOTS)) != nil)
			return (*gcCdr != nil) ? evalProgn(gcCdr, env, GC_ROOTS) : *gcCar;
		else {
			GC_TRACE(gcArgs, (*args)->cdr);
			return evalCond(gcArgs, env, GC_ROOTS);
		}
	}
}

Object *evalLambda(Object ** args, Object ** env, GC_PARAM)
{
	GC_TRACE(gcParams, (*args)->car);
	GC_TRACE(gcBody, (*args)->cdr);

	return newLambda(gcParams, gcBody, env, GC_ROOTS);
}

Object *evalMacro(Object ** args, Object ** env, GC_PARAM)
{
	GC_TRACE(gcParams, (*args)->car);
	GC_TRACE(gcBody, (*args)->cdr);

	return newMacro(gcParams, gcBody, env, GC_ROOTS);
}

Object *expandMacro(Object ** macro, Object ** args, GC_PARAM)
{
	GC_TRACE(gcEnv, newEnv(macro, args, GC_ROOTS));
	GC_TRACE(gcBody, (*macro)->body);

	GC_TRACE(gcObject, evalProgn(gcBody, gcEnv, GC_ROOTS));
	*gcObject = evalExpr(gcObject, gcEnv, GC_ROOTS);

	return *gcObject;
}

Object *expandMacroTo(Object ** macro, Object ** args, Object ** cons, GC_PARAM)
{
	GC_TRACE(gcObject, expandMacro(macro, args, GC_ROOTS));

	if ((*gcObject)->type == TYPE_CONS) {
		(*cons)->car = (*gcObject)->car;
		(*cons)->cdr = (*gcObject)->cdr;
	} else {
		(*cons)->car = newSymbol("progn", GC_ROOTS);
		(*cons)->cdr = newCons(gcObject, &nil, GC_ROOTS);
	}

	return *cons;
}

Object *evalList(Object ** args, Object ** env, GC_PARAM)
{
	if ((*args)->type != TYPE_CONS)
		return evalExpr(args, env, GC_ROOTS);
	else {
		GC_TRACE(gcObject, (*args)->car);
		GC_TRACE(gcArgs, (*args)->cdr);

		*gcObject = evalExpr(gcObject, env, GC_ROOTS);
		*gcArgs = evalList(gcArgs, env, GC_ROOTS);

		return newCons(gcObject, gcArgs, GC_ROOTS);
	}
}

Object *evalExpr(Object ** object, Object ** env, GC_PARAM)
{
	GC_TRACE(gcObject, *object);
	GC_TRACE(gcEnv, *env);

	GC_TRACE(gcFunc, nil);
	GC_TRACE(gcArgs, nil);
	GC_TRACE(gcBody, nil);

	for (;;) {
		if ((*gcObject)->type == TYPE_SYMBOL)
			return envLookup(*gcObject, *gcEnv);
		if ((*gcObject)->type != TYPE_CONS)
			return *gcObject;

		*gcFunc = (*gcObject)->car;
		*gcArgs = (*gcObject)->cdr;

		*gcFunc = evalExpr(gcFunc, gcEnv, GC_ROOTS);
		*gcBody = nil;

		if ((*gcFunc)->type == TYPE_LAMBDA) {
			*gcBody = (*gcFunc)->body;
			*gcArgs = evalList(gcArgs, gcEnv, GC_ROOTS);
			*gcEnv = newEnv(gcFunc, gcArgs, GC_ROOTS);
			*gcObject = evalProgn(gcBody, gcEnv, GC_ROOTS);
		} else if ((*gcFunc)->type == TYPE_MACRO) {
			*gcObject = expandMacroTo(gcFunc, gcArgs, gcObject, GC_ROOTS);
		} else if ((*gcFunc)->type == TYPE_PRIMITIVE) {
			Primitive *primitive = &primitives[(*gcFunc)->primitive];
			int nArgs = 0;

			for (Object * args = *gcArgs; args != nil; args = args->cdr, nArgs++)
				if (args->type != TYPE_CONS)
					exceptionWithObject(args, "is not a list");

			if (nArgs < primitive->nMinArgs)
				exceptionWithObject(*gcFunc, "expects at least %d arguments", primitive->nMinArgs);
			if (nArgs > primitive->nMaxArgs && primitive->nMaxArgs >= 0)
				exceptionWithObject(*gcFunc, "expects at most %d arguments", primitive->nMaxArgs);
			if (primitive->nMaxArgs < 0 && nArgs % -primitive->nMaxArgs)
				exceptionWithObject(*gcFunc, "expects a multiple of %d arguments", -primitive->nMaxArgs);

			switch ((*gcFunc)->primitive) {
			case PRIMITIVE_QUOTE:
				return (*gcArgs)->car;
			case PRIMITIVE_SETQ:
				return evalSetq(gcArgs, gcEnv, GC_ROOTS);
			case PRIMITIVE_PROGN:
				*gcObject = evalProgn(gcArgs, gcEnv, GC_ROOTS);
				break;
			case PRIMITIVE_IF:
				*gcObject = evalIf(gcArgs, gcEnv, GC_ROOTS);
				break;
			case PRIMITIVE_COND:
				*gcObject = evalCond(gcArgs, gcEnv, GC_ROOTS);
				break;
			case PRIMITIVE_LAMBDA:
				return evalLambda(gcArgs, gcEnv, GC_ROOTS);
			case PRIMITIVE_MACRO:
				return evalMacro(gcArgs, gcEnv, GC_ROOTS);
			default:
				*gcArgs = evalList(gcArgs, gcEnv, GC_ROOTS);
				return primitive->eval(gcArgs, GC_ROOTS);
			}
		} else {
			exceptionWithObject(*gcFunc, "is not a function");
		}
	}
}

// STANDARD LIBRARY ///////////////////////////////////////////////////////////

#define LISP(...) #__VA_ARGS__

static char *stdlib = LISP(
  (setq list (lambda args args))
  
  (setq defmacro (macro (name params . body)
    (list (quote setq) name (list (quote macro) params . body))))
  
  (defmacro defun (name params . body)
    (list (quote setq) name (list (quote lambda) params . body)))
  
  (defun null (x) (eq x nil))
  
  (defun map1 (func xs)
    (if (null xs)
        nil
        (cons (func (car xs))
              (map1 func (cdr xs)))))
  
  (defmacro and args
    (cond ((null args) t)
          ((null (cdr args)) (car args))
          (t (list (quote if) (car args) (cons (quote and) (cdr args))))))

  (defmacro or args 
    (if (null args)
        nil
        (cons (quote cond) (map1 list args))))

  (defun not (x) (if x nil t))

  (defun consp (x) (not (atom x)))
  (defun listp (x) (or (null x) (consp x)))

  (defun zerop (x) (= x 0))
  
  (defun equal (x y)
    (or (and (atom x) (atom y)
             (eq x y))
        (and (not (atom x)) (not (atom y))
             (equal (car x) (car y))
             (equal (cdr x) (cdr y)))))
  
  (defun nth (n xs)
    (if (zerop n)
        (car xs)
        (nth (- n 1) (cdr xs))))

  (defun append (xs y)
    (if (null xs)
        y
        (cons (car xs) (append (cdr xs) y))))
);

// MAIN ///////////////////////////////////////////////////////////////////////

Object *newRootEnv(GC_PARAM)
{
	GC_TRACE(gcEnv, newEnv(&nil, &nil, GC_ROOTS));
	GC_TRACE(gcVar, nil);
	GC_TRACE(gcVal, nil);

	// add constants
	envSet(&nil, &nil, gcEnv, GC_ROOTS);
	envSet(&t, &t, gcEnv, GC_ROOTS);

	// add primitives
	int nPrimitives = sizeof(primitives) / sizeof(primitives[0]);

	for (int i = 0; i < nPrimitives; ++i) {
		*gcVar = newSymbol(primitives[i].name, GC_ROOTS);
		*gcVal = newPrimitive(i, primitives[i].name, GC_ROOTS);

		envSet(gcVar, gcVal, gcEnv, GC_ROOTS);
	}

	// add standard library
	Stream stream = { STREAM_TYPE_STRING,.buffer = stdlib };
	GC_TRACE(gcObject, nil);

	while (peekNext(&stream) != EOF) {
		*gcObject = nil;
		*gcObject = readExpr(&stream, GC_ROOTS);
		evalExpr(gcObject, gcEnv, GC_ROOTS);
	}

	return *gcEnv;
}

Object *theRoot;
Object temp_root;
Object **theEnv;

void set_stream_file(Stream *stream, int fd)
{
	assert(stream != NULL);
	stream->type = STREAM_TYPE_FILE;
	stream->fd = fd;
}

void set_input_stream_buffer(Stream *stream, char *buffer)
{
	assert(stream != NULL);
	assert(buffer != NULL);
	assert(strlen(buffer) > 0);

	stream->type = STREAM_TYPE_STRING;
	stream->buffer = buffer;
	stream->length = strlen(buffer);
	stream->capacity = strlen(buffer);
	stream->offset = 0;
	stream->size = 0;
}

void reset_output_stream()
{
	Stream *stream = &ostream; /* we only want 1 output stream */
	stream->type = STREAM_TYPE_STRING;
	stream->length = 0;
	if (stream->buffer != NULL) {
		free(stream->buffer);
		stream->buffer = NULL;
	}
}

void load_file_body(Object ** env, GC_PARAM, Stream *input_stream)
{
	//debug("load_file_body\n");
	GC_TRACE(gcObject, nil);

	if (setjmp(exceptionEnv))
		return;

	while (peekNext(input_stream) != EOF) {
		*gcObject = nil;
		*gcObject = readExpr(input_stream, GC_ROOTS);
		*gcObject = evalExpr(gcObject, theEnv, GC_ROOTS);
		writeObject(*gcObject, true, &ostream);
		writeChar('\n', &ostream);
	}
}

void call_lisp_body(Object ** env, GC_PARAM, Stream *input_stream)
{
	GC_TRACE(gcObject, nil);

	for (;;) {
		if (setjmp(exceptionEnv))
			return;

		*gcObject = nil;

		if (peekNext(input_stream) == EOF) {
			writeChar('\n', &ostream);
			return;
		}

		*gcObject = readExpr(input_stream, GC_ROOTS);
		*gcObject = evalExpr(gcObject, env, GC_ROOTS);
		writeObject(*gcObject, true, &ostream);
		writeChar('\n', &ostream);
	}
}

/*
 * 3 interface functions that enable lisp to be embedded in an application (eg Editor)
 */
int init_lisp()
{
	theRoot = nil;

	//set_stream_file(&istream, STDIN_FILENO);
	set_stream_file(&ostream, STDOUT_FILENO);

	if (setjmp(exceptionEnv))
		return 1;

	symbols = nil;
	symbols = newCons(&nil, &symbols, theRoot);
	symbols = newCons(&t, &symbols, theRoot);

	temp_root.type = TYPE_CONS;
	temp_root.car = newRootEnv(theRoot);
	temp_root.cdr = theRoot;

	theEnv = &temp_root.car;
	theRoot = &temp_root;
	return 0;
}

char *call_lisp(char *input)
{
	assert(input != NULL);
	Stream is = { .type = STREAM_TYPE_STRING };

	//debug("START: call_lisp() '%s'\n", input);
	set_input_stream_buffer(&is, input);
	call_lisp_body(theEnv, theRoot, &is);
	//debug("END: call_lisp() '%s'\n", input);
	return ostream.buffer;
}

char *load_file(int infd)
{
	//debug("START: load_file fd=%d\n", infd);
	Stream input_stream = { .type = STREAM_TYPE_FILE, .fd = -1 };
	set_stream_file(&input_stream, infd);
	load_file_body(theEnv, theRoot, &input_stream);
	//debug("END: load_file fd=%d\n", infd);
	return ostream.buffer;
}
