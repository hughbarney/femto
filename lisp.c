/*
 * fLisp - a tiny yet practical Lisp interpreter.
 *
 * Based on Tiny-Lisp: https://github.com/matp/tiny-lisp
 *
 *   public:
 *   lisp_init() .. initialize the interpreter
 *   lisp_eval() .. evaluate interpreter input stream
 *   lisp_eval_string() .. evaluate a string
 *   file_fopen()  .. create lisp stream object from file
 *   file_fclose() .. close file associated with lisp stream object
 */

#include <sys/mman.h>
#include <sys/stat.h>
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <curses.h>
#include <limits.h>

// Note: can this be untangled?
#ifdef FLISP_FEMTO_EXTENSION
#include "header.h"
#endif

#include "lisp.h"

#define F_NONE          0
#define F_CLEAR         1
typedef long point_t;

#if !defined(MAP_ANONYMOUS) && defined(MAP_ANON)
#define MAP_ANONYMOUS        MAP_ANON
#endif

#define CPP_XSTR(s) CPP_STR(s)
#define CPP_STR(s) #s


Object *nil = &(Object) { TYPE_SYMBOL,.string = "nil" };
Object *t = &(Object) { TYPE_SYMBOL,.string = "t" };
Object *e = &(Object) { TYPE_STRING,.string = "\0" };
Object *one = &(Object) { TYPE_NUMBER,.number = 1 };

/* List of interpreters */
Interpreter *lisp_interpreters = NULL;

// Note: remove after rewriting the whole fLisp core to pass the
// interpreter instead of GC_PARAM
static Interpreter *interp = 0;


// DEBUG LOG ///////////////////////////////////////////////////////////////////

#ifdef __GNUC__
void fl_debug(Interpreter *, char *format, ...)
    __attribute__ ((format(printf, 2, 3)));
#endif
/** fl_debug() - fLisp debugger
 *
 * @param interp  Interpreter for which to send a debug message
 * @param format ...  printf() style debug string
 *
 * The format string is sent to the interpreters debug stream - if there is one.
 *
 * Be sure to file_fflush() the debug stream to get all text logged.
 *
 */
void fl_debug(Interpreter *interp, char *format, ...)
{
    if (interp->debug == nil)
        return;

    va_list(args);
    va_start(args, format);
    if (vfprintf(interp->debug->fd, format, args) < 0) {
        va_end(args);
        (void)fprintf(interp->debug->fd, "fatal: failed to print debug message: %d, %s", errno, format);
    }
    va_end(args);
    fputc('\n', interp->debug->fd);
}


// EXCEPTION HANDLING /////////////////////////////////////////////////////////

void resetBuf(Interpreter *);

/** exceptionWithObject - break out of errors
 *
 * @param interp  interpreter in which the error occured.
 * @param object  object on which an error occured, set to nil if none.
 * @param result  result code corresponding to error type, FLISP_ERROR for general errors.
 * @param format ... printf style error string
 *
 * *object* and *result* are stored in the interpreter structure.
 * The return code for longjmp is FLISP_ERROR
 *
 * The error message is formatted into the message buffer of the interpreter. If it has to
 * be truncated the last three characters are overwritten with "..."
 *
 * Note: if an exception handler is registered in the interpreter,
 *    control is returned to it.  Otherwise the function
 *    "falls trough". This is used in file_fopen() and file_fclose(),
 *    so they can be used w/o an exception handler.
 */
#ifdef __GNUC__
void exceptionWithObject(Interpreter *, Object * object, ResultCode, char *format, ...)
    __attribute__ ((format(printf, 4, 5)));
#endif
void exceptionWithObject(Interpreter *interp, Object * object, ResultCode result, char *format, ...)
{
    interp->object = object;
    interp->result = result;
    resetBuf(interp);

    int len = sizeof(interp->message);
    va_list(args);
    va_start(args, format);
    if (vsnprintf(interp->message, len, format, args) <0)
        strncpy(interp->message, "failed to format error message", len);
    va_end(args);
    if (snprintf(NULL, 0, format, args) > len)
        strcpy(interp->message+len-4, "...");

    if (interp->stackframe != NULL) {
        longjmp(*interp->stackframe, FLISP_ERROR);
        __builtin_unreachable();
    }
    // See note about "fall through" in comment above
}

/** exception - break out of errors
 *
 * @param interp  interpreter in which the error occurred.
 * @param result  result code corresponding to error type, FLISP_ERROR for general errors.
 *
 * *result* is stored in the interpreter structures result field, nil in the object field
 * The longjmp return code is FLISP_ERROR.
 *
 *
 * The error message is formatted into the message buffer of the interpreter. If it has to
 * be truncated the last three characters are overwritten with "..."
 */
#define exception(interp, result, ...)       exceptionWithObject(interp, nil, result, __VA_ARGS__)

// FILE_IO /////////////////////////////////////////////////////////////////////////////////

Object *file_outputMemStream(Interpreter *);

/** writeChar - write character to Lisp stream
 *
 * @param interp  fLisp interpreter
 * @param ch      character to write
 *
 * throws: FLISP_IO_ERROR
 */
void writeChar(Interpreter *interp, char ch)
{
    if (interp->output == nil)
        interp->output = file_outputMemStream(interp);

    if(fputc(ch, interp->output->fd) == EOF)
        exceptionWithObject(interp, interp->output, FLISP_IO_ERROR, "failed to write character %c, errno: %d", ch, errno);
}
/** writeString - write string to Lisp stream
 *
 * @param interp  fLisp interpreter
 * @param str     string to write
 *
 * throws: FLISP_IO_ERROR
 *
 */
void writeString(Interpreter *interp, char *str)
{
    int len;

    if (interp->output == nil)
        interp->output = file_outputMemStream(interp);

    len = strlen(str);
    if(fprintf(interp->output->fd, str) != len)
        exceptionWithObject(interp, interp->output, FLISP_IO_ERROR, "failed to write %d files, errno: %d", len, errno);
}
/** writeFmt - write printf formatted string to Lisp stream
 *
 * @param interp  fLisp interpreter
 * @param format ... printf like format string
 *
 * throws: FLISP_IO_ERROR
 */
#ifdef __GNUC__
void writeFmt(Interpreter *, char *format, ...)
    __attribute__ ((format(printf, 2, 3)));
#endif
void writeFmt(Interpreter *interp, char *format, ...)
{
    if (interp->output == nil)
        interp->output = file_outputMemStream(interp);

    va_list(args);
    va_start(args, format);
    if (vfprintf(interp->output->fd, format, args) < 0) {
        va_end(args);
        exceptionWithObject(interp, interp->output, FLISP_IO_ERROR, "failed to fprintf, errno: %d", errno);
    }
    va_end(args);
}

/** streamGetc - get a character from an interpreters input stream
 *
 * @param interp  fLisp interpreter
 *
 * returns: the next character from the stream, or EOF
 *
 * throws: FLISP_IO_ERROR
 */
int streamGetc(Interpreter *interp)
{
    int c;
    if ((c = fgetc(interp->input->fd)) == EOF)
        if (ferror(interp->input->fd))
            exceptionWithObject(interp, interp->input, FLISP_IO_ERROR, "failed to fgetc, errno: %d", errno);
    return c;
}
/** streamUngetc - push back the last streamGetc'd character to the interpreters input stream
 *
 * @param interp  fLisp interpreter
 *
 * returns: pushed back character or EOF on error
 *
 * throws: FLISP_IO_ERROR
 *
 */
int streamUngetc(Interpreter *interp, int c)
{
    if ((c = ungetc(c, interp->input->fd)) == EOF)
        exceptionWithObject(interp, interp->input, FLISP_IO_ERROR, "failed to ungetc, errno: %d", errno);
    return c;
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

#define GC_TRACE(name, init)                                            \
    Object GC_UNIQUE(GC_ROOTS) = { TYPE_CONS, .car = init, .cdr = GC_ROOTS }; \
    Object **name = &GC_UNIQUE(GC_ROOTS).car;                           \
    GC_ROOTS = &GC_UNIQUE(GC_ROOTS)

Object *gcMoveObject(Object * object)
{
    // skip object if it is not within from-space (i.e. on the stack)
    if (object < (Object *) interp->memory->fromSpace || object >= (Object *) ((char *)interp->memory->fromSpace + interp->memory->fromOffset))
        return object;

    // if the object has already been moved, return its new location
    if (object->type == TYPE_MOVED)
        return object->forward;

    // copy object to to-space
    Object *forward = (Object *) ((char *)interp->memory->toSpace + interp->memory->toOffset);
    memcpy(forward, object, object->size);
    interp->memory->toOffset += object->size;

    // mark object as moved and set forwarding pointer
    object->type = TYPE_MOVED;
    object->forward = forward;

    return object->forward;
}

void gc(GC_PARAM)
{
    fl_debug(interp, "collecting garbage");

    interp->memory->toOffset = 0;

    // move symbols and root objects
    interp->symbols = gcMoveObject(interp->symbols);

    for (Object * object = GC_ROOTS; object != nil; object = object->cdr)
        object->car = gcMoveObject(object->car);

    // iterate over objects in to-space and move all objects they reference
    for (Object * object = interp->memory->toSpace; object < (Object *) ((char *)interp->memory->toSpace + interp->memory->toOffset); object = (Object *) ((char *)object + object->size)) {

        switch (object->type) {
        case TYPE_NUMBER:
        case TYPE_STRING:
        case TYPE_SYMBOL:
        case TYPE_PRIMITIVE:
        case TYPE_STREAM:
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
        case TYPE_MOVED:
            exceptionWithObject(interp, object, FLISP_GC_ERROR, "object already moved");
            break;
        }
    }

    // swap from- and to-space
    void *swap = interp->memory->fromSpace;
    interp->memory->fromSpace = interp->memory->toSpace;
    interp->memory->toSpace = swap;
    interp->memory->fromOffset = interp->memory->toOffset;
}

// MEMORY MANAGEMENT //////////////////////////////////////////////////////////

size_t memoryAlign(size_t size, size_t alignment)
{
    return (size + alignment - 1) & ~(alignment - 1);
}

Object *memoryAllocObject(ObjectType type, size_t size, GC_PARAM)
{
    size = memoryAlign(size, sizeof(void *));

    // allocate from- and to-space
    if (!interp->memory->fromSpace) {
        if (!(interp->memory->fromSpace = mmap(NULL, interp->memory->capacity * 2, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0))) {
            if (!interp->stackframe) return nil;
            exception(interp, FLISP_OOM, "mmap() failed, %s", strerror(errno));
        }
        interp->memory->toSpace = (char *)interp->memory->fromSpace + interp->memory->capacity;
    }
    // run garbage collection if capacity exceeded
    if (interp->memory->fromOffset + size >= interp->memory->capacity)
        gc(GC_ROOTS);
    if (interp->memory->fromOffset + size >= interp->memory->capacity) {
        if (!interp->stackframe) return nil;
        exception(interp, FLISP_OOM, "out of memory, %lu bytes", (unsigned long)size);
    }
    // allocate object in from-space
    Object *object = (Object *) ((char *)interp->memory->fromSpace + interp->memory->fromOffset);
    object->type = type;
    object->size = size;
    interp->memory->fromOffset += size;

    return object;
}

// CONSTRUCTING OBJECTS ///////////////////////////////////////////////////////

Object *newObject(ObjectType type, GC_PARAM)
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

Object *newObjectWithString(ObjectType type, size_t size, GC_PARAM)
{
    size = (size > sizeof(((Object *) NULL)->string))
        ? size - sizeof(((Object *) NULL)->string)
        : 0;
    return memoryAllocObject(type, sizeof(Object) + size, GC_ROOTS);
}

Object *newStringWithLength(char *string, size_t length, GC_PARAM)
{
    int nEscapes = 0;

    if (length == 0)
        return e;

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
    for (Object * object = interp->symbols; object != nil; object = object->cdr)
        if (memcmp(object->car->string, string, length) == 0 && object->car->string[length] == '\0')
            return object->car;

    GC_TRACE(gcObject, newObjectWithString(TYPE_SYMBOL, length + 1, GC_ROOTS));
    memcpy((*gcObject)->string, string, length);
    (*gcObject)->string[length] = '\0';

    interp->symbols = newCons(gcObject, &interp->symbols, GC_ROOTS);

    return *gcObject;
}

Object *newSymbol(char *string, GC_PARAM)
{
    return newSymbolWithLength(string, strlen(string), GC_ROOTS);
}

Object *newObjectWithClosure(ObjectType type, Object ** params, Object ** body, Object ** env, GC_PARAM)
{
    Object *list;

    for (list = *params; list->type == TYPE_CONS; list = list->cdr) {
        if (list->car->type != TYPE_SYMBOL)
            exceptionWithObject(interp, list->car, FLISP_WRONG_TYPE, "(lambda|macro params body) - param is not a symbol");
        if (list->car == nil || list->car == t)
            exceptionWithObject(interp, list->car, FLISP_INVALID_VALUE, "(lambda|macro params body) - param cannot be used as a parameter");
    }

    if (list != nil && list->type != TYPE_SYMBOL)
        exceptionWithObject(interp, list, FLISP_WRONG_TYPE, "(lambda|macro params body) - param is not a symbol");

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
                exceptionWithObject(interp, val, FLISP_WRONG_TYPE, "(env) is not a list: val %d", nArgs);
            else if (param == nil && val != nil)
                exceptionWithObject(interp, *func, FLISP_PARAMETER_ERROR, "(env) expects at most %d arguments", nArgs);
            else if (param != nil && val == nil) {
                for (; param->type == TYPE_CONS; param = param->cdr, ++nArgs);
                exceptionWithObject(interp, *func, FLISP_PARAMETER_ERROR, "(env) expects at least %d arguments", nArgs);
            }
        }

        object->parent = (*func)->env;
        object->vars = (*func)->params;
        object->vals = *vals;
    }

    return object;
}
/**
 * @param fd .. FILE * stream descripter to register
 * @param name .. NULL or name of the file associated with fd
 * @param buf .. NULL or string to convert into an input file stream.
 */
Object *newStreamObject(FILE *fd, char *path, GC_PARAM)
{
    Object *object = newObject(TYPE_STREAM, GC_ROOTS);
    object->fd = fd;
    object->buf = NULL;
    if (path == NULL) {
        object->path = NULL;
    } else {
        object->path = newString(path, GC_ROOTS);
    }
    return object;
}


// READING S-EXPRESSIONS //////////////////////////////////////////////////////

/* Begin helpers */
int isSymbolChar(int ch)
{
    static const char *valid = "!#$%&*+-./:<=>?@^_~";
    return isalnum(ch) || strchr(valid, ch);
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

/** resetBuf - initialize interpreters read buffer
 *
 * @param interp  fLisp interpreter
 *
 */
void resetBuf(Interpreter *interp)
{
    free(interp->buf);
    interp->buf = NULL;
    interp->capacity = 0;
    interp->len = 0;
}
/** addCharToBuf - add a character to interpreters read buffer
 *
 * @param: interp  fLisp interpreter
 *
 * The read buffer is used to incrementally capture numbers, strings
 * or symbols from the input stream. It is cleared in the exception
 * handler.
 *
 * A parser first calls resetBuf(), then accumulates characters with
 * addCharToBuf() and frees the memory allocated for the read buffer
 * with resetBuf() after use.
 *
 * addCharToBuf() allocated memory in BUFSIZ chunks.
 */
size_t addCharToBuf(Interpreter *interp, int c)
{
    char *r;

    if (interp->len >= interp->capacity) {
        interp->capacity += BUFSIZ;
        if ((r = realloc(interp->buf, interp->capacity)) == NULL)
            exception(interp, FLISP_OOM, "failed to allocate %ld bytes for readString buffer", interp->capacity);
        interp->buf = r;
    }
    interp->buf[interp->len++] = c;
    return interp->len;
}
/** newInteger - add an integer from the read buffer to the
 *     interpreter
 *
 * @param interp  fLisp interpreter
 *
 * returns: number object
 *
 * throws: FLISP_READ_RANGE
 */
Object *newInteger(Interpreter *interp)
{
    Object *gcRoots = interp->theRoot; // gcRoots is needed by GC_TRACE and GC_ROOTS

    long l;
    Object *number;

    addCharToBuf(interp, '\0');
    errno = 0;
    l = strtol(interp->buf, NULL, 10);
    if (errno == ERANGE)
        exceptionWithObject(interp, interp->input, FLISP_READ_RANGE, "integer out of range,: %ld", l);
    number = newNumber(l, GC_ROOTS);
    resetBuf(interp);
    return number;
}
/** newDouble - add a float from the read buffer to the interpreter
 *
 * @param interp  fLisp interpreter
 *
 * returns: number object
 *
 * throws: FLISP_READ_RANGE
 */
Object *newDouble(Interpreter *interp)
{
    Object *gcRoots = interp->theRoot; // gcRoots is needed by GC_TRACE and GC_ROOTS

    double d;
    Object *number;

    addCharToBuf(interp, '\0');
    errno = 0;
    d = strtod(interp->buf, NULL);
    if (errno == ERANGE)
        exceptionWithObject(interp, interp->input, FLISP_READ_RANGE, "integer out of range,: %f", d);
    // Note: purposely not dealing with NaN
    number = newNumber(d, GC_ROOTS);
    resetBuf(interp);
    return number;
}

/* End helpers */

/** streamPeek - get the next character from the interpreter input stream, but stay at the current offset
 *
 * @param interp  fLisp interpreter
 *
 * returns: next character in stream or EOF
 *
 * throws: FLISP_IO_ERROR
 */
int streamPeek(Interpreter *interp)
{
    int c = streamGetc(interp);
    if (c != EOF)
        streamUngetc(interp, c);
    return c;
}

/** readNext - skip comments and spaces in interpreters input stream
 *
 * @param interp  fLisp interpreter
 *
 * returns: next not space, not comment character
 *
 * throws: FLISP_IO_ERROR
 */
int readNext(Interpreter *interp)
{
    for (;;) {
        int ch = streamGetc(interp);
        if (ch == ';')
            while ((ch = streamGetc(interp)) != EOF && ch != '\n');
        if (isspace(ch))
            continue;
        return ch;
    }
}
/** peekNext - skip to last space or comment character in interpreter input stream
 *
 * @param interp  fLisp interpreter
 *
 * returns: next not space, not comment character
 *
 * throws: FLISP_IO_ERROR
 */
int peekNext(Interpreter *interp)
{
    int c = readNext(interp);
    if (c != EOF)
        streamUngetc(interp, c);
    return c;
}
/** readWhile - skip to next charater not fullfilling a predicate in interpreter input stream
 *
 * @param interp     fLisp interpreter
 * @param predicate  function returning 0 if a character matches *predicate*
 *
 * returns: next character not fullfilling *predicate*
 *
 * throws: FLISP_IO_ERROR
 */
int readWhile(Interpreter *interp, int (*predicate) (int ch))
{
    for (;;) {
        int ch = streamPeek(interp);
        if (!predicate(ch))
            return ch;
        (void)addCharToBuf(interp, streamGetc(interp));
    }
}

/** readString - return string object from interpreter input stream
 *
 * @param interp  fLisp interpreter
 *
 * throws: FLISP_IO_ERROR, FLISP_READ_INCOMPLETE, FLISP_OOM
 */
Object *readString(Interpreter *interp)
{
    Object *gcRoots = interp->theRoot; // gcRoots is needed by GC_TRACE and GC_ROOTS

    int ch;
    Object *string;

    resetBuf(interp);

    for (bool isEscaped = false;;) {
        ch = streamGetc(interp);
        if (ch == EOF) {
            exceptionWithObject(interp, interp->input, FLISP_READ_INCOMPLETE, "unexpected end of stream in string literal");
        }
        if (ch == '"' && !isEscaped) {
            string = newStringWithLength(interp->buf, interp->len, GC_ROOTS);
            resetBuf(interp);
            return string;
        }
        isEscaped = (ch == '\\' && !isEscaped);
        (void)addCharToBuf(interp, ch);
    }
}

/** readNumberOrSymbol - return integer, float or symbol from
 *      interpreter input stream
 *
 * @param interp  fLisp interpreter
 *
 * returns: number object
 *
 * throws: FLISP_IO_ERROR, FLISP_READ_INCOMPLETE, FLISP_READ_RANGE, FLISP_OOM
 */
Object *readNumberOrSymbol(Interpreter *interp) {

    Object *gcRoots = interp->theRoot; // gcRoots is needed by GC_TRACE and GC_ROOTS

    int ch = streamPeek(interp);

    resetBuf(interp);

    // skip optional leading sign
    if (ch == '+' || ch == '-') {
        (void)addCharToBuf(interp, streamGetc(interp));
        ch = streamPeek(interp);
    }
    // try to read a number in integer or decimal format
    if (ch == '.' || isdigit(ch)) {
        if (isdigit(ch))
            ch = readWhile(interp, isdigit);
        if (!isSymbolChar(ch))
            return newInteger(interp);
        if (ch == '.') {
            ch = streamGetc(interp);
            if (isdigit(streamPeek(interp))) {
                ch = readWhile(interp, isdigit);
                if (!isSymbolChar(ch))
                    return newDouble(interp);
            }
        }
    }
    // non-numeric character encountered, read a symbol
    readWhile(interp, isSymbolChar);
    Object * obj = newSymbolWithLength(interp->buf, interp->len, GC_ROOTS);
    resetBuf(interp);
    return obj;
}

Object *readExpr(Interpreter *);
/** readList - return list from interpreter input stream
 *
 * @param interp  fLisp interpreter
 *
 * returns: list
 *
 * throws: FLISP_IO_ERROR, FLISP_READ_INCOMPLETE, FLISP_READ_RANGE, FLISP_OOM
 */
Object *readList(Interpreter *interp)
{
    Object *gcRoots = interp->theRoot; // gcRoots is needed by GC_TRACE and GC_ROOTS

    GC_TRACE(gcList, nil);
    GC_TRACE(gcLast, nil);

    for (;;) {
        int ch = readNext(interp);
        if (ch == EOF)
            exceptionWithObject(interp, interp->input, FLISP_READ_INCOMPLETE, "unexpected end of stream in list");
        else if (ch == ')')
            return reverseList(*gcList);
        else if (ch == '.' && !isSymbolChar(streamPeek(interp))) {
            if (*gcLast == nil)
                exceptionWithObject(interp, interp->input, FLISP_READ_INVALID, "unexpected dot at start of list");
            if ((ch = peekNext(interp)) == ')')
                exceptionWithObject(interp, interp->input, FLISP_READ_INVALID, "expected object at end of dotted list");
            if (!(*gcLast = readExpr(interp)))
                exceptionWithObject(interp, interp->input, FLISP_READ_INCOMPLETE, "unexpected end of stream in dotted list");
            if ((ch = peekNext(interp)) != ')')
                exceptionWithObject(interp, interp->input, FLISP_READ_INVALID, "unexpected object at end of dotted list");
            readNext(interp);
            Object *list = reverseList(*gcList);
            (*gcList)->cdr = *gcLast;

            return list;
        } else {
            streamUngetc(interp, ch);
            *gcLast = readExpr(interp);
            *gcList = newCons(gcLast, gcList, GC_ROOTS);
        }
    }
}
/** readUnary - return an unary operator together with the next
 *     expression from interpreter input stream
 *
 * @param interp  fLisp interpreter
 * @param symbol  symbol to be inserted
 *
 * returns: unary operator expression
 *
 * throws: FLISP_IO_ERROR, FLISP_READ_INCOMPLETE, FLISP_READ_RANGE,
 *     FLISP_OOM
 */
Object *readUnary(Interpreter *interp, char *symbol)
{
    Object *gcRoots = interp->theRoot; // gcRoots is needed by GC_TRACE and GC_ROOTS

    if (peekNext(interp) == EOF)
        exceptionWithObject(interp, interp->input, FLISP_READ_INCOMPLETE, "unexpected end of stream in %s", symbol);

    GC_TRACE(gcSymbol, newSymbol(symbol, GC_ROOTS));
    GC_TRACE(gcObject, readExpr(interp));

    *gcObject = newCons(gcObject, &nil, GC_ROOTS);
    *gcObject = newCons(gcSymbol, gcObject, GC_ROOTS);

    return *gcObject;
}
/** readExpr - return next lisp sexp object from interpreter input stream
 *
 * @param interp  fLisp interpreter
 *
 * returns: sexp object
 *
 * throws: FLISP_IO_ERROR, FLISP_READ_INCOMPLETE, FLISP_READ_RANGE,
 *     FLISP_OOM
 */
Object *readExpr(Interpreter *interp)
{
    for (;;) {

        int ch = readNext(interp);

        if (ch == EOF)
            return NULL;
        else if (ch == '\'')
            return readUnary(interp, "quote");
        else if (ch == '"')
            return readString(interp);
        else if (ch == '(')
            return readList(interp);
        else if (isSymbolChar(ch) && (ch != '.' || isSymbolChar(streamPeek(interp)))) {
            (void)streamUngetc(interp, ch);
            return readNumberOrSymbol(interp);
        }
        else
            exception(interp, FLISP_READ_INVALID, "unexpected character, `%c'", ch);
    }
}


// WRITING OBJECTS ////////////////////////////////////////////////////////////

void writeObject(Interpreter *interp, Object *object, bool readably)
{
    switch (object->type) {
#define CASE(type, ...)                         \
        case type:                              \
            writeFmt(interp, __VA_ARGS__);      \
            break
        CASE(TYPE_NUMBER, "%g", object->number);
        CASE(TYPE_SYMBOL, "%s", object->string);
        CASE(TYPE_PRIMITIVE, "#<Primitive %s>", object->name);
        CASE(TYPE_STREAM, "#<Stream %p, %s>", (void *) object->fd, (object->fd == NULL) ? "<closed>" : object->path->string);
#undef CASE
    case TYPE_STRING:
        if (readably) {
            writeChar(interp, '"');
            for (char *string = object->string; *string; ++string) {
                switch (*string) {
                case '"':
                    writeString(interp, "\\\"");
                    break;
                case '\t':
                    writeString(interp, "\\t");
                    break;
                case '\r':
                    writeString(interp, "\\r");
                    break;
                case '\n':
                    writeString(interp, "\\n");
                    break;
                case '\\':
                    writeString(interp, "\\\\");
                    break;
                default:
                    writeChar(interp, *string);
                    break;
                }
            }
            writeChar(interp, '"');
        } else
            writeFmt(interp, "%s", object->string);
        break;
    case TYPE_CONS:
        writeChar(interp, '(');
        writeObject(interp, object->car, readably);
        while (object->cdr != nil) {
            object = object->cdr;
            if (object->type == TYPE_CONS) {
                writeChar(interp, ' ');
                writeObject(interp, object->car, readably);
            } else {
                writeString(interp, " . ");
                writeObject(interp, object, readably);
                break;
            }
        }
        writeChar(interp, ')');
        break;
#define CASE(type, name, object)                    \
        case type:                                  \
            writeFmt(interp, "#<%s ", name);        \
            writeObject(interp, object, readably);  \
            writeChar(interp, '>');                 \
            break
        CASE(TYPE_LAMBDA, "Lambda", object->params);
        CASE(TYPE_MACRO, "Macro", object->params);
        CASE(TYPE_ENV, "Env", object->vars);
#undef CASE
    case TYPE_MOVED:
        exception(interp, FLISP_GC_ERROR, "won't write a garbage collected item");
        break;
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

    exceptionWithObject(interp, var, FLISP_INVALID_VALUE, "has no value");
    __builtin_unreachable();
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

Object *primitiveNullP(Object **args, GC_PARAM)
{
    return ((*args)->car == nil) ? t : nil;
}

Object *primitiveConsP(Object ** args, GC_PARAM)
{
    return ((*args)->car->type == TYPE_CONS) ? t : nil;
}

Object *primitiveSymbolP(Object ** args, GC_PARAM)
{
    return ((*args)->car->type == TYPE_SYMBOL) ? t : nil;
}

Object *primitiveSymbolName(Object ** args, GC_PARAM)
{
    Object *first = (*args)->car;
    if (first->type !=  TYPE_SYMBOL)
        exceptionWithObject(interp, first, FLISP_WRONG_TYPE, "(symbolp arg) - arg is not a symbol");
    return newString(first->string, GC_ROOTS);
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

    exceptionWithObject(interp, first, FLISP_WRONG_TYPE, "(car arg) - arg must be a list");
    __builtin_unreachable();
}

Object *primitiveCdr(Object ** args, GC_PARAM)
{
    Object *first = (*args)->car;

    if (first == nil)
        return nil;
    else if (first->type == TYPE_CONS)
        return first->cdr;

    exceptionWithObject(interp, first, FLISP_WRONG_TYPE, "(cdr arg) - arg must be a list");
    __builtin_unreachable();
}

Object *primitiveCons(Object ** args, GC_PARAM)
{
    GC_TRACE(gcFirst, (*args)->car);
    GC_TRACE(gcSecond, (*args)->cdr->car);

    return newCons(gcFirst, gcSecond, GC_ROOTS);
}

Object *fl_primitivePrinc(Interpreter *interp, Object ** args)
{
    assert(interp != NULL && args != NULL);
    writeObject(interp, (*args)->car, false);
    return (*args)->car;
}
Object *primitivePrinc(Object **args, GC_PARAM) { return fl_primitivePrinc(interp, args); }

Object *fl_primitivePrint(Interpreter *interp, Object ** args)
{
    assert(interp != NULL && args != NULL);
    writeChar(interp, '\n');
    writeObject(interp, (*args)->car, true);
    writeChar(interp, ' ');
    return (*args)->car;
}
Object *primitivePrint(Object ** args, GC_PARAM) { return fl_primitivePrint(interp, args); }

Object *primitiveSignal(Object ** args, GC_PARAM)
{
    Object *first = (*args)->car;
    Object *second = (*args)->cdr->car;

    if (first->type != TYPE_SYMBOL)
        exceptionWithObject(interp, first , FLISP_WRONG_TYPE, "(signal type data) - type is not a symbol");
    if (second != nil && second->type != TYPE_CONS)
        exceptionWithObject(interp, second, FLISP_WRONG_TYPE, "(signal type data) - data is not a list");

    GC_TRACE(e, newCons(&first, &second, GC_ROOTS));
    exceptionWithObject(interp, *e, FLISP_USER, first->string);
    return *e;
}

#define DEFINE_PRIMITIVE_ARITHMETIC(name, op, init)                     \
    Object *name(Object **args, GC_PARAM) {                             \
        if (*args == nil)                                               \
            return newNumber(init, GC_ROOTS);                           \
        if ((*args)->car->type == TYPE_NUMBER) {                        \
            Object *object, *rest;                                      \
                                                                        \
            if ((*args)->cdr == nil) {                                  \
                object = newNumber(init, GC_ROOTS);                     \
                rest = *args;                                           \
            } else {                                                    \
                GC_TRACE(gcFirst, (*args)->car);                        \
                object = newObjectFrom(gcFirst, GC_ROOTS);              \
                rest = (*args)->cdr;                                    \
            }                                                           \
                                                                        \
            for (; rest != nil; rest = rest->cdr) {                     \
                if (rest->car->type != TYPE_NUMBER)                     \
                    exceptionWithObject(interp, rest->car, FLISP_WRONG_TYPE, "(" CPP_XSTR(op) " number arg ..) - arg is not a number"); \
                                                                        \
                object->number = object->number op rest->car->number;   \
            }                                                           \
                                                                        \
            return object;                                              \
        }                                                               \
        exceptionWithObject(interp, (*args)->car, FLISP_WRONG_TYPE, "(" CPP_XSTR(op) " arg ..) - arg is not a number"); \
        __builtin_unreachable();                                        \
    }


DEFINE_PRIMITIVE_ARITHMETIC(primitiveAdd, +, 0)
DEFINE_PRIMITIVE_ARITHMETIC(primitiveSubtract, -, 0)
DEFINE_PRIMITIVE_ARITHMETIC(primitiveMultiply, *, 1)
DEFINE_PRIMITIVE_ARITHMETIC(primitiveDivide, /, 1)

Object *primitiveMod(Object **args, GC_PARAM) {
    if (*args == nil)
        return one;
    if ((*args)->car->type == TYPE_NUMBER) {
        Object *object, *rest;

        if ((*args)->cdr == nil) {
            object = one;
            rest = *args;
        } else {
            GC_TRACE(gcFirst, (*args)->car);
            object = newObjectFrom(gcFirst, GC_ROOTS);
            rest = (*args)->cdr;
        }

        for (; rest != nil; rest = rest->cdr) {
            if (rest->car->type != TYPE_NUMBER)
                exceptionWithObject(interp, rest->car, FLISP_WRONG_TYPE, "(%% dividend divisor ..) - divisor is not a number");

            object->number = (int)object->number % (int)rest->car->number;
        }

        return object;
    }
    exceptionWithObject(interp, (*args)->car, FLISP_WRONG_TYPE, "(%% dividend ..) - dividend is not a number");
    __builtin_unreachable();
}


#define DEFINE_PRIMITIVE_RELATIONAL(name, op)                           \
    Object *name(Object **args, GC_PARAM) {                             \
    if ((*args)->car->type == TYPE_NUMBER) {                            \
        Object *rest = *args;                                           \
        bool result = true;                                             \
                                                                        \
        for (; result && rest->cdr != nil; rest = rest->cdr) {          \
            if (rest->cdr->car->type != TYPE_NUMBER)                    \
                exceptionWithObject(interp, rest->cdr->car, FLISP_WRONG_TYPE, "(" CPP_XSTR(op) " number arg ..) - arg is not a number"); \
                                                                        \
            result &= rest->car->number op rest->cdr->car->number;      \
        }                                                               \
                                                                        \
        return result ? t : nil;                                        \
    }                                                                   \
    exceptionWithObject(interp, (*args)->car, FLISP_WRONG_TYPE, "(" CPP_XSTR(op) " arg ..) - arg is not a number"); \
    __builtin_unreachable();                                            \
}

DEFINE_PRIMITIVE_RELATIONAL(primitiveEqual, ==)
DEFINE_PRIMITIVE_RELATIONAL(primitiveLess, <)
DEFINE_PRIMITIVE_RELATIONAL(primitiveLessEqual, <=)
DEFINE_PRIMITIVE_RELATIONAL(primitiveGreater, >)
DEFINE_PRIMITIVE_RELATIONAL(primitiveGreaterEqual, >=)


#define TWO_STRING_ARGS(func)                                   \
    Object *first = (*args)->car;                               \
    Object *second = (*args)->cdr->car;                         \
    if (first->type != TYPE_STRING)                             \
        exceptionWithObject(interp, first, FLISP_WRONG_TYPE, "(" CPP_XSTR(func) "  first second) - first is not a string"); \
    if (second->type != TYPE_STRING)                            \
        exceptionWithObject(interp, second, FLISP_WRONG_TYPE, "(" CPP_XSTR(func) " first second) - second is not a string");

#define ONE_STRING_ARG(func)                                  \
    Object *arg = (*args)->car;                               \
    if (arg->type != TYPE_STRING)                             \
        exceptionWithObject(interp, arg, FLISP_WRONG_TYPE, "(" CPP_XSTR(func) " arg) - arg is not a string");

#define ONE_NUMBER_ARG(func)                                  \
    Object *num = (*args)->car;                               \
    if (num->type != TYPE_NUMBER)                             \
        exceptionWithObject(interp, num, FLISP_WRONG_TYPE, "(" CPP_XSTR(func) " num) - num is not a number");

#define ONE_STREAM_ARG(func)                                  \
    Object *fd = (*args)->car;                            \
    if (fd->type != TYPE_STREAM)                          \
        exceptionWithObject(interp, fd, FLISP_WRONG_TYPE, "(" CPP_XSTR(func) " fd) - fd is not a stream");

Object *fl_system(Object ** args, GC_PARAM) {

    ONE_STRING_ARG(system);
    return newNumber((double) system(arg->string), GC_ROOTS);
}

Object *stringAppend(Object ** args, GC_PARAM)
{
    TWO_STRING_ARGS(string.append);

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

Object *stringSubstring(Object ** args, GC_PARAM)
{
    Object *str = (*args)->car;
    Object *start = (*args)->cdr->car;
    Object *end = (*args)->cdr->cdr->car;

    if (str->type != TYPE_STRING)
        exceptionWithObject(interp, str, FLISP_WRONG_TYPE, "(string.substring str start end) str is not a string (string.substring)");
    if (start->type != TYPE_NUMBER)
        exceptionWithObject(interp, start, FLISP_WRONG_TYPE, "is not a number");
    if (end->type != TYPE_NUMBER)
        exceptionWithObject(interp, end, FLISP_WRONG_TYPE, "is not a number");

    int s = (int)(start->number);
    int e = (int)(end->number);
    int len = strlen(str->string);

    if (s < 0 || s > len -1)
        exceptionWithObject(interp, start, FLISP_INVALID_VALUE, "is out of bounds");
    if (e < 0 || e > len -1)
        exceptionWithObject(interp, end, FLISP_INVALID_VALUE, "is out of bounds");
    if (s > e)
        exceptionWithObject(interp, start, FLISP_INVALID_VALUE, "start index greater than end index");

    char *sub = strdup(str->string);
    int newlen = e - s + 1;

    memcpy(sub, (str->string + s), newlen);
    *(sub + newlen) = '\0';
    Object *obj = newStringWithLength(sub, newlen, GC_ROOTS);
    free(sub);

    return obj;
}

Object *stringLength(Object ** args, GC_PARAM)
{
    ONE_STRING_ARG(string.length);

    return newNumber(strlen(arg->string), GC_ROOTS);
}

/* Strings */

Object *primitiveStringP(Object ** args, GC_PARAM)
{
    Object *first = (*args)->car;
    return (first != nil && first->type == TYPE_STRING) ? t : nil;
}

Object *primitiveNumberP(Object ** args, GC_PARAM)
{
    Object *first = (*args)->car;
    return (first != nil && first->type == TYPE_NUMBER) ? t : nil;
}

Object *stringToNumber(Object ** args, GC_PARAM)
{
    Object *first = (*args)->car;

    ONE_STRING_ARG(strint-to-number);

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

    ONE_NUMBER_ARG(number-to-string);

    if (num->number == (long)num->number)
        sprintf(buf, "%ld", (long)num->number);
    else
        sprintf(buf, "%lf", num->number);

    return newStringWithLength(buf, strlen(buf), GC_ROOTS);
}


Object *asciiToString(Object ** args, GC_PARAM)
{
    char ch[2];
    ONE_NUMBER_ARG(ascii)
    if (num->type < 0 || num->type > 255)
        exceptionWithObject(interp, num, FLISP_INVALID_VALUE, "(ascii num) - num is not in range 0-255");

    ch[0] = (unsigned char)num->number;
    ch[1] = '\0';
    return newStringWithLength(ch, 1, GC_ROOTS);
}

Object *asciiToNumber(Object ** args, GC_PARAM)
{
    ONE_STRING_ARG(ascii->number);

    if (strlen(arg->string) < 1)
        exceptionWithObject(interp, arg, FLISP_INVALID_VALUE, "(ascii->number string) - string is empty");

    return newNumber((double)*arg->string, GC_ROOTS);
}

Object *file_fopen(Interpreter *, char *, char *);
ResultCode lisp_eval(Interpreter *);
/** primitiveLoad - evaluate a file in interpeter
 *
 * @param arg  path to file or "-" to read from stdin
 *
 * returns: last object evaluated
 *
 * throws:
 * - FLISP_IO_ERROR if file cannot be closed
 * - any error from evaluating the input file
 *
 * *path* is opened for reading. The interpreters input stream is set
 * to the resulting stream object.
 *
 * After evaluating the new input stream the current input stream is restored.
 *
 */
Object *primitiveLoad(Object **args, GC_PARAM)
{
    ONE_STRING_ARG(load);

    Object *new, *current;
    ResultCode result;
    int err;

    fl_debug(interp, "(load \"%s\")", arg->string);

    if (!strcmp(arg->string, "-")) // Note: do we really want to read
                                   // stdin programatically like this?
        new = file_fopen(interp, "<0", "r");
    else
        new = file_fopen(interp, arg->string, "r");

    current = interp->input;
    interp->input = new;

    // eval file
    result = lisp_eval(interp);
    if (result)
        exceptionWithObject(interp, new, result, "failed to load file %s: %s", arg->string, interp->message);

    if ((err = file_fclose(interp, new)))
        exceptionWithObject(interp, new, FLISP_IO_ERROR, "failed to close input stream, errno: %d", err);

    interp->input = current;

    return interp->object;
}

/* OS interface */

Object *os_getenv(Object ** args, GC_PARAM)
{
    ONE_STRING_ARG(os.getenv);

    char *e = getenv(arg->string);
    if (e == NULL) return nil;
    return newStringWithLength(e, strlen(e), GC_ROOTS);
}

/* The minimal stream C-API for interpreter operation */

/** file_outputMemStream - create a memory based output stream
 *
 * @param interp  fLisp Interpreter
 *
 * returns: lisp stream object
 *
 * throws: FLISP_IO_ERROR
 *
 */
Object *file_outputMemStream(Interpreter *interp)
{
    FILE *fd;
    Object * stream;

    Object *gcRoots = interp->theRoot;

    stream =  newStreamObject(NULL, ">STREAM", GC_ROOTS);
    fd = open_memstream(&stream->buf, &stream->len);
    if (fd == NULL) return nil;
    stream->fd = fd;
    GC_TRACE(gcStream, stream);
    return *gcStream;
}

/** file_fopen() - returns a stream object for the interpreter
 *
 * @param interp  fLisp interpreter
 * @param path    path to a file to open, or string for memory input
 *   buffer, or "<num" / ">num" for file descriptor input / output.
 * @param mode    see fopen(3p). One of "r", "w", "a", "r+", "w+",
 *   "a+" plus optional "b" modifier, or "<" / ">" for memory input /
 *   output buffer.
 *
 * returns: lisp stream object
 *
 * throws: FLISP_IO_ERROR, FLISP_INVALID_VALUE
 *
 * Additionally a file associated with a string buffer can be created:
 *
 * If mode is "<", *path* is converted into a memory based stream
 * opened with mode "r". The file name of the stream is set to "<STRING".
 *
 * If mode is ">", a dynamic memory based stream is opened with mode
 * "w". The file name of the stream is set to ">STRING".
 *
 * If path is "<num" or ">num" the standard file descriptor with
 * number *num* is opened in "r" or "a" mode respectively and mode is
 * ignored.
 *
 */
Object *file_fopen(Interpreter *interp, char *path, char* mode) {
    FILE * fd;

    Object *gcRoots = interp->theRoot;

    GC_TRACE(gcStream, nil);

    if (strcmp("<", mode) == 0) {
        fd = fmemopen(path, strlen(path), "r");
        if (fd == NULL) {
            exception(interp, FLISP_IO_ERROR, "failed to convert string to input stream, errno: %d", errno);
            goto no_handler;
        }
        *gcStream = newStreamObject(fd, "<STRING", GC_ROOTS);
        return *gcStream;
    }
    if (strcmp(">", mode) == 0)
        return file_outputMemStream(interp);

    char c = path[0];
    if (c == '<' || c == '>') {
        char *end;
        errno = 0;
        long d = strtol(&path[1], &end, 0);
        if (errno || *end != '\0' || d < 0 || d > _POSIX_OPEN_MAX) {
            exception(interp, FLISP_INVALID_VALUE, "invalid I/O stream number: %s", &path[1]);
            goto no_handler;
        }
        fd = fdopen((int)d, c == '<' ? "r" : "a");
        if (fd == NULL) {
            exception(interp, FLISP_IO_ERROR, "failed to open I/O stream %ld for %s", d, c == '<' ? "reading" : "writing");
            goto no_handler;
        }
        *gcStream = newStreamObject(fd, path, GC_ROOTS);
        return *gcStream;
    }

    fd = fopen(path, mode);
    if (fd == NULL) {
        exception(interp, FLISP_IO_ERROR, "failed to open file '%s' with mode '%s', errno: %d", path, mode, errno);
        goto no_handler;
    }
    *gcStream = newStreamObject(fd, path, GC_ROOTS);
    return *gcStream;

    // Note: when called without an exception environment the exception() falls through and execution continues here.
no_handler:
    return nil;
}
/** (Fclose StreamObject) => flag
 *
 * @param StreamObject
 *
 * Close *StreamObject*, return nil on error, else t.
 *
 */
int file_fclose(Interpreter *interp, Object *stream)
{
    if (stream->fd == NULL) {
        exception(interp, FLISP_INVALID_VALUE, "(fclose arg) - stream arg already closed");
        // if we fall through exception
        return interp->result;
    }
    // Note: we thought we needed to do this: but we get segfaulted. why?
    //if (stream->buf != NULL)
    //    free(stream->buf);
    if (fclose(stream->fd) == EOF) return errno;
    stream->fd = NULL;
    return 0;
}
int file_fflush(Interpreter *interp, Object *stream)
{
    return (fflush(stream->fd) == EOF) ? errno : 0;
}
#ifdef FLISP_FILE_EXTENSION
#include "file.c"
#endif

#ifdef FLISP_FEMTO_EXTENSION
/************************* Editor Extensions **************************************/

#define DEFINE_EDITOR_FUNC(name)                \
    extern void name();                         \
    Object *e_##name(Object ** args, GC_PARAM)  \
    {                                           \
        name();                                 \
        return t;                               \
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
DEFINE_EDITOR_FUNC(suspend)
DEFINE_EDITOR_FUNC(quit)
DEFINE_EDITOR_FUNC(eval_block)
DEFINE_EDITOR_FUNC(delete_other_windows)
DEFINE_EDITOR_FUNC(list_buffers)
DEFINE_EDITOR_FUNC(describe_bindings)
DEFINE_EDITOR_FUNC(describe_functions)
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

Object *e_refresh(Object ** args, GC_PARAM)
{
    refresh();
    return t;
}

Object *e_set_key(Object ** args, GC_PARAM)
{
    TWO_STRING_ARGS(set-key);
    return (1 == set_key(first->string, second->string) ? t : nil);
}

Object *e_add_mode_global(Object ** args, GC_PARAM)
{
    ONE_STRING_ARG(add-mode-global);
    return (1 == add_mode_global(arg->string) ? t : nil);
}

Object *e_set_clipboard(Object ** args, GC_PARAM)
{
    ONE_STRING_ARG(set-clipboard);

    /* gets freed by next call to set_clipboard */
    char *sub = strdup(arg->string);
    set_scrap((unsigned char *)sub);
    return t;
}

Object *e_get_temp_file(Object ** args, GC_PARAM)
{
    char *fn = get_temp_file();
    return newStringWithLength(fn, strlen(fn), GC_ROOTS);
}

Object *e_insert_file(Object ** args, GC_PARAM) {

    // Note: want to give an optional modify flag, but then it segfaults
    int mflag;

    ONE_STRING_ARG(insert-file);

    mflag = (arg->cdr != nil && arg->cdr->car != nil);
    mflag = FALSE;

    return ((insert_file(arg->string, mflag) == TRUE) ? t : nil);
}

Object *e_getfilename(Object **args, GC_PARAM) {

    ONE_STRING_ARG(prompt-filename);

    if (FALSE == getfilename(arg->string, (char*) response_buf, NAME_MAX))
        return nil;

    return newString(response_buf, GC_ROOTS);
}

Object *e_show_prompt(Object ** args, GC_PARAM)
{
    TWO_STRING_ARGS(show-prompt);
    display_prompt_and_response(first->string, second->string);
    return t;
}

Object *e_prompt(Object ** args, GC_PARAM)
{
    TWO_STRING_ARGS(prompt);

    char response[81];
    strncpy(response, second->string, 80);
    response[80] = '\0';

    (void) ! getinput(first->string, response, 80, F_NONE);
    return newStringWithLength(response, strlen(response), GC_ROOTS);
}

Object *e_get_version_string(Object ** args, GC_PARAM)
{
    char *ver = get_version_string();
    return newStringWithLength(ver, strlen(ver), GC_ROOTS);
}

Object *e_goto_line(Object ** args, GC_PARAM)
{
    Object *arg = (*args)->car;

    ONE_NUMBER_ARG(goto-line);

    int result = goto_line(arg->number);
    return (result == 1 ? t : nil);
}

Object *e_select_buffer(Object ** args, GC_PARAM)
{
    ONE_STRING_ARG(select-buffer);
    // Note: select buffer always returns TRUE
    //  so it seems to be superfluous to test for the return value
    int result = select_buffer(arg->string);
    return (result ? t : nil);
}

Object *e_rename_buffer(Object ** args, GC_PARAM)
{
    ONE_STRING_ARG(rename-buffer);
    rename_current_buffer(arg->string);
    char *bname = get_current_bufname();
    return newStringWithLength(bname, strlen(bname), GC_ROOTS);
}

Object *e_save_buffer(Object ** args, GC_PARAM)
{
    ONE_STRING_ARG(save-buffer);
    int result = save_buffer_byname(arg->string);
    return (result ? t : nil);
}

Object *e_kill_buffer(Object ** args, GC_PARAM)
{
    ONE_STRING_ARG(kill-buffer);
    int result = delete_buffer_byname(arg->string);
    return (result ? t : nil);
}
Object *e_zero_buffer(Object ** args, GC_PARAM)
{
    assert(curbp != NULL);
    zero_buffer(curbp);
    return nil;
}

Object *e_find_file(Object ** args, GC_PARAM)
{
    ONE_STRING_ARG(find-file);
    readfile(arg->string);
    return t;
}

Object *e_search_forward(Object ** args, GC_PARAM)
{
    ONE_STRING_ARG(search-forward);
    point_t founded = search_forward(arg->string);
    move_to_search_result(founded);
    return (founded == -1 ? nil : t);
}

Object *e_search_backward(Object ** args, GC_PARAM)
{
    ONE_STRING_ARG(search-backward);
    point_t founded = search_backwards(arg->string);
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
Object *e_message(Object ** args, GC_PARAM)
{
    ONE_STRING_ARG(message);
    msg(arg->string);
    return t;
}

Object *e_log_message(Object ** args, GC_PARAM)
{
    ONE_STRING_ARG(log-message);
    log_message(arg->string);
    return t;
}

Object *e_log_debug(Object ** args, GC_PARAM)
{
    ONE_STRING_ARG(log-debug);
    fl_debug(interp, arg->string);
    return t;
}

Object *e_insert_string(Object ** args, GC_PARAM)
{
    ONE_STRING_ARG(insert-string);
    insert_string(arg->string);
    return t;
}

extern void set_point(point_t);
extern point_t get_mark(void);
extern point_t get_point(void);
extern point_t get_point_max(void);

Object *e_set_point(Object ** args, GC_PARAM)
{
    ONE_NUMBER_ARG(set-point)
    set_point(num->number);
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
#endif

typedef struct Primitive {
    char *name;
    int nMinArgs, nMaxArgs;
    Object *(*eval) (Object ** args, GC_PARAM);
} Primitive;

Primitive primitives[] = {
    {"quote", 1, 1 /* special form */ },
    {"setq", 0, -2 /* special form */ },
    {"progn", 0, -1 /* special form */ },
    {"cond", 0, -1 /* special form */ },
    {"lambda", 1, -1 /* special form */ },
    {"macro", 1, -1 /* special form */ },
    {"macroexpand-1", 1, 2 /* special form */ },
    {"null", 1, 1, primitiveNullP},
    {"consp", 1, 1, primitiveConsP},
    {"symbolp", 1, 1, primitiveSymbolP},
    {"symbol-name", 1, 1, primitiveSymbolName},
    {"eq", 2, 2, primitiveEq},
    {"car", 1, 1, primitiveCar},
    {"cdr", 1, 1, primitiveCdr},
    {"cons", 2, 2, primitiveCons},
    {"princ", 1, 1, primitivePrinc},
    {"print", 1, 1, primitivePrint},
    {"signal", 2, 2, primitiveSignal},
    {"+", 0, -1, primitiveAdd},
    {"-", 0, -1, primitiveSubtract},
    {"*", 0, -1, primitiveMultiply},
    {"/", 1, -1, primitiveDivide},
    {"%", 1, -1, primitiveMod},
    {"=", 1, -1, primitiveEqual},
    {"<", 1, -1, primitiveLess},
    {"<=", 1, -1, primitiveLessEqual},
    {">", 1, -1, primitiveGreater},
    {">=", 1, -1, primitiveGreaterEqual},
    {"numberp", 1, 1, primitiveNumberP},
    {"stringp", 1, 1, primitiveStringP},
    {"string.length", 1, 1, stringLength},
    {"string.append", 2, 2, stringAppend},
    {"string.substring", 3, 3, stringSubstring},
    {"string-to-number", 1, 1, stringToNumber},
    {"number-to-string", 1, 1, numberToString},
    {"ascii", 1, 1, asciiToString},
    {"ascii->number", 1, 1, asciiToNumber},
    {"load", 1, 1, primitiveLoad},
    {"os.getenv", 1, 1, os_getenv},
    {"system", 1, 1, fl_system},
    FLISP_REGISTER_FILE_EXTENSION
#ifdef FLISP_FEMTO_EXTENSION
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
    {"insert-file-contents-literally", 1, 2, e_insert_file},
    {"select-buffer", 1, 1, e_select_buffer},
    {"rename-buffer", 1, 1, e_rename_buffer},
    {"kill-buffer", 1, 1, e_kill_buffer},
    {"erase-buffer", 0, 0, e_zero_buffer},
    {"find-file", 1, 1, e_find_file},
    {"update-display", 0, 0, e_update_display},
    {"prompt-filename", 1, 1, e_getfilename},
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
    {"delete", 0, 0, e_delete},
    {"copy-region", 0, 0, e_copy_region},
    {"kill-region", 0, 0, e_kill_region},
    {"yank", 0, 0, e_yank},
    {"backspace", 0, 0, e_backspace},
    {"delete-other-windows", 0, 0, e_delete_other_windows},
    {"execute-key", 0, 0, e_execute_key},
    {"list-buffers", 0, 0, e_list_buffers},
    {"describe-bindings", 0, 0, e_describe_bindings},
    {"describe-functions", 0, 0, e_describe_functions},
    {"split-window", 0, 0, e_split_window},
    {"other-window", 0, 0, e_other_window},
    {"get-clipboard", 0, 0, e_get_clipboard},
    {"get-buffer-count", 0, 0, e_get_buffer_count},
    {"suspend", 0, 0, e_suspend},
    {"exit", 0, 0, e_quit}
#endif
};

// Special forms handled by evalExpr. Must be in the same order as above.
enum {
    PRIMITIVE_QUOTE,
    PRIMITIVE_SETQ,
    PRIMITIVE_PROGN,
    PRIMITIVE_COND,
    PRIMITIVE_LAMBDA,
    PRIMITIVE_MACRO,
    PRIMITIVE_MACROEXPAND
};

// EVALUATION /////////////////////////////////////////////////////////////////

/* Scheme-style tail recursive evaluation. evalProgn and evalCond
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
            exceptionWithObject(interp, *gcVar, FLISP_WRONG_TYPE, "(setq name value) - name is not a symbol");
        if (*gcVar == nil || *gcVar == t)
            exceptionWithObject(interp, *gcVar, FLISP_WRONG_TYPE, "(setq name value) name is a constant and cannot be set");

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

    if ((*args)->type != TYPE_CONS)
        exceptionWithObject(interp, *args, FLISP_WRONG_TYPE, "(progn args) args is not a list");

    if ((*args)->cdr == nil)
        return (*args)->car;

    GC_TRACE(gcObject, (*args)->car);
    GC_TRACE(gcArgs, (*args)->cdr);

    evalExpr(gcObject, env, GC_ROOTS);
    return evalProgn(gcArgs, env, GC_ROOTS);
}

Object *evalCond(Object ** args, Object ** env, GC_PARAM)
{
    if (*args == nil) return nil;

    Object *clause = (*args)->car;

    if (clause == nil)
        goto cond_cdr;

    if (clause->type != TYPE_CONS)
        exceptionWithObject(interp, clause, FLISP_WRONG_TYPE, "(cond clause ..) - is not a list: clause");

    Object *action = clause->cdr;
    // (p v) => (p . (v . nil))
    // (p . nil) = (p)
    // (p . v) x
    if (action != nil && action->type != TYPE_CONS)
        exceptionWithObject(interp, clause, FLISP_WRONG_TYPE, "(cond (pred action) ..) action is not a list");

    Object *pred = clause->car;
    if (pred == nil)
        goto cond_cdr;

    GC_TRACE(gcPred, pred);
    if ((*gcPred = evalExpr(gcPred, env, GC_ROOTS)) == nil)
        goto cond_cdr;

    if (clause->cdr == nil)
        return *gcPred;

    GC_TRACE(gcCdr, clause->cdr);
    if ((*gcCdr)->type == TYPE_CONS)
        return evalProgn(gcCdr, env, GC_ROOTS);
    return evalExpr(gcCdr, env, GC_ROOTS);

cond_cdr:
    if ((*args)->cdr == nil) return nil;

    GC_TRACE(gcArgs, (*args)->cdr);
    return evalCond(gcArgs, env, GC_ROOTS);
}

Object *evalLambda(Object ** args, Object ** env, GC_PARAM)
{
    GC_TRACE(gcParams, (*args)->car);
    GC_TRACE(gcBody, (*args)->cdr);

    return newLambda(gcParams, gcBody, env, GC_ROOTS);
}

Object *evalMacro(Object ** args, Object ** env, GC_PARAM)
{
    // Note: needed? (see primitives[])
    assert((*args) == nil || (*args)->type == TYPE_CONS);

    GC_TRACE(gcParams, (*args)->car);
    GC_TRACE(gcBody, (*args)->cdr);

    return newMacro(gcParams, gcBody, env, GC_ROOTS);
}

Object *expandMacro(Object ** macro, Object ** args, GC_PARAM)
{
    assert((*args) == nil || (*args)->type == TYPE_CONS);

    GC_TRACE(gcEnv, newEnv(macro, args, GC_ROOTS));
    GC_TRACE(gcBody, (*macro)->body);

    assert((*gcBody)->type == TYPE_CONS);
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

Object *evalMacroExpand(Object **args, Object ** env, GC_PARAM)
{
    if ((*args)->type != TYPE_CONS)
        return evalExpr(args, env, GC_ROOTS);

    GC_TRACE(gcMacro, evalExpr(&(*args)->car, env, GC_ROOTS));
    if ((*gcMacro)->type != TYPE_MACRO)
        return (*args)->car;

    GC_TRACE(gcParams, (*args)->cdr);
    return expandMacro(gcMacro, gcParams, GC_ROOTS);
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
            assert((*gcBody)->type == TYPE_CONS);
            *gcObject = evalProgn(gcBody, gcEnv, GC_ROOTS);
        } else if ((*gcFunc)->type == TYPE_MACRO) {
            *gcObject = expandMacroTo(gcFunc, gcArgs, gcObject, GC_ROOTS);
        } else if ((*gcFunc)->type == TYPE_PRIMITIVE) {
            Primitive *primitive = &primitives[(*gcFunc)->primitive];
            int nArgs = 0;

            for (Object * args = *gcArgs; args != nil; args = args->cdr, nArgs++)
                if (args->type != TYPE_CONS)
                    exceptionWithObject(interp, args, FLISP_WRONG_TYPE, "(%s args) - args is not a list: arg %d", primitive->name, nArgs);

            if (nArgs < primitive->nMinArgs)
                exceptionWithObject(interp, *gcFunc, FLISP_PARAMETER_ERROR, "expects at least %d arguments", primitive->nMinArgs);
            if (nArgs > primitive->nMaxArgs && primitive->nMaxArgs >= 0)
                exceptionWithObject(interp, *gcFunc, FLISP_PARAMETER_ERROR, "expects at most %d arguments", primitive->nMaxArgs);
            if (primitive->nMaxArgs < 0 && nArgs % -primitive->nMaxArgs)
                exceptionWithObject(interp, *gcFunc, FLISP_PARAMETER_ERROR, "expects a multiple of %d arguments", -primitive->nMaxArgs);

            switch ((*gcFunc)->primitive) {
            case PRIMITIVE_QUOTE:
                return (*gcArgs)->car;
            case PRIMITIVE_SETQ:
                return evalSetq(gcArgs, gcEnv, GC_ROOTS);
            case PRIMITIVE_PROGN:
                assert((*gcArgs) == nil || (*gcArgs)->type == TYPE_CONS);
                *gcObject = evalProgn(gcArgs, gcEnv, GC_ROOTS);
                break;
            case PRIMITIVE_COND:
                *gcObject = evalCond(gcArgs, gcEnv, GC_ROOTS);
                break;
            case PRIMITIVE_LAMBDA:
                return evalLambda(gcArgs, gcEnv, GC_ROOTS);
            case PRIMITIVE_MACRO:
                return evalMacro(gcArgs, gcEnv, GC_ROOTS);
            case PRIMITIVE_MACROEXPAND:
                return evalMacroExpand(gcArgs, gcEnv, GC_ROOTS);
            default:
                *gcArgs = evalList(gcArgs, gcEnv, GC_ROOTS);
                return primitive->eval(gcArgs, GC_ROOTS);
            }
        } else {
            exceptionWithObject(interp, *gcFunc, FLISP_WRONG_TYPE, "is not a function");
        }
    }
}

// MAIN ///////////////////////////////////////////////////////////////////////

Object *newRootEnv(GC_PARAM, int arg, char **argv, char* flib)
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

    // add argv0 and argv
    *gcVar = newSymbol("argv0", GC_ROOTS);
    *gcVal = newString(*argv, GC_ROOTS);
    argv++;
    envSet(gcVar, gcVal, gcEnv, GC_ROOTS);

    *gcVar = newSymbol("argv", GC_ROOTS);
    *gcVal = nil;
    Object **i = gcVal;
    while(*argv) {
        *i = newCons(&nil, &nil, GC_ROOTS);
        (*i)->car = newString(*argv, GC_ROOTS);
        i = &(*i)->cdr;
        argv++;
    }
    envSet(gcVar, gcVal, gcEnv, GC_ROOTS);

    // add fLisp library dir aka script_dir
    *gcVar = newSymbol("script_dir", GC_ROOTS);
    *gcVal = newString(flib, GC_ROOTS);

    envSet(gcVar, gcVal, gcEnv, GC_ROOTS);

    return *gcEnv;
}

Memory *newMemory(size_t size)
{
    Memory *memory = malloc(sizeof(Memory));
    if (!memory) return NULL;

    memory->capacity = size;
    memory->fromOffset = 0;
    memory->toOffset = 0;
    memory->fromSpace = NULL;
    memory->toSpace = NULL;

    return memory;
}

/*
 * Public interface for embedding fLisp into an application.
 */

/** Initialize and return an fLisp interpreter.
 *
 * @param size  memory size for the Lisp objects
 * @param argc  argument count
 * @param argv  null terminated array to arguments
 * @param flib  path to Lisp library
 *
 * @returns On success: a pointer to an fLisp interpreter structure
 * @returns On failures: NULL
 *
 * Note: at the moment we only initialize and the statically allocated
 * interpreter `flisp` and return a pointer to it.
 */
Interpreter *lisp_new(size_t size, int argc, char **argv, char *library_path)
{
    if (lisp_interpreters != NULL)
        return NULL;

    // Note: this is the static interp. Change to local variable after
    //   refactoring the whole fLisp core to pass the interpreter
    //   instead of GC_PARAM
    interp = malloc(sizeof(Interpreter));
    if (interp == NULL) return NULL;

    Memory *memory = newMemory(size);
    if (memory == NULL) {
        interp->result = FLISP_OOM;
        strncpy(interp->message,
                "failed to allocate memory for the interpreter",  sizeof(interp->message));
        return NULL;
    }
    interp->memory = memory;

    interp->input = nil;
    interp->output = nil;
    interp->object = nil;
    interp->message[0] = '\0';
    interp->result = FLISP_OK;
    interp->debug = nil;

    interp->stackframe = NULL;

    interp->buf = NULL;
    resetBuf(interp);

    interp->theRoot = nil;
    interp->symbols = nil;
    interp->symbols = newCons(&nil, &interp->symbols, interp->theRoot);
    interp->symbols = newCons(&t, &interp->symbols, interp->theRoot);

    interp->root.type = TYPE_CONS;
    interp->root.car = newRootEnv(interp->theRoot, argc, argv, library_path);
    interp->root.cdr = interp->theRoot;

    interp->theEnv = &interp->root.car;
    interp->theRoot = &interp->root;

    interp->next = interp;
    lisp_interpreters = interp;

    return interp;
}

void lisp_destroy(Interpreter *interp)
{
    Interpreter *i;
    for (i=lisp_interpreters; i->next != interp; i=i->next);
    i->next = interp->next;
    i = NULL;

    free(interp->memory);
    free(interp);
}


void lisp_flush_output(Interpreter *interp)
{
    int result;
    if ((result = file_fflush(interp, interp->output))) {
        exceptionWithObject(interp, interp->output, FLISP_IO_ERROR, "failed to fflush output stream, errno: %d", result);
    }
}
/** lisp_eval(interp) - protected evaluation of interpreters input stream
 *
 * @param interp  fLisp interpreter
 *
 * returns: result code of first exception or RESULT_OK if none
 *   happened
 *
 */
ResultCode lisp_eval(Interpreter *interp)
{
    // gcRoots is needed by GC_TRACE
    Object *gcRoots = interp->theRoot;

    GC_TRACE(gcObject, nil);

    for (;;) {

        switch (setjmp(*interp->stackframe)) {
        case FLISP_OK: break;
        case FLISP_RETURN: return FLISP_OK;
        default: return FLISP_ERROR;
        }

        *gcObject = nil;

        if (peekNext(interp) == EOF) {
            writeChar(interp, '\n');
            interp->object = *gcObject;
            lisp_flush_output(interp);
            //return FLISP_OK;
            break;
        }
        *gcObject = readExpr(interp);
        *gcObject = evalExpr(gcObject, interp->theEnv, GC_ROOTS);
        writeObject(interp, *gcObject, true);
        writeChar(interp, '\n');
        lisp_flush_output(interp);
        }
    longjmp(*interp->stackframe, FLISP_RETURN);
}

/** lisp_eval_string() - interpret a string in Lisp
 *
 * @param interp  Interpreter to use
 * @param input   string to evaluate
 *
 * Before calling `lisp_eval_string()` initialize:
 * - interp->output: to an output stream, otherwise you get output to memory.
 * - interp->message: to an empty string
 * - interp->result:  to FLISP_OK
 * - interp->debug:   optional: to an output stream for debug
 *                     messages. see file_fopen()
 *
 * Returns: FLISP_OK if successful, FLISP_ERROR otherwise.
 *
 * interp->result is set to the result code of the evaluation.
 *
 * If an error occurs during evaluation *interp->message* is set to an
 * error message.
 *
 */
ResultCode lisp_eval_string(Interpreter *interp, char * input)
{
    jmp_buf exceptionEnv, *prevEnv;
    ResultCode result;

    //fl_debug(interp, "lisp_eval_string(\"%s\")", input);

    interp->message[0] = '\0';
    interp->result = FLISP_OK;

    if (nil == (interp->input = file_fopen(interp, input, "<")))
        // If falling through exception:
        return interp->result;

    prevEnv = interp->stackframe;
    interp->stackframe = &exceptionEnv;
    result = lisp_eval(interp);
    interp->stackframe = prevEnv;

    //fl_debug(interp, "lisp_eval_string() => %d", interp->result);
    if (result)
        fl_debug(interp, "lisp_eval_string() => error: %s", interp->message);
    if (file_fflush(interp, interp->debug)) {
        interp->result = FLISP_IO_ERROR;
        strncpy(interp->message, "failed to fflush debug stream", sizeof(interp->message));
    }
    return result;
}

// Test: Open listp stream with fd and name
// if used with "empty" memory, chance is we don't get an exception
// "" "<"      lisp_read_string()
// "" ">"      file_fopen(, ">STRING")
// "<n" ""     lisp_stream(fdopen(n), "<n")
// ">n" ""     lisp_stream(fdopen(n), ">n")
// "path" "*"  lisp_stream(fd, "path")
Object *lisp_stream(Interpreter * interp, FILE * fd, char *name)
{
    return newStreamObject(fd, name, interp->theRoot);
}

/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
