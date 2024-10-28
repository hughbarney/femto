/*
 * fLisp - a tiny yet practical Lisp interpreter.
 *
 * Based on Tiny-Lisp: https://github.com/matp/tiny-lisp
 *
 *   public:
 *   lisp_new() .. create a new interpreter
 *   lisp_destroy() .. destroy an interpreter
 *   lisp_eval() .. evaluate interpreter input stream
 *   lisp_eval_string() .. evaluate a string
 *   lisp_stream() .. create lisp stream object from file
 *   file_fclose() .. close file associated with lisp stream object
 *   file_fflush() .. flush a stream associated with a lisp stream object
 *   writeObject() .. write a lisp object to an output stream
 */

#include <sys/mman.h>
#include <sys/stat.h>
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
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

#if !defined(MAP_ANONYMOUS) && defined(MAP_ANON)
#define MAP_ANONYMOUS        MAP_ANON
#endif

#define CPP_XSTR(s) CPP_STR(s)
#define CPP_STR(s) #s

/* Constant objects */
/* public */
Object *nil = &(Object) { TYPE_SYMBOL,.string = "nil" };
Object *t = &(Object) { TYPE_SYMBOL,.string = "t" };

/* intern */
Object *empty = &(Object) { TYPE_STRING,.string = "\0" };
Object *one = &(Object) { TYPE_NUMBER,.number = 1 };

/* List of interpreters */
Interpreter *lisp_interpreters = NULL;

// Note: remove after rewriting the whole fLisp core to pass the
// interpreter instead of GC_PARAM
static Interpreter *interp = NULL;


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
 * The format string is sent to the interpreters debug file descriptor - if there is one.
 *
 */
void fl_debug(Interpreter *interp, char *format, ...)
{
    if (interp->debug == NULL)
        return;

    va_list(args);
    va_start(args, format);
    if (vfprintf(interp->debug, format, args) < 0) {
        va_end(args);
        (void)fprintf(interp->debug, "fatal: failed to print debug message: %d, %s", errno, format);
    }
    va_end(args);
    (void)fputc('\n', interp->debug);
    (void)fflush(interp->debug);
}

char *typeNameStrings[] = { "NUMBER", "STRING", "SYMBOL", "CONS", "LAMBDA", "MACRO", "PRIMITIVE", "ENV", "STREAM" };

char *typeName(Object *object)
{
    return (object->type == -1) ? "MOVED" : typeNameStrings[object->type];
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
 */
#ifdef __GNUC__
void exceptionWithObject(Interpreter *, Object * object, ResultCode, char *format, ...)
    __attribute__ ((noreturn, format(printf, 4, 5)));
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

    assert(interp->catch != NULL);
    longjmp(*interp->catch, FLISP_ERROR);
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

Object *gcMoveObject(Interpreter *interp, Object * object)
{
    // skip object if it is not within from-space (i.e. on the stack)
    if (object < (Object *) interp->memory->fromSpace || object >= (Object *) ((char *)interp->memory->fromSpace + interp->memory->fromOffset)) {
        return object;
    }
    // if the object has already been moved, return its new location
    if (object->type == TYPE_MOVED)
        return object->forward;

        // copy object to to-space
    Object *forward = (Object *) ((char *)interp->memory->toSpace + interp->memory->toOffset);
    memcpy(forward, object, object->size);
    interp->memory->toOffset += object->size;

    if (object->type == TYPE_STREAM)
        fl_debug(interp, "moved stream %p, path %p/%s %s to %p",
                 (void *)object, (void *)object->path, object->path->string, typeName(object->path), (void *)forward);


    // mark object as moved and set forwarding pointer
    object->type = TYPE_MOVED;
    object->forward = forward;

    return object->forward;
}

void gc(Interpreter *interp, GC_PARAM)
{
    fl_debug(interp, "collecting garbage: %lu/%lu", interp->memory->fromOffset, interp->memory->capacity);

    interp->memory->toOffset = 0;

    // move symbols and root objects
    *interp->theEnv = gcMoveObject(interp, *interp->theEnv);
    interp->symbols = gcMoveObject(interp, interp->symbols);

    for (Object * object = GC_ROOTS; object != nil; object = object->cdr)
        object->car = gcMoveObject(interp, object->car);

    // iterate over objects in to-space and move all objects they reference
    for (Object * object = interp->memory->toSpace; object < (Object *) ((char *)interp->memory->toSpace + interp->memory->toOffset); object = (Object *) ((char *)object + object->size)) {

        switch (object->type) {
        case TYPE_NUMBER:
        case TYPE_STRING:
        case TYPE_SYMBOL:
        case TYPE_PRIMITIVE:
            break;
        case TYPE_STREAM:
            fl_debug(interp, "moving path %p/%s of stream %p", (void *)object->path, object->path->string, (void *)object);
            object->path = gcMoveObject(interp, object->path);
            break;
        case TYPE_CONS:
            object->car = gcMoveObject(interp, object->car);
            object->cdr = gcMoveObject(interp, object->cdr);
            break;
        case TYPE_LAMBDA:
        case TYPE_MACRO:
            object->params = gcMoveObject(interp, object->params);
            object->body = gcMoveObject(interp, object->body);
            object->env = gcMoveObject(interp, object->env);
            break;
        case TYPE_ENV:
            object->parent = gcMoveObject(interp, object->parent);
            object->vars = gcMoveObject(interp, object->vars);
            object->vals = gcMoveObject(interp, object->vals);
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

    fl_debug(interp, "collected: %lu: %lu/%lu",
             interp->memory->fromOffset - interp->memory->toOffset,
             interp->memory->toOffset, interp->memory->capacity
        );

    interp->memory->fromOffset = interp->memory->toOffset;

}


// MEMORY MANAGEMENT //////////////////////////////////////////////////////////

size_t memoryAlign(size_t size, size_t alignment)
{
    return (size + alignment - 1) & ~(alignment - 1);
}

Object *memoryAllocObject(Interpreter *interp, ObjectType type, size_t size, GC_PARAM)
{
    size = memoryAlign(size, sizeof(void *));

    // allocate from- and to-space
    if (!interp->memory->fromSpace) {
        if (!(interp->memory->fromSpace = mmap(NULL, interp->memory->capacity * 2, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0))) {
            if (!interp->catch) return nil;
            exception(interp, FLISP_OOM, "mmap() failed, %s", strerror(errno));
        }
        interp->memory->toSpace = (char *)interp->memory->fromSpace + interp->memory->capacity;
    }
    // run garbage collection if capacity exceeded
    if (interp->memory->fromOffset + size >= interp->memory->capacity)
        gc(interp, GC_ROOTS);
    if (interp->memory->fromOffset + size >= interp->memory->capacity) {
        if (!interp->catch) return nil;
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
    // Note: still static ---v
    return memoryAllocObject(interp, type, sizeof(Object), GC_ROOTS);
}

Object *newObjectFrom(Object ** from, GC_PARAM)
{
    // Note: still static -------------v
    Object *object = memoryAllocObject(interp, (*from)->type, (*from)->size, GC_ROOTS);
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

    // Note: still static ---v
    return memoryAllocObject(interp, type, sizeof(Object) + size, GC_ROOTS);
}

Object *newStringWithLength(char *string, size_t length, GC_PARAM)
{
    int nEscapes = 0;

    if (length == 0)
        return empty;

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

/** newStreamObject - create stream object from file descriptor and path
 *
 * @param fd .. FILE * stream descripter to register
 * @param name .. NULL or name of the file associated with fd
 * @param buf .. NULL or string to convert into an input file stream.
 */
Object *newStreamObject(FILE *fd, char *path, GC_PARAM)
{
    Object *stream = newObject(TYPE_STREAM, GC_ROOTS);
    stream->fd = fd;
    stream->buf = NULL;
    stream->path = NULL;

    if (path != NULL)
        (stream)->path = newString(path, GC_ROOTS);
    return stream;
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
void setRootSymbol(Interpreter *interp, char *name, Object *value, GC_PARAM)
{
    Object *symbol = newSymbol(name, GC_ROOTS);
    envSet(&symbol, &value, interp->theEnv, GC_ROOTS);
}

Object *evalExpr(Object **, Object **, Object *);

Object * getRootSymbol(Interpreter *interp, char *name, GC_PARAM)
{
    Object *symbol = newSymbol(name, GC_ROOTS);
    return evalExpr(&symbol, interp->theEnv, GC_ROOTS);
}


// READING S-EXPRESSIONS //////////////////////////////////////////////////////

// Input //////////

/** streamGetc - get a character from an input stream
 *
 * @param interp  fLisp interpreter
 * @param stream  open readable stream object
 *
 * returns: the next character from the stream, or EOF
 *
 * throws: FLISP_IO_ERROR
 */
int streamGetc(Interpreter *interp, Object *stream)
{
    assert(stream->type == TYPE_STREAM && stream->fd != NULL);

    int c;
    if ((c = fgetc(stream->fd)) == EOF)
        if (ferror(stream->fd))
            exceptionWithObject(interp, stream, FLISP_IO_ERROR, "failed to fgetc, errno: %d", errno);
    return c;
}
/** streamUngetc - push back the last streamGetc'd character to input stream
 *
 * @param interp  fLisp interpreter
 * @param stream  open readable stream object
 *
 * returns: pushed back character or EOF on error
 *
 * throws: FLISP_IO_ERROR
 *
 */
int streamUngetc(Interpreter *interp, Object *stream, int c)
{
    assert(stream->type == TYPE_STREAM && stream->fd != NULL);

    if ((c = ungetc(c, stream->fd)) == EOF)
        exceptionWithObject(interp, stream, FLISP_IO_ERROR, "failed to ungetc, errno: %d", errno);
    return c;
}


// Begin helpers //////////
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
 * @param stream  open readable input stream
 *
 * returns: number object
 *
 * throws: FLISP_READ_RANGE
 */
Object *newInteger(Interpreter *interp, Object *stream, GC_PARAM)
{
    long l;

    Object *number;

    addCharToBuf(interp, '\0');
    errno = 0;
    l = strtol(interp->buf, NULL, 10);
    if (errno == ERANGE)
        exceptionWithObject(interp, stream, FLISP_READ_RANGE, "integer out of range,: %ld", l);
    number = newNumber(l, GC_ROOTS);
    resetBuf(interp);
    return number;
}

/** newDouble - add a float from the read buffer to the interpreter
 *
 * @param interp  fLisp interpreter
 * @param stream  open readable input stream
 *
 * returns: number object
 *
 * throws: FLISP_READ_RANGE
 */
Object *newDouble(Interpreter *interp, Object *stream, GC_PARAM)
{
    double d;
    Object *number;

    addCharToBuf(interp, '\0');
    errno = 0;
    d = strtod(interp->buf, NULL);
    if (errno == ERANGE)
        exceptionWithObject(interp, stream, FLISP_READ_RANGE, "integer out of range,: %f", d);
    // Note: purposely not dealing with NaN
    number = newNumber(d, GC_ROOTS);
    resetBuf(interp);
    return number;
}

// Reader /////////

/** streamPeek - get the next character from input stream, but stay at the current offset
 *
 * @param interp  fLisp interpreter
 * @param stream  open readable stream object
 *
 * returns: next character in stream or EOF
 *
 * throws: FLISP_IO_ERROR
 */
int streamPeek(Interpreter *interp, Object *stream)
{
    assert(stream->type == TYPE_STREAM && stream->fd != NULL);

    int c = streamGetc(interp, stream);
    if (c != EOF)
        streamUngetc(interp, stream, c);
    return c;
}

/** readNext - skip comments and spaces in input stream
 *
 * @param interp  fLisp interpreter
 * @param stream  open readable stream
 *
 * returns: next not space, not comment character
 *
 * throws: FLISP_IO_ERROR
 */
int readNext(Interpreter *interp, Object *stream)
{
    assert(stream->type == TYPE_STREAM && stream->fd != NULL);

    for (;;) {
        int ch = streamGetc(interp, stream);
        if (ch == EOF)
            return ch;
        if (ch == ';')
            while ((ch = streamGetc(interp, stream)) != EOF && ch != '\n');
        if (isspace(ch))
            continue;
        return ch;
    }
}
/** peekNext - skip to last space or comment character in input stream
 *
 * @param interp  fLisp interpreter
 * @param stream  open readable stream object
 *
 * returns: next not space, not comment character
 *
 * throws: FLISP_IO_ERROR
 */
int peekNext(Interpreter *interp, Object *stream)
{
    assert(stream->type == TYPE_STREAM && stream->fd != NULL);

    int c = readNext(interp, stream);
    if (c != EOF)
        streamUngetc(interp, stream, c);
    return c;
}
/** readWhile - skip to next charater not fullfilling a predicate in input stream
 *
 * @param interp     fLisp interpreter
 * @param stream     open readable stream object
 * @param predicate  function returning 0 if a character matches *predicate*
 *
 * returns: next character not fullfilling *predicate*
 *
 * throws: FLISP_IO_ERROR
 */
int readWhile(Interpreter *interp, Object *stream, int (*predicate) (int ch))
{
    assert(stream->type == TYPE_STREAM && stream->fd != NULL);

    for (;;) {
        int ch = streamPeek(interp, stream);
        if (ch == EOF)
            return ch;
        if (!predicate(ch))
            return ch;
        (void)addCharToBuf(interp, streamGetc(interp, stream));
    }
}

/** readString - return string object from input stream
 *
 * @param interp  fLisp interpreter
 * @param stream  open readable stream object
 *
 * throws: FLISP_IO_ERROR, FLISP_READ_INCOMPLETE, FLISP_OOM
 */
Object *readString(Interpreter *interp, Object *stream, GC_PARAM)
{
    int ch;
    Object *string;

    assert(stream->type == TYPE_STREAM && stream->fd != NULL);

    resetBuf(interp);

    for (bool isEscaped = false;;) {
        ch = streamGetc(interp, stream);
        if (ch == EOF) {
            exceptionWithObject(interp, stream, FLISP_READ_INCOMPLETE, "unexpected end of stream in string literal");
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

/** readNumberOrSymbol - return integer, float or symbol from input stream
 *
 * @param interp  fLisp interpreter
 * @param stream  open readable stream object
 *
 * returns: number object
 *
 * throws: FLISP_IO_ERROR, FLISP_READ_INCOMPLETE, FLISP_READ_RANGE, FLISP_OOM
 */
Object *readNumberOrSymbol(Interpreter *interp, Object *stream, GC_PARAM)
{
    assert(stream->type == TYPE_STREAM && stream->fd != NULL);

    int ch = streamPeek(interp, stream);

    resetBuf(interp);

    // skip optional leading sign
    if (ch == '+' || ch == '-') {
        (void)addCharToBuf(interp, streamGetc(interp, stream));
        ch = streamPeek(interp, stream);
    }
    // try to read a number in integer or decimal format
    if (ch == '.' || isdigit(ch)) {
        if (isdigit(ch))
            ch = readWhile(interp, stream, isdigit);
        if (!isSymbolChar(ch))
            return newInteger(interp, stream, GC_ROOTS);
        if (ch == '.') {
            ch = streamGetc(interp, stream);
            if (isdigit(streamPeek(interp, stream))) {
                ch = readWhile(interp, stream, isdigit);
                if (!isSymbolChar(ch))
                    return newDouble(interp, stream, GC_ROOTS);
            }
        }
    }
    // non-numeric character encountered, read a symbol
    readWhile(interp, stream, isSymbolChar);
    Object * obj = newSymbolWithLength(interp->buf, interp->len, GC_ROOTS);
    resetBuf(interp);
    return obj;
}

Object *readExpr(Interpreter *, Object *, Object *);

/** readList - return list from input stream
 *
 * @param interp  fLisp interpreter
 * @param stream  open readable stream object
 *
 * returns: list
 *
 * throws: FLISP_IO_ERROR, FLISP_READ_INCOMPLETE, FLISP_READ_RANGE, FLISP_OOM
 */
Object *readList(Interpreter *interp, Object *stream, GC_PARAM)
{
    assert(stream->type == TYPE_STREAM && stream->fd != NULL);

    GC_TRACE(gcList, nil);
    GC_TRACE(gcLast, nil);
    GC_TRACE(gcStream, stream);

    for (;;) {
        int ch = readNext(interp, *gcStream);
        if (ch == EOF)
            exceptionWithObject(interp, *gcStream, FLISP_READ_INCOMPLETE, "unexpected end of stream in list");
        else if (ch == ')')
            return reverseList(*gcList);
        else if (ch == '.' && !isSymbolChar(streamPeek(interp, *gcStream))) {
            if (*gcLast == nil)
                exceptionWithObject(interp, *gcStream, FLISP_READ_INVALID, "unexpected dot at start of list");
            if ((ch = peekNext(interp, *gcStream)) == ')')
                exceptionWithObject(interp, *gcStream, FLISP_READ_INVALID, "expected object at end of dotted list");
            if (!(*gcLast = readExpr(interp, *gcStream, GC_ROOTS)))
                exceptionWithObject(interp, *gcStream, FLISP_READ_INCOMPLETE, "unexpected end of stream in dotted list");
            if ((ch = peekNext(interp, *gcStream)) != ')')
                exceptionWithObject(interp, *gcStream, FLISP_READ_INVALID, "unexpected object at end of dotted list");
            readNext(interp, *gcStream);
            Object *list = reverseList(*gcList);
            (*gcList)->cdr = *gcLast;

            return list;
        } else {
            streamUngetc(interp, *gcStream, ch);
            *gcLast = readExpr(interp, *gcStream, GC_ROOTS);
            *gcList = newCons(gcLast, gcList, GC_ROOTS);
        }
    }
}
/** readUnary - return an unary operator together with the next
 *     expression from input stream
 *
 * @param interp  fLisp interpreter
 * @param stream  readable input stream object
 * @param symbol  symbol to be inserted
 *
 * returns: unary operator expression
 *
 * throws: FLISP_IO_ERROR, FLISP_READ_INCOMPLETE, FLISP_READ_RANGE,
 *     FLISP_OOM
 */
Object *readUnary(Interpreter *interp, Object *stream, char *symbol, GC_PARAM)
{
    assert(stream->type == TYPE_STREAM && stream->fd != NULL);

    GC_TRACE(gcStream, stream);

    if (peekNext(interp, *gcStream) == EOF)
        exceptionWithObject(interp, *gcStream, FLISP_READ_INCOMPLETE, "unexpected end of stream in readUnary(%s)", symbol);

    GC_TRACE(gcSymbol, newSymbol(symbol, GC_ROOTS));
    GC_TRACE(gcObject, readExpr(interp, *gcStream, GC_ROOTS));

    *gcObject = newCons(gcObject, &nil, GC_ROOTS);
    *gcObject = newCons(gcSymbol, gcObject, GC_ROOTS);

    return *gcObject;
}
/** readExpr - return next lisp sexp object from stream or from interpreter input stream
 *
 * @param interp  fLisp interpreter
 * @param stream   open readable stream object
 *
 * returns: sexp object or NULL if EOF
 *
 * throws: FLISP_IO_ERROR, FLISP_READ_INCOMPLETE, FLISP_READ_RANGE,
 *     FLISP_OOM
 */
Object *readExpr(Interpreter *interp, Object *stream, GC_PARAM)
{
    assert(stream->type == TYPE_STREAM && stream->fd != NULL);

    GC_TRACE(gcStream, stream);

    for (;;) {

        int ch = readNext(interp, *gcStream);

        if (ch == EOF)
            return NULL;
        else if (ch == '\'' || ch == ':')
            return readUnary(interp, *gcStream, "quote", GC_ROOTS);
        else if (ch == '"')
            return readString(interp, *gcStream, GC_ROOTS);
        else if (ch == '(')
            return readList(interp, *gcStream, GC_ROOTS);
        else if (isSymbolChar(ch) && (ch != '.' || isSymbolChar(streamPeek(interp, *gcStream)))) {
            (void)streamUngetc(interp, *gcStream, ch);
            return readNumberOrSymbol(interp, *gcStream, GC_ROOTS);
        }
        else
            exceptionWithObject(interp, *gcStream, FLISP_READ_INVALID, "unexpected character, `%c'", ch);
    }
}

/** (fread stream eofv) - read one object from input stream
 *
 * @param stream  input stream to read.
 * @param eofv    On EOF: if nil throw exception, else value to return.
 *
 * returns: Object
 *
 * throws: FLISP_INVALID_VALUE, FLISP_IO_ERROR, FLISP_EOF
 */
Object *primitiveFread(Object ** args, GC_PARAM)
{
    Object *eofv = nil;

    ONE_STREAM_ARG(fread);

    if ((*args)->cdr != nil)
        eofv = (*args)->cdr->car;

    if (stream->type != TYPE_STREAM)
        exceptionWithObject(interp, stream, FLISP_INVALID_VALUE, "(read [fd ..]) - fd is not a stream: %s", typeName(stream));

    GC_TRACE(gcStream, stream);

    Object *result = readExpr(interp, *gcStream, GC_ROOTS);
    if (result == NULL) {
        if (eofv == nil)
            exceptionWithObject(interp, *gcStream, FLISP_EOF, "(read [..]) input exhausted");
        else
            result = eofv;
    }
    return result;
}


// EVALUATION /////////////////////////////////////////////////////////////////

typedef struct Primitive {
    char *name;
    int nMinArgs, nMaxArgs;
    Object *(*eval) (Object ** args, GC_PARAM);
} Primitive;

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

/* Scheme-style tail recursive evaluation. evalProgn and evalCond
 * return the object in the tail recursive position to be evaluated by
 * evalExpr. Macros are expanded in-place the first time they are evaluated.
 */

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

Primitive primitives[];

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

            for (Object * args = *gcArgs; args != nil; args = args->cdr, nArgs++) {
                if (args->type != TYPE_CONS)
                    exceptionWithObject(interp, args, FLISP_WRONG_TYPE, "(%s args) - args is not a list: arg %d", primitive->name, nArgs);
                if (args->cdr->type == TYPE_MOVED)
                    exceptionWithObject(interp, args->cdr, FLISP_GC_ERROR, "(%s args) - arg %d is already disposed off", primitive->name, nArgs);
            }
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

Object *primitiveEval(Object **args, GC_PARAM)
{
    return evalExpr(&(*args)->car, interp->theEnv, GC_ROOTS);
}


// Write /////////////////////////////////////////////////////////////////////////////////

// Output ////////

Object *file_outputMemStream(Interpreter *);

/** getOutputStream - return a stream for output
 *
 * @param interp  fLisp interpreter
 *
 * returns: the stream, if nil the default output stream, if nil a
 *          memory output stream.
 *
 * throws: FILSP_IO_ERROR
 */

Object *getOutputStream(Interpreter *interp, Object *stream, GC_PARAM)
{
    if (stream == nil) {
        stream = getRootSymbol(interp, "OUTPUT", GC_ROOTS);
        if (stream == nil) {
            stream = file_outputMemStream(interp);
            setRootSymbol(interp, "OUTPUT", stream, GC_ROOTS);
        }
    }
    GC_TRACE(gcStream, stream);
    return *gcStream;
}
/** fl_flush_stream - flush given stream, throwing exception if due
 *
 * @param interp  fLisp interpreter
 * @param stream  open writable output stream
 *
 * throws: FLISP_IO_ERROR
 */
void fl_flush_stream(Interpreter *interp, Object *stream, GC_PARAM)
{
    int result;

    stream = getOutputStream(interp, stream, GC_ROOTS);

    if ((result = file_fflush(interp, stream))) {
        exceptionWithObject(interp, stream, FLISP_IO_ERROR, "failed to fflush output stream, errno: %d", result);
    }
}

/** writeChar - write character to Lisp stream
 *
 * @param interp  fLisp interpreter
 * @param stream  open writeable stream
 * @param ch      character to write
 *
 * throws: FLISP_IO_ERROR
 */
void writeChar(Interpreter *interp, Object *stream, char ch, GC_PARAM)
{
    stream = getOutputStream(interp, stream, GC_ROOTS);

    if(fputc(ch, stream->fd) == EOF)
        exceptionWithObject(interp, stream, FLISP_IO_ERROR, "failed to write character %c, errno: %d", ch, errno);
}

/** writeString - write string to Lisp stream
 *
 * @param interp  fLisp interpreter
 * @param stream  open writable output stream
 * @param str     string to write
 *
 * throws: FLISP_IO_ERROR
 *
 */
void writeString(Interpreter *interp, Object *stream, char *str, GC_PARAM)
{
    int len;

    stream = getOutputStream(interp, stream, GC_ROOTS);

    len = strlen(str);
    if(fprintf(stream->fd, str) != len)
        exceptionWithObject(interp, stream, FLISP_IO_ERROR, "failed to write %d files, errno: %d", len, errno);
}
/** writeFmt - write printf formatted string to Lisp stream
 *
 * @param interp  fLisp interpreter
 * @param stream  open writable stream
 * @param format ... printf like format string
 *
 * throws: FLISP_IO_ERROR
 */
#ifdef __GNUC__
void writeFmt(Interpreter *, Object *, Object *, char *format, ...)
    __attribute__ ((format(printf, 4, 5)));
#endif
void writeFmt(Interpreter *interp, GC_PARAM, Object *stream, char *format, ...)
{
    stream = getOutputStream(interp, stream, GC_ROOTS);

    va_list(args);
    va_start(args, format);
    if (vfprintf(stream->fd, format, args) < 0) {
        va_end(args);
        exceptionWithObject(interp, stream, FLISP_IO_ERROR, "failed to fprintf, errno: %d", errno);
    }
    va_end(args);
}


// WRITING OBJECTS ////////////////////////////////////////////////////////////

/** writeObject - format and write object to stream
 *
 * @param interp  fLisp interpreter
 * @param stream  open writeable stream
 * @param object  object to be serialized
 * @param readably  if true, write in a format which can be read back
 *
 * throws: FLISP_GC_ERROR, FLISP_IO_ERROR
 *
 */
void writeObject(Interpreter *interp, Object *stream, Object *object, bool readably, GC_PARAM)
{
    switch (object->type) {
#define CASE(type, ...)                         \
        case type:                              \
            writeFmt(interp, GC_ROOTS, stream, __VA_ARGS__);     \
            break
        CASE(TYPE_NUMBER, "%g", object->number);
        CASE(TYPE_SYMBOL, "%s", object->string);
        CASE(TYPE_PRIMITIVE, "#<Primitive %s>", object->name);
        CASE(TYPE_STREAM, "#<Stream %p, %s>", (void *) object->fd, (object->fd == NULL) ? "<closed>" : object->path->string);
#undef CASE
    case TYPE_STRING:
        if (readably) {
            writeChar(interp, stream, '"', GC_ROOTS);
            for (char *string = object->string; *string; ++string) {
                switch (*string) {
                case '"':
                    writeString(interp, stream, "\\\"", GC_ROOTS);
                    break;
                case '\t':
                    writeString(interp, stream, "\\t", GC_ROOTS);
                    break;
                case '\r':
                    writeString(interp, stream, "\\r", GC_ROOTS);
                    break;
                case '\n':
                    writeString(interp, stream, "\\n", GC_ROOTS);
                    break;
                case '\\':
                    writeString(interp, stream, "\\\\", GC_ROOTS);
                    break;
                default:
                    writeChar(interp, stream, *string, GC_ROOTS);
                    break;
                }
            }
            writeChar(interp, stream, '"', GC_ROOTS);
        } else
            writeFmt(interp, GC_ROOTS, stream, "%s", object->string);
        break;
    case TYPE_CONS:
        writeChar(interp, stream, '(', GC_ROOTS);
        writeObject(interp, stream, object->car, readably, GC_ROOTS);
        while (object->cdr != nil) {
            object = object->cdr;
            if (object->type == TYPE_CONS) {
                writeChar(interp, stream, ' ', GC_ROOTS);
                writeObject(interp, stream, object->car, readably, GC_ROOTS);
            } else {
                writeString(interp, stream, " . ", GC_ROOTS);
                writeObject(interp, stream, object, readably, GC_ROOTS);
                break;
            }
        }
        writeChar(interp, stream, ')', GC_ROOTS);
        break;
#define CASE(type, name, object)                                        \
        case type:                                                      \
            writeFmt(interp, GC_ROOTS, stream, "#<%s ", name);          \
            writeObject(interp, stream, object, readably, GC_ROOTS);    \
            writeChar(interp, stream, '>', GC_ROOTS);                   \
            break
        CASE(TYPE_LAMBDA, "Lambda", object->params);
        CASE(TYPE_MACRO, "Macro", object->params);
        CASE(TYPE_ENV, "Env", object->vars);
#undef CASE
    case TYPE_MOVED:
        exception(interp, FLISP_GC_ERROR, "won't write a garbage collected item");
        break;
    }
    fl_flush_stream(interp, stream, GC_ROOTS);
}

/** (write object [[key value] ..]) - write object
 *
 * @param object         object to write
 * @param key :stream    optional, use *value* as stream
 * @param key :readably  optional, if not nil escape strings
 *
 * returns: object
 *
 * throws: FLISP_PARAMETER_ERROR, FLISP_IO_ERROR, FLISP_GC_ERROR
 */
Object *primitiveWrite(Object **args, GC_PARAM)
{
    Object *stream = nil;
    bool readably = false;
    Object *obj = (*args)->car;

    if ((*args)->cdr == nil)
        goto write;

    Object *list = (*args)->cdr;
    GC_TRACE(gcKey, nil);
    GC_TRACE(gcValue, nil);
    for (;list != nil; list = list->cdr) {
        *gcKey = list->car;
        if ((*gcKey)->type != TYPE_SYMBOL)
            exceptionWithObject(interp, *args, FLISP_PARAMETER_ERROR, "(write obj [[key val] ..]) - key is not a symbol");
        if (list->cdr == nil)
            exceptionWithObject(interp, *args, FLISP_PARAMETER_ERROR, "(write obj [[key val] ..]) - val is missing");
        list = list->cdr;
        *gcValue = list->car;
        if (!strcmp("stream", (*gcKey)->string)) {
            if ((*gcValue)->type != TYPE_STREAM)
                exceptionWithObject(interp, *args, FLISP_PARAMETER_ERROR, "(write obj [[key val] ..]) - value of key stream is not a stream");
            stream = *gcValue;
        } else if (!strcmp("readably", (*gcKey)->string)) {
            readably = (*gcValue != nil);
        } else
            exceptionWithObject(interp, *args, FLISP_PARAMETER_ERROR, "(write obj [[key val] ..]) - unknown key: %s", (*gcKey)->string);
    }
write:
    writeObject(interp, stream, obj, readably, GC_ROOTS);
    return obj;
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
        // Note: TYPE_STREAM: compare fd's ?
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
}

Object *primitiveCdr(Object ** args, GC_PARAM)
{
    Object *first = (*args)->car;

    if (first == nil)
        return nil;
    else if (first->type == TYPE_CONS)
        return first->cdr;

    exceptionWithObject(interp, first, FLISP_WRONG_TYPE, "(cdr arg) - arg must be a list");
}

Object *primitiveCons(Object ** args, GC_PARAM)
{
    GC_TRACE(gcFirst, (*args)->car);
    GC_TRACE(gcSecond, (*args)->cdr->car);

    return newCons(gcFirst, gcSecond, GC_ROOTS);
}

Object *primitiveGc(Object ** args, GC_PARAM)
{
    // Note:
    // v-- static
    gc(interp, GC_ROOTS);
    return t;
}
Object *primitiveRoot(Object ** args, GC_PARAM)
{
    // Note:
    //     v--- static
    return interp->theRoot;
}
Object *primitiveEnv(Object ** args, GC_PARAM)
{
    GC_TRACE(env, nil);
    // Note: static v
    *env = newCons(&interp->root.car->vars, env, GC_ROOTS);
    *env = newCons(&interp->root.car->vals, env, GC_ROOTS);
    return *env;
}
Object *primitiveSignal(Object ** args, GC_PARAM)
{
    Object *first = (*args)->car;
    Object *second = (*args)->cdr;

    if (first->type != TYPE_SYMBOL)
        exceptionWithObject(interp, first , FLISP_WRONG_TYPE, "(signal type data) - type is not a symbol");

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
}

DEFINE_PRIMITIVE_RELATIONAL(primitiveEqual, ==)
DEFINE_PRIMITIVE_RELATIONAL(primitiveLess, <)
DEFINE_PRIMITIVE_RELATIONAL(primitiveLessEqual, <=)
DEFINE_PRIMITIVE_RELATIONAL(primitiveGreater, >)
DEFINE_PRIMITIVE_RELATIONAL(primitiveGreaterEqual, >=)


// STREAMS //////////////////////////////////////////////////

/* Minimal stream C-API for interpreter operation */

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
    return stream;
}
/** file_inputMemStream - convert string to Lisp stream object
 *
 * @param interp  fLisp interpreter
 * @param string  string to read
 *
 * returns: Lisp stream object or nil on failure
 *
 */
Object *file_inputMemStream(Interpreter *interp, char *string)
{
    FILE *fd;

    Object *gcRoots = interp->theRoot;

    fd = fmemopen(string, strlen(string), "r");
    if (fd == NULL) return nil;
    return newStreamObject(fd, "<STRING", GC_ROOTS);
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
    Object *stream;

    Object *gcRoots = interp->theRoot;

    if (strcmp("<", mode) == 0) {
        if (nil == (stream = file_inputMemStream(interp, path)))
            exception(interp, FLISP_IO_ERROR, "failed to open string as memory input stream: %d", errno);
        return stream;
    }
    if (strcmp(">", mode) == 0) {
        if (nil == (stream = file_outputMemStream(interp)))
            exception(interp, FLISP_IO_ERROR, "failed to open memory output stream: %d", errno);
        return stream;
    }
    char c = path[0];
    if (c == '<' || c == '>') {
        char *end;
        errno = 0;
        long d = strtol(&path[1], &end, 0);
        if (errno || *end != '\0' || d < 0 || d > _POSIX_OPEN_MAX)
            exception(interp, FLISP_INVALID_VALUE, "invalid I/O stream number: %s", &path[1]);
        if (NULL == (fd = fdopen((int)d, c == '<' ? "r" : "a")))
            exception(interp, FLISP_IO_ERROR, "failed to open I/O stream %ld for %s", d, c == '<' ? "reading" : "writing");
    } else {
        if (NULL == (fd = fopen(path, mode)))
            exception(interp, FLISP_IO_ERROR, "failed to open file '%s' with mode '%s', errno: %d", path, mode, errno);
    }
    return newStreamObject(fd, path, GC_ROOTS);
}
/** file_fclose - close stream
 *
 * @param interp  fLisp interpreter
 * @param stream  open stream
 *
 * returns: 0 on success, errno otherwise
 *
 * Public C Interface
 */
int file_fclose(Interpreter *interp, Object *stream)
{
    // Note: remove after debugging lisp_eval_string()
    fl_debug(interp, "file_fclose(%s/%d/%p)",stream->path->string, fileno(stream->fd), (void *)stream->fd);
    // Note: we thought we needed to do this: but we get segfaulted. why?
    //if (stream->buf != NULL)
    //    free(stream->buf);
    if (fclose(stream->fd) == EOF) return errno;
    stream->fd = NULL;
    return 0;
}
/** file_fflush - flush output stream
 *
 * @param interp  fLisp interpreter
 * @param stream  open output stream
 *
 * returns: 0 on success, erno otherwise
 *
 * Public C Interface
 */
int file_fflush(Interpreter *interp, Object *stream)
{
    return (fflush(stream->fd) == EOF) ? errno : 0;
}

#ifdef FLISP_FILE_EXTENSION
#include "file.c"
#endif

/* OS interface */

Object *fl_system(Object ** args, GC_PARAM) {

    ONE_STRING_ARG(system);
    return newNumber((double) system(arg->string), GC_ROOTS);
}

Object *os_getenv(Object ** args, GC_PARAM)
{
    ONE_STRING_ARG(os.getenv);

    char *e = getenv(arg->string);
    if (e == NULL) return nil;
    return newStringWithLength(e, strlen(e), GC_ROOTS);
}

/* Strings */

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

    Object * str = newStringWithLength(new, len1 + len2, GC_ROOTS);
    free(new);

    return str;
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
    Object * new = newStringWithLength(sub, newlen, GC_ROOTS);
    free(sub);

    return new;
}

Object *stringLength(Object ** args, GC_PARAM)
{
    ONE_STRING_ARG(string.length);

    return newNumber(strlen(arg->string), GC_ROOTS);
}

/* String/Number conversion */

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


#ifdef FLISP_FEMTO_EXTENSION
#include "femto.primitives.c"
#endif

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
    {"fread", 0, 3, primitiveFread},
    {"eval", 1, 1, primitiveEval},
    {"write", 1, -1, primitiveWrite},
    {"gc", 0, 0, primitiveGc},
    {"root", 0, 0, primitiveRoot},
    {"env", 0, 0, primitiveEnv},
    {"signal", 1, -1, primitiveSignal},
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
//    {"load", 1, 1, primitiveLoad},
    {"os.getenv", 1, 1, os_getenv},
    {"system", 1, 1, fl_system},
    FLISP_REGISTER_FILE_EXTENSION
#ifdef FLISP_FEMTO_EXTENSION
#include "femto.register.c"
#endif
};


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

    return *gcEnv;
}

Memory *newMemory(size_t size)
{
    Memory *memory = malloc(sizeof(Memory));
    if (!memory) return NULL;

    memory->capacity = size/2;
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
 * @param size          memory size for the Lisp objects.
 * @param argv          null terminated array to arguments to be imported.
 * @param library_path  path to Lisp library, aka 'script_dir'.
 * @param input         open readable file descriptor for default input or NULL
 * @param output        open writable file descriptor for default output or NULL.
 * @param debug         open writable file descriptor for debug output or NULL.
 *
 * @returns On success: a pointer to an fLisp interpreter structure
 * @returns On failures: NULL
 *
 * Note: at the moment we only provide a single interpreter store a
 * pointer to int in the static variable *interp* and return that variable.
 *
 */
Interpreter *lisp_new(
    size_t size, char **argv, char *library_path,
    FILE *input, FILE *output, FILE* debug)
{
    if (lisp_interpreters != NULL)
        return NULL;

    if (size*2 < FLISP_MIN_MEMORY) {
        interp->result = FLISP_INVALID_VALUE;
        strncpy(interp->message,
                "fLisp needs at least" CPP_STR(FLISP_MIN_MEMORY)  "bytes to start up", sizeof(interp->message));
        return NULL;
    }

    // Note: this is the static interp. Change to local variable after
    //   refactoring the whole fLisp core to pass the interpreter
    //   instead of GC_PARAM
    interp = malloc(sizeof(Interpreter));
    if (interp == NULL) return NULL;

    interp->debug = debug;

    Memory *memory = newMemory(size);
    if (memory == NULL) {
        interp->result = FLISP_OOM;
        strncpy(interp->message,
                "failed to allocate memory for the interpreter",  sizeof(interp->message));
        return NULL;
    }
    interp->memory = memory;

    interp->object = nil;
    interp->message[0] = '\0';
    interp->result = FLISP_OK;

    interp->catch = NULL;

    interp->buf = NULL;
    resetBuf(interp);

    interp->theRoot = nil;
    interp->symbols = nil;
    interp->symbols = newCons(&nil, &interp->symbols, interp->theRoot);
    interp->symbols = newCons(&t, &interp->symbols, interp->theRoot);

    interp->root.type = TYPE_CONS;
    interp->root.car = newRootEnv(interp->theRoot);
    interp->root.cdr = interp->theRoot;

    interp->theEnv = &interp->root.car;
    interp->theRoot = &interp->root;

    interp->catch = &interp->exceptionEnv;

    interp->next = interp;
    lisp_interpreters = interp;

    /* Add argv0 to the environment */
    Object *gcRoots = interp->theRoot; // gcRoots is needed by GC_TRACE and GC_ROOTS
    GC_TRACE(gcVar, newSymbol("argv0", GC_ROOTS));
    GC_TRACE(gcVal, newString(*argv, GC_ROOTS));
    envSet(gcVar, gcVal, interp->theEnv, GC_ROOTS);

    /* Add argv to the environement */
    *gcVar = newSymbol("argv", GC_ROOTS);
    *gcVal = nil;
    for (Object **i = gcVal; *++argv; i = &(*i)->cdr) {
        *i = newCons(&nil, &nil, GC_ROOTS);
        (*i)->car = newString(*argv, GC_ROOTS);
    }
    envSet(gcVar, gcVal, interp->theEnv, GC_ROOTS);

    /* Add library_path to the environment */
    *gcVar = newSymbol("script_dir", GC_ROOTS);
    *gcVal = newString(library_path, GC_ROOTS);
    envSet(gcVar, gcVal, interp->theEnv, GC_ROOTS);

    /* input stream */
    GC_TRACE(gcInput, nil);
    if (input)
        *gcInput = newStreamObject(input, "STDIN", interp->theRoot);
    interp->input = *gcInput;
    *gcVar = newSymbol("INPUT", GC_ROOTS);
    envSet(gcVar, gcInput, interp->theEnv, GC_ROOTS);

    /* output stream */
    GC_TRACE(gcOutput, nil);
    if (output)
        *gcOutput = newStreamObject(output, "STDOUT", interp->theRoot);
    interp->output = *gcOutput;
    *gcVar = newSymbol("OUTPUT", GC_ROOTS);
    envSet(gcVar, gcOutput, interp->theEnv, GC_ROOTS);

    return interp;
}

void lisp_destroy(Interpreter *interp)
{
    Interpreter *i;
    for (i=lisp_interpreters; i->next != interp; i=i->next);
    i->next = interp->next;
    i = NULL;


    if (interp->memory->fromSpace)
        (void)munmap(interp->memory->fromSpace, interp->memory->capacity * 2);
    // Note: we do not know which one it is, so we free both.
    if (interp->memory->toSpace)
        (void)munmap(interp->memory->toSpace, interp->memory->capacity * 2);
    free(interp->memory);
    free(interp);
}

/** lisp_eval - protected evaluation of input stream
 *
 * @param interp  fLisp interpreter
 * @param stream  open readable stream object
 * @param gcRoots gc root object
 *
 * returns: result code of first exception or RESULT_OK if none
 *     happened.
 */
ResultCode lisp_eval(Interpreter *interp, Object *stream, GC_PARAM)
{
    interp->result = FLISP_OK;
    interp->message[0] = '\0';

    fl_debug(interp, "lisp_eval(%s)", stream->path->string);

    GC_TRACE(gcObject, nil);
    GC_TRACE(gcStream, stream);

    for (;;) {

        switch (setjmp(*interp->catch)) {
        case FLISP_OK: break;
        case FLISP_RETURN: return FLISP_OK;
        default: return FLISP_ERROR;
        }

        if ((*gcObject = readExpr(interp, *gcStream, GC_ROOTS)) == NULL)
            break;
        *gcObject = evalExpr(gcObject, interp->theEnv, GC_ROOTS);
        interp->object = *gcObject;
        writeObject(interp, nil, *gcObject, true, GC_ROOTS);
        writeChar(interp, nil, '\n', GC_ROOTS);
        fl_flush_stream(interp, nil, GC_ROOTS);
    }
    longjmp(*interp->catch, FLISP_RETURN);
}

/** lisp_eval_string() - interpret a string in Lisp
 *
 * @param interp  Interpreter to use
 * @param input   string to evaluate
 * @param gcRoots gc root object
 *
 * Before calling `lisp_eval_string()` initialize:
 *
 * Returns: FLISP_OK if successful, FLISP_ERROR otherwise.
 *
 * - interp->result is set to the result code of the evaluation.
 * - interp->object is set to the resulting object
 *
 * The output from the evaluation is written to the default output of
 * the interpreter.
 *
 * If an error occurs during evaluation:
 *
 * - interp->object is set to the object causing the exception, or nil.
 * - interp->message is set to an error message.
 *
 */
ResultCode lisp_eval_string(Interpreter *interp, char * input, GC_PARAM)
{
    Object *stream;
    ResultCode result;

    fl_debug(interp, "lisp_eval_string(\"%s\")", input);

    if (nil == (stream = file_inputMemStream(interp, input))) {
        interp->result = FLISP_IO_ERROR;
        strncpy(interp->message, "failed to convert input to stream", sizeof(interp->message));
        return interp->result;
    }
    GC_TRACE(gcStream, stream);
    result = lisp_eval(interp, *gcStream, GC_ROOTS);
    file_fclose(interp, *gcStream);
    fl_debug(interp, "lisp_eval_string() => %d", interp->result);
    if (result) {
        if (interp->object == nil)
            fl_debug(interp, "lisp_eval_string() => error: %s", interp->message);
        else {
            Object *str = file_outputMemStream(interp);
            writeObject(interp, str, interp->object, true, GC_ROOTS);
            fl_debug(interp, "lisp_eval_string() => error: '%s', %s", str->buf, interp->message);
            file_fclose(interp, str);
        }
    }
    return result;
}

/** lisp_stream - convert a file descriptor into a Lisp stream object.
 *
 * @param interp  fLisp interpreter
 * @param fd      open file descriptor
 * @param name    name of the stream
 *
 * returns: Lisp stream object
 *
 * The file descriptor must be of type `FILE *`, obtained e.g. by
 * `fopen()`, `fdopen()`, `fmemopen()` or `open_memstream()`.
 *
 * The name should be either the path of the regular file, or:
 * - "<STRING" .. for fmemopen().
 * - ">STRING" .. for open_memstream().
 * - "<STDIN", ">STDOUT", ">STDERR" .. for `stdin`, `stdout` and
 *                `stderr` respectively.
 * - "<n", ">n" .. for fdopen(), where n is the decimal number of the
 *                underlying file descriptor.
 */

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
