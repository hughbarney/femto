/*
 * fLisp - a tiny yet practical Lisp interpreter.
 *
 * Based on Tiny-Lisp: https://github.com/matp/tiny-lisp
 *
 */

#include <sys/mman.h>
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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
#ifndef S_SPLINT_S
Object *nil = &(Object) { TYPE_SYMBOL,.string = "nil" };
Object *t = &(Object) { TYPE_SYMBOL,.string = "t" };
#else
Object _nil =  { TYPE_SYMBOL, 0, .string = "nil" };
Object *nil = &_nil; //&(Object) { TYPE_SYMBOL, 0, .string = "nil" };
Object _t = { TYPE_SYMBOL, 0, .string = "t" };
Object *t = &_t;
#endif

/* intern */
#ifndef S_SPLINT_S
Object *empty = &(Object) { TYPE_STRING,.string = "\0" };
Object *one = &(Object) { TYPE_NUMBER,.number = 1 };
#else
Object _empty = { TYPE_STRING, 0, .string = "\0" };
Object *empty = &_empty;
Object _one =  { TYPE_NUMBER, 0, .number = 1 };
Object *one = &_one;
#endif

bool gc_always = false;

/* List of interpreters */
Interpreter *lisp_interpreters = NULL;


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
    if (object->type > TYPE_STREAM)
        return "INVALID";
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
    size_t written;

    interp->object = object;
    interp->result = result;
    resetBuf(interp);

    size_t len = sizeof(interp->message);
    va_list(args);
    va_start(args, format);
    written = vsnprintf(interp->message, len, format, args);
    va_end(args);
    if (written > len)
        strcpy(interp->message+len-4, "...");
    else if (written < 0)
        strncpy(interp->message, "failed to format error message", len);

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
 * Originally GC_ROOTS and GC_PARAM are used to pass the list from
 * function to function.
 *
 *
 * GC_TRACE adds an object to the list and declares a variable which points to
 * the objects pointer inside the list.
 *
 *   GC_TRACE(gcX, X):  add object X to the list and declare Object **gcX
 *                      to point to the pointer to X inside the list.
 */

#define GC_PASTE1(name, id)  name ## id
#define GC_PASTE2(name, id)  GC_PASTE1(name, id)
#define GC_UNIQUE(name)      GC_PASTE2(name, __LINE__)

#define GC_CHECKPOINT Object *gcTop = interp->gcTop
#define GC_RELEASE interp->gcTop = gcTop
Object *gcReturn(Interpreter *interp, Object *gcTop, Object *result)
{
    GC_RELEASE;
    return result;
}
#define GC_RETURN(expr)  return gcReturn(interp, gcTop, expr)

#define GC_TRACE(name, init)                                            \
    Object GC_UNIQUE(gcTrace) = { TYPE_CONS, .car = init, .cdr = interp->gcTop }; \
    interp->gcTop = &GC_UNIQUE(gcTrace);                                \
    Object **name = &GC_UNIQUE(gcTrace).car;


/** gcMoveObject - save a single object from garbage collection
 *
 * @param interp  fLisp interpreter
 * @param object  object to save
 *
 * return: object at new location
 *
 */
typedef struct gcStats { size_t moved, constant, skipped; } gcStats;
Object *gcMoveObject(Interpreter *interp, Object *object, gcStats *stats)
{
    // skip object if it is not within from-space (i.e. on the stack)
    if (object < (Object *) interp->memory->fromSpace || object >= (Object *) ((char *)interp->memory->fromSpace + interp->memory->fromOffset)) {
        stats->constant++;
        return object;
    }
    // if the object has already been moved, return its new location
    if (object->type == TYPE_MOVED) {
        stats->skipped++;
        return object->forward;
    }
    stats->moved++;
    // copy object to to-space
    Object *forward = (Object *) ((char *)interp->memory->toSpace + interp->memory->toOffset);
    memcpy(forward, object, object->size);
    interp->memory->toOffset += object->size;

#if DEBUG_GC
    if (object->type == TYPE_STREAM)
        fl_debug(interp, "moved stream %p, path %p/%s %s to %p",
                 (void *)object, (void *)object->path, object->path->string, typeName(object->path), (void *)forward);
    if (object->type == TYPE_SYMBOL)
        fl_debug(interp, "moved symbol %s", object->string);
#endif
    // mark object as moved and set forwarding pointer
    object->type = TYPE_MOVED;
    object->forward = forward;

    return object->forward;
}

/** gc - move all active objects to new memory page
 *
 * @param interp   fLisp interpreter
 */
void gc(Interpreter *interp)
{
    Object *object;
    gcStats stats = {0};

    fl_debug(interp, "collecting garbage, memory: %lu/%lu", interp->memory->fromOffset, interp->memory->capacity);

    interp->memory->toOffset = 0;

    // move trace, symbols and root objects
    for (object = interp->gcTop; object != nil; object = object->cdr) {
#if DEBUG_GC
        fl_debug(interp, "moving gc traced object %p, %d", (void *)object->car, object->car->type);
#endif
        object->car = gcMoveObject(interp, object->car, &stats);
    }
#if DEBUG_GC
    fl_debug(interp, "gc traced objects: %lu, skipped %lu, constant %lu", stats.moved, stats.skipped, stats.constant);
#endif
    interp->symbols = gcMoveObject(interp, interp->symbols, &stats);
    interp->global = gcMoveObject(interp, interp->global, &stats);

    // iterate over objects in to-space and move all objects they reference
    for (object = interp->memory->toSpace;
         object < (Object *) ((char *)interp->memory->toSpace + interp->memory->toOffset);
         object = (Object *) ((char *)object + object->size)) {

        switch (object->type) {
        case TYPE_NUMBER:
        case TYPE_STRING:
        case TYPE_SYMBOL:
        case TYPE_PRIMITIVE:
            break;
        case TYPE_STREAM:
#if DEBUG_GC
            fl_debug(interp, "moving path %p/%s of stream %p", (void *)object->path, object->path->string, (void *)object);
#endif
            object->path = gcMoveObject(interp, object->path, &stats);
            break;
        case TYPE_CONS:
            object->car = gcMoveObject(interp, object->car, &stats);
            object->cdr = gcMoveObject(interp, object->cdr, &stats);
            break;
        case TYPE_LAMBDA:
        case TYPE_MACRO:
            object->params = gcMoveObject(interp, object->params, &stats);
            object->body = gcMoveObject(interp, object->body, &stats);
            object->env = gcMoveObject(interp, object->env, &stats);
            break;
        case TYPE_ENV:
            object->parent = gcMoveObject(interp, object->parent, &stats);
            object->vars = gcMoveObject(interp, object->vars, &stats);
            object->vals = gcMoveObject(interp, object->vals, &stats);
            break;
        case TYPE_MOVED:
            exceptionWithObject(interp, object, FLISP_GC_ERROR, "object already moved");
            break;
        default:
            exception(interp, FLISP_GC_ERROR, "unidentified object: %d", object->type);
        }
    }

    // swap from- and to-space
    void *swap = interp->memory->fromSpace;
    interp->memory->fromSpace = interp->memory->toSpace;
    interp->memory->toSpace = swap;

    fl_debug(interp, "collected %lu objects, skipped %lu, constants %lu, saved %lu bytes, memory: %lu/%lu",
             stats.moved, stats.skipped, stats.constant,
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

Object *memoryAllocObject(Interpreter *interp, ObjectType type, size_t size)
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
#if DEBUG_GC
    if (gc_always)
        gc(interp);
#else
    if ((interp->memory->fromOffset + size >= interp->memory->capacity))
        gc(interp);
#endif
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

Object *newObject(Interpreter *interp, ObjectType type)
{
    return memoryAllocObject(interp, type, sizeof(Object));
}

Object *newObjectFrom(Interpreter *interp, Object ** from)
{
    GC_CHECKPOINT;
    GC_TRACE(gcFrom, *from);
    Object *object = memoryAllocObject(interp, (*from)->type, (*from)->size);
    GC_RELEASE;
    memcpy(object, *gcFrom, (*gcFrom)->size);
    return object;
}

Object *newNumber(Interpreter *interp, double number)
{
    Object *object = newObject(interp, TYPE_NUMBER);
    object->number = number;
    return object;
}

Object *newObjectWithString(Interpreter *interp, ObjectType type, size_t size)
{
    size = (size > sizeof(((Object *) NULL)->string))
        ? size - sizeof(((Object *) NULL)->string)
        : 0;

    return memoryAllocObject(interp, type, sizeof(Object) + size);
}
/** unescapeString - copy a string, converting escaped symbols
 *
 * @param dst    destination
 * @param src    escaped string to copy
 * @param len    length of the string
 *
 */
void unescapeString(char *dst, char *src, size_t len)
{
    int r, w;
    for (r = 1, w = 0; r <= len; ++r) {
        if (src[r - 1] == '\\' && r < len) {
            switch (src[r]) {
            case '\\':
                dst[w++] = '\\';
                r++;
                break;
            case '"':
                dst[w++] = '"';
                r++;
                break;
            case 't':
                dst[w++] = '\t';
                r++;
                break;
            case 'r':
                dst[w++] = '\r';
                r++;
                break;
            case 'n':
                dst[w++] = '\n';
                r++;
                break;
            default:
                dst[w++] = '\\';
                break;
            }
        } else
            dst[w++] = src[r - 1];
    }
    dst[w] = '\0';
}
Object *newStringWithLength(Interpreter *interp, char *string, size_t length)
{
    int i, nEscapes = 0;

    if (length == 0)
        return empty;

    for (i = 1; i < length; ++i)
        if (string[i - 1] == '\\' && strchr("\\\"trn", string[i]))
            ++i, ++nEscapes;

    Object *object = newObjectWithString(interp, TYPE_STRING,
                                         length - nEscapes + 1);
    unescapeString(object->string, string, length);
    return object;
}

Object *newString(Interpreter *interp, char *string)
{
    return newStringWithLength(interp, string, strlen(string));
}

Object *newCons(Interpreter *interp, Object ** car, Object ** cdr)
{
    GC_CHECKPOINT;
    GC_TRACE(gcCar, *car);
    GC_TRACE(gcCdr, *cdr);
    Object *object = newObject(interp, TYPE_CONS);
    GC_RELEASE;
    object->car = *gcCar;
    object->cdr = *gcCdr;
    return object;
}

Object *newSymbolWithLength(Interpreter *interp, char *string, size_t length)
{
    Object *object;
    for (object = interp->symbols; object != nil; object = object->cdr)
        if (memcmp(object->car->string, string, length) == 0 && object->car->string[length] == '\0')
            return object->car;

    GC_CHECKPOINT;
    GC_TRACE(gcObject, newObjectWithString(interp, TYPE_SYMBOL, length + 1));
    memcpy((*gcObject)->string, string, length);
    (*gcObject)->string[length] = '\0';
    // Note: symbols are traced by gc() itself
    interp->symbols = newCons(interp, gcObject, &interp->symbols);
    GC_RELEASE;
    return *gcObject;
}

Object *newSymbol(Interpreter *interp, char *string)
{
    return newSymbolWithLength(interp, string, strlen(string));
}

Object *newObjectWithClosure(Interpreter *interp, ObjectType type, Object ** params, Object ** body, Object ** env)
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

    GC_CHECKPOINT;
    GC_TRACE(gcParams, *params);
    GC_TRACE(gcBody, *body);
    GC_TRACE(gcEnv, *env);
    Object *object = newObject(interp, type);
    GC_RELEASE;
    object->params = *gcParams;
    object->body = *gcBody;
    object->env = *gcEnv;
    return object;
}

Object *newLambda(Interpreter *interp, Object ** params, Object ** body, Object ** env)
{
    return newObjectWithClosure(interp, TYPE_LAMBDA, params, body, env);
}

Object *newMacro(Interpreter *interp, Object ** params, Object ** body, Object ** env)
{
    return newObjectWithClosure(interp, TYPE_MACRO, params, body, env);
}

Object *newPrimitive(Interpreter *interp, int primitive, char *name)
{
    Object *object = newObject(interp, TYPE_PRIMITIVE);
    object->primitive = primitive;
    object->name = name;
    return object;
}

Object *newEnv(Interpreter *interp, Object ** func, Object ** vals)
{
    int nArgs;

    GC_CHECKPOINT;
    GC_TRACE(gcFunc, *func);
    GC_TRACE(gcVals, *vals);
    Object *object = newObject(interp, TYPE_ENV);
    GC_RELEASE;
    // Note: this can be moved out of GC handling
    if ((*gcFunc) == nil)
        object->parent = object->vars = object->vals = nil;
    else {
        Object *param = (*gcFunc)->params, *val = *gcVals;

        for (nArgs = 0;; param = param->cdr, val = val->cdr, ++nArgs) {
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

        object->parent = (*gcFunc)->env;
        object->vars = (*gcFunc)->params;
        object->vals = *gcVals;
    }

    return object;
}

/** newStreamObject - create stream object from file descriptor and path
 *
 * @param fd .. FILE * stream descripter to register
 * @param name .. NULL or name of the file associated with fd
 * @param buf .. NULL or string to convert into an input file stream.
 */
Object *newStreamObject(Interpreter *interp, FILE *fd, char *path)
{
    char *buf;
    size_t len = strlen(path);

    if (!(buf = malloc(len+1)))
        exception(interp, FLISP_OOM, "failed to allocate %lu bytes for stream path", len);
    memcpy(buf, path, len+1);
    
    GC_CHECKPOINT;
    GC_TRACE(gcPath, newString(interp, buf));
    free(buf);
    Object *stream = newObject(interp, TYPE_STREAM);
    GC_RELEASE;
    stream->fd = fd;
    stream->buf = NULL;
    stream->len = 0;
    stream->path = *gcPath;

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

Object *envLookup(Interpreter *interp, Object *var, Object *env)
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

Object *envAdd(Interpreter *interp, Object ** var, Object ** val, Object ** env)
{
    GC_CHECKPOINT;
    GC_TRACE(gcEnv, *env);
    GC_TRACE(gcVar, *var);
    GC_TRACE(gcVal, *val);
    GC_TRACE(gcVars, newCons(interp, gcVar, &nil));
    Object *vals = newCons(interp, gcVal, &nil);
    GC_RELEASE;
    
    (*gcVars)->cdr = (*gcEnv)->vars, (*gcEnv)->vars = *gcVars;
    vals->cdr = (*gcEnv)->vals, (*gcEnv)->vals = vals;

    return *gcVal;
}

Object *envSet(Interpreter *interp, Object ** var, Object ** val, Object ** env)
{

    for (;;) {
        Object *vars = (*env)->vars, *vals = (*env)->vals;

        for (; vars->type == TYPE_CONS; vars = vars->cdr, vals = vals->cdr) {
            if (vars->car == *var)
                return vals->car = *val;
            if (vars->cdr == *var)
                return vals->cdr = *val;
        }

        if ((*env)->parent == nil) {
            GC_CHECKPOINT;
            GC_TRACE(gcEnv, *env);
            GC_RETURN(envAdd(interp, var, val, gcEnv));
        } else
            *env = (*env)->parent;
    }
}

Object *evalExpr(Interpreter *, Object **, Object **);

// READING S-EXPRESSIONS //////////////////////////////////////////////////////

// Input //////////

/** streamGetc - get a character from file descriptor
 *
 * @param interp  fLisp interpreter
 * @param fd      open readable file descriptor
 *
 * returns: the next character from the file descriptor, or EOF
 *
 * throws: FLISP_IO_ERROR
 */
int streamGetc(Interpreter *interp, FILE *fd)
{
    int c;
    if ((c = fgetc(fd)) == EOF)
        if (ferror(fd))
            exception(interp, FLISP_IO_ERROR, "failed to fgetc, errno: %d", errno);
    return c;
}
/** streamUngetc - push back the last streamGetc'd to file descriptor
 *
 * @param interp  fLisp interpreter
 * @param fd      open readable file descriptor
 *
 * returns: pushed back character or EOF on error
 *
 * throws: FLISP_IO_ERROR
 *
 */
int streamUngetc(Interpreter *interp, FILE *fd, int c)
{
    if ((c = ungetc(c, fd)) == EOF)
        exception(interp, FLISP_IO_ERROR, "failed to ungetc, errno: %d", errno);
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
 *
 * returns: number object
 *
 * throws: FLISP_READ_RANGE
 */
Object *newInteger(Interpreter *interp)
{
    long l;

    Object *number;

    addCharToBuf(interp, '\0');
    errno = 0;
    l = strtol(interp->buf, NULL, 10);
    if (errno == ERANGE)
        exception(interp, FLISP_READ_RANGE, "integer out of range,: %ld", l);
    number = newNumber(interp, l);
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
    double d;
    Object *number;

    addCharToBuf(interp, '\0');
    errno = 0;
    d = strtod(interp->buf, NULL);
    if (errno == ERANGE)
        exception(interp, FLISP_READ_RANGE, "integer out of range,: %f", d);
    // Note: purposely not dealing with NaN
    number = newNumber(interp, d);
    resetBuf(interp);
    return number;
}

// Reader /////////

/** streamPeek - get the next character from input file descriptor, but stay at the current offset
 *
 * @param interp  fLisp interpreter
 * @param fd      open readable file descriptor
 *
 * returns: next character in stream or EOF
 *
 * throws: FLISP_IO_ERROR
 */
int streamPeek(Interpreter *interp, FILE *fd)
{
    int c = streamGetc(interp, fd);
    if (c != EOF)
        streamUngetc(interp, fd, c);
    return c;
}

/** readNext - skip comments and spaces in input file
 *
 * @param interp  fLisp interpreter
 * @param fd      open readable file descriptor
 *
 * returns: next not space, not comment character
 *
 * throws: FLISP_IO_ERROR
 */
int readNext(Interpreter *interp, FILE *fd)
{
    for (;;) {
        int ch = streamGetc(interp, fd);
        if (ch == EOF)
            return ch;
        if (ch == ';')
            while ((ch = streamGetc(interp, fd)) != EOF && ch != '\n');
        if (isspace(ch))
            continue;
        return ch;
    }
}
/** peekNext - skip to last space or comment character in input file
 *
 * @param interp  fLisp interpreter
 * @param fd      open readable file descriptor
 *
 * returns: next not space, not comment character
 *
 * throws: FLISP_IO_ERROR
 */
int peekNext(Interpreter *interp, FILE *fd)
{
    int c = readNext(interp, fd);
    if (c != EOF)
        streamUngetc(interp, fd, c);
    return c;
}
/** readWhile - skip to next charater not fullfilling a predicate in input file
 *
 * @param interp     fLisp interpreter
 * @param fd      open readable file descriptor
 * @param predicate  function returning 0 if a character matches *predicate*
 *
 * returns: next character not fullfilling *predicate*
 *
 * throws: FLISP_IO_ERROR
 */
int readWhile(Interpreter *interp, FILE *fd, int (*predicate) (int ch))
{
    for (;;) {
        int ch = streamPeek(interp, fd);
        if (ch == EOF)
            return ch;
        if (!predicate(ch))
            return ch;
        (void)addCharToBuf(interp, streamGetc(interp, fd));
    }
}

/** readString - return string object from input file
 *
 * @param interp  fLisp interpreter
 * @param fd      open readable file descriptor
 *
 * throws: FLISP_IO_ERROR, FLISP_READ_INCOMPLETE, FLISP_OOM
 */
Object *readString(Interpreter *interp, FILE *fd)
{
    bool isEscaped;
    int ch;
    Object *string;

    resetBuf(interp);

    for (isEscaped = false;;) {
        ch = streamGetc(interp, fd);
        if (ch == EOF) {
            exception(interp, FLISP_READ_INCOMPLETE, "unexpected end of stream in string literal");
        }
        if (ch == '"' && !isEscaped) {
            string = newStringWithLength(interp, interp->buf, interp->len);
            resetBuf(interp);
            return string;
        }
        isEscaped = (ch == '\\' && !isEscaped);
        (void)addCharToBuf(interp, ch);
    }
}

/** readNumberOrSymbol - return integer, float or symbol from input file
 *
 * @param interp  fLisp interpreter
 * @param fd      open readable file descriptor
 *
 * returns: number object
 *
 * throws: FLISP_IO_ERROR, FLISP_READ_INCOMPLETE, FLISP_READ_RANGE, FLISP_OOM
 */
Object *readNumberOrSymbol(Interpreter *interp, FILE *fd)
{
    int ch = streamPeek(interp, fd);

    resetBuf(interp);

    // skip optional leading sign
    if (ch == '+' || ch == '-') {
        (void)addCharToBuf(interp, streamGetc(interp, fd));
        ch = streamPeek(interp, fd);
    }
    // try to read a number in integer or decimal format
    if (ch == '.' || isdigit(ch)) {
        if (isdigit(ch))
            ch = readWhile(interp, fd, isdigit);
        if (!isSymbolChar(ch))
            return newInteger(interp);
        if (ch == '.') {
            ch = streamGetc(interp, fd);
            if (isdigit(streamPeek(interp, fd))) {
                ch = readWhile(interp, fd, isdigit);
                if (!isSymbolChar(ch))
                    return newDouble(interp);
            }
        }
    }
    // non-numeric character encountered, read a symbol
    readWhile(interp, fd, isSymbolChar);
    Object * obj = newSymbolWithLength(interp, interp->buf, interp->len);
    resetBuf(interp);
    return obj;
}

Object *readExpr(Interpreter *, FILE *);

/** readList - return list from input file
 *
 * @param interp  fLisp interpreter
 * @param fd      open readable file descriptor
 *
 * returns: list
 *
 * throws: FLISP_IO_ERROR, FLISP_READ_INCOMPLETE, FLISP_READ_RANGE, FLISP_OOM
 */
Object *readList(Interpreter *interp, FILE *fd)
{
    Object *last = nil;
    Object *list = nil;
    for (;;) {
        int ch = readNext(interp, fd);
        if (ch == EOF)
            exception(interp, FLISP_READ_INCOMPLETE, "unexpected end of stream in list");
        if (ch == ')')
            return (list == nil) ? nil : reverseList(list);
        if (ch == '.' && !isSymbolChar(streamPeek(interp, fd))) {
            if (last == nil)
                exception(interp, FLISP_READ_INVALID, "unexpected dot at start of list");
            if ((ch = peekNext(interp, fd)) == ')')
                exception(interp, FLISP_READ_INVALID, "expected object at end of dotted list");
            GC_CHECKPOINT;
            GC_TRACE(gcList, list);
            last = readExpr(interp, fd);
            GC_RELEASE;
            if (!last)
                exception(interp, FLISP_READ_INCOMPLETE, "unexpected end of stream in dotted list");
            if ((ch = peekNext(interp, fd)) != ')')
                exception(interp, FLISP_READ_INVALID, "unexpected object at end of dotted list");
            readNext(interp, fd);
            list = reverseList(*gcList);
            (*gcList)->cdr = last;
            return list;
        } else {
            streamUngetc(interp, fd, ch);
            GC_CHECKPOINT;
            GC_TRACE(gcList, list);
            GC_TRACE(gcLast, last);
            *gcLast = readExpr(interp, fd);
            list = newCons(interp, gcLast, gcList);
            GC_RELEASE;
            last = *gcLast;
        }
    }
}
/** readUnary - return an unary operator together with the next
 *     expression from input file
 *
 * @param interp  fLisp interpreter
 * @param fd      open readable file descriptor
 * @param symbol  symbol to be inserted
 *
 * returns: unary operator expression
 *
 * throws: FLISP_IO_ERROR, FLISP_READ_INCOMPLETE, FLISP_READ_RANGE,
 *     FLISP_OOM
 */
Object *readUnary(Interpreter *interp, FILE *fd, char *symbol)
{
    if (peekNext(interp, fd) == EOF)
        exception(interp, FLISP_READ_INCOMPLETE, "unexpected end of stream in readUnary(%s)", symbol);

    GC_CHECKPOINT;
    GC_TRACE(gcSymbol, newSymbol(interp, symbol));
    GC_TRACE(gcObject, readExpr(interp, fd));

    *gcObject = newCons(interp, gcObject, &nil);
    *gcObject = newCons(interp, gcSymbol, gcObject);
    GC_RELEASE;
    
    return *gcObject;
}
/** readExpr - return next lisp sexp object from stream or from interpreter input file
 *
 * @param interp  fLisp interpreter
 * @param fd      open readable file descriptor
 *
 * returns: sexp object or NULL if EOF
 *
 * throws: FLISP_IO_ERROR, FLISP_READ_INCOMPLETE, FLISP_READ_RANGE,
 *     FLISP_OOM
 */
Object *readExpr(Interpreter *interp, FILE *fd)
{
    for (;;) {

        int ch = readNext(interp, fd);

        if (ch == EOF)
            return NULL;
        else if (ch == '\'' || ch == ':')
            return readUnary(interp, fd, "quote");
        else if (ch == '"')
            return readString(interp, fd);
        else if (ch == '(')
            return readList(interp, fd);
        else if (isSymbolChar(ch) && (ch != '.' || isSymbolChar(streamPeek(interp, fd)))) {
            (void)streamUngetc(interp, fd, ch);
            return readNumberOrSymbol(interp, fd);
        }
        else
            exception(interp, FLISP_READ_INVALID, "unexpected character, `%c'", ch);
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
Object *primitiveFread(Interpreter *interp, Object **args, Object **env)
{
    Object *eofv = nil;

    ONE_STREAM_ARG(fread);

    if ((*args)->cdr != nil)
        eofv = (*args)->cdr->car;

    if (stream->type != TYPE_STREAM)
        exceptionWithObject(interp, stream, FLISP_INVALID_VALUE, "(fread [fd ..]) - fd is not a stream: %s", typeName(stream));

    GC_CHECKPOINT;
    GC_TRACE(gcStream, stream);
    Object *result = readExpr(interp, (*gcStream)->fd);
    GC_RELEASE;
    
    if (result == NULL) {
        if (eofv == nil)
            exceptionWithObject(interp, *gcStream, FLISP_EOF, "(fread [..]) input exhausted");
        else
            result = eofv;
    }
    return result;
}


// EVALUATION /////////////////////////////////////////////////////////////////

typedef struct Primitive {
    char *name;
    int nMinArgs, nMaxArgs;
    Object *(*eval) (Interpreter *, Object ** args, Object **env);
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

Object *evalSetq(Interpreter *interp, Object ** args, Object ** env)
{
    if (*args == nil)
        return nil;

    Object * var = (*args)->car;

    if (var->type != TYPE_SYMBOL)
        exceptionWithObject(interp, var, FLISP_WRONG_TYPE, "(setq name value) - name is not a symbol");
    if (var == nil || var == t)
        exceptionWithObject(interp, var, FLISP_WRONG_TYPE, "(setq name value) name is a constant and cannot be set");

    GC_CHECKPOINT;
    GC_TRACE(gcEnv, *env);
    GC_TRACE(gcVar, var);
    GC_TRACE(gcRest, (*args)->cdr);
    GC_TRACE(gcVal, (*gcRest)->car);
    *gcVal = evalExpr(interp, gcVal, gcEnv);
    envSet(interp, gcVar, gcVal, gcEnv);
    GC_RELEASE;
    if ((*gcRest)->cdr == nil)
        return *gcVal;
    else
        return evalSetq(interp, &(*gcRest)->cdr, gcEnv);
}

Object *evalProgn(Interpreter *interp, Object ** args, Object ** env)
{
    if (*args == nil)
        return nil;

    if ((*args)->type != TYPE_CONS)
        exceptionWithObject(interp, *args, FLISP_WRONG_TYPE, "(progn args) args is not a list");

    if ((*args)->cdr == nil)
        return (*args)->car;

    GC_CHECKPOINT;
    GC_TRACE(gcObject, (*args)->car);
    GC_TRACE(gcArgs, (*args)->cdr);
    GC_TRACE(gcEnv, *env);
    evalExpr(interp, gcObject, gcEnv);
    GC_RETURN(evalProgn(interp, gcArgs, gcEnv));
}

/** (cond [clause ..]), clause: (pred [action]) - generic conditional
 *
 * (cond arg):
 * () => (cond)
 * (nil) => nil
 * (pred) => pred
 * (pred action) => nil|* .. nil|(progn action)
 */
Object *evalCond(Interpreter *interp, Object ** args, Object ** env)
{
    if (*args == nil)
        return nil;

    Object *clause = (*args)->car;
    Object *next_clause = (*args)->cdr;

    if (clause == nil)
        goto next_clause;

    if (clause->type != TYPE_CONS)
        exceptionWithObject(interp, clause, FLISP_WRONG_TYPE, "(cond clause ..) - is not a list: clause");

    Object *action = clause->cdr;
    if (action != nil && action->type != TYPE_CONS)
        exceptionWithObject(interp, clause, FLISP_WRONG_TYPE, "(cond (pred action) ..) action is not a list");

    Object *pred = clause->car;
    if (pred == nil)
        goto next_clause;

    GC_CHECKPOINT;
    GC_TRACE(gcPred, pred);
    GC_TRACE(gcAction, action);
    GC_TRACE(gcEnv, *env);
    GC_TRACE(gcNext, next_clause);
    *gcPred = evalExpr(interp, gcPred, gcEnv);
    GC_RELEASE;
    if (*gcPred == nil) {
        *env = *gcEnv;
        next_clause = *gcNext;
        goto next_clause;
    }
    if (*gcAction == nil)
        return *gcPred;

    if ((*gcAction)->type == TYPE_CONS)
        return evalProgn(interp, gcAction, gcEnv);
    return evalExpr(interp, gcAction, gcEnv);

next_clause:
    if (next_clause == nil) return nil;
    return evalCond(interp, &next_clause, env);
}

Object *evalLambda(Interpreter *interp, Object ** args, Object ** env)
{
    return newLambda(interp, &(*args)->car, &(*args)->cdr, env);
}

Object *evalMacro(Interpreter *interp, Object ** args, Object ** env)
{
    return newMacro(interp, &(*args)->car, &(*args)->cdr, env);
}

Object *expandMacro(Interpreter *interp, Object ** macro, Object ** args)
{
    GC_CHECKPOINT;
    GC_TRACE(gcMacro, *macro);
    GC_TRACE(gcEnv, newEnv(interp, gcMacro, args));
    Object *object = evalProgn(interp, &(*gcMacro)->body, gcEnv);
    object = evalExpr(interp, &object, gcEnv);
    GC_RELEASE;
    return object;
}

Object *expandMacroTo(Interpreter *interp, Object ** macro, Object ** args)
{
    Object *body = expandMacro(interp, macro, args);

    if (body->type == TYPE_CONS)
        return body;

    GC_CHECKPOINT;
    GC_TRACE(gcBody, body);
    GC_TRACE(gcCons, newCons(interp, &nil, &nil));
    (*gcCons)->car = newSymbol(interp, "progn");
    (*gcCons)->cdr = newCons(interp, gcBody, &nil);
    GC_RELEASE;
    return *gcCons;
}

Object *evalMacroExpand(Interpreter *interp, Object **args, Object ** env)
{
    if ((*args)->type != TYPE_CONS)
        return evalExpr(interp, args, env);

    GC_CHECKPOINT;
    GC_TRACE(gcArgs, (*args)->cdr);
    Object *macro = evalExpr(interp, &(*args)->car, env);
    GC_RELEASE;
    if (macro->type != TYPE_MACRO)
        return macro;

    return expandMacro(interp, &macro, gcArgs);
}

Object *evalList(Interpreter *interp, Object ** args, Object ** env)
{
    if ((*args)->type != TYPE_CONS)
        return evalExpr(interp, args, env);
    else {
        GC_CHECKPOINT;
        GC_TRACE(gcEnv, *env);
        GC_TRACE(gcCdr, (*args)->cdr);
        GC_TRACE(gcObject, evalExpr(interp, &(*args)->car, gcEnv));
        *gcCdr = evalList(interp, gcCdr, gcEnv);
        GC_RETURN(newCons(interp, gcObject, gcCdr));
    }
}

Primitive primitives[];

Object *evalExpr(Interpreter *interp, Object ** object, Object **env)
{
    GC_CHECKPOINT;
    GC_TRACE(gcObject, *object);
    GC_TRACE(gcEnv, *env);

    GC_TRACE(gcFunc, nil);
    GC_TRACE(gcArgs, nil);
    GC_TRACE(gcBody, nil);

    for (;;) {
        if ((*gcObject)->type == TYPE_SYMBOL)
            GC_RETURN(envLookup(interp, *gcObject, *gcEnv));
        if ((*gcObject)->type != TYPE_CONS)
            GC_RETURN(*gcObject);

        *gcFunc = (*gcObject)->car;
        *gcArgs = (*gcObject)->cdr;

        *gcFunc = evalExpr(interp, gcFunc, gcEnv);
        *gcBody = nil;

        if ((*gcFunc)->type == TYPE_LAMBDA) {
            *gcBody = (*gcFunc)->body;
            *gcArgs = evalList(interp, gcArgs, gcEnv);
            *gcEnv = newEnv(interp, gcFunc, gcArgs);
            *gcObject = evalProgn(interp, gcBody, gcEnv);
        } else if ((*gcFunc)->type == TYPE_MACRO) {
            *gcObject = expandMacroTo(interp, gcFunc, gcArgs);
        } else if ((*gcFunc)->type == TYPE_PRIMITIVE) {
            Primitive *primitive = &primitives[(*gcFunc)->primitive];
            int nArgs = 0;
            Object *args;

            for (args = *gcArgs; args != nil; args = args->cdr, nArgs++) {
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
                GC_RETURN((*gcArgs)->car);
            case PRIMITIVE_SETQ:
                GC_RETURN(evalSetq(interp, gcArgs, gcEnv));
            case PRIMITIVE_PROGN:
                *gcObject = evalProgn(interp, gcArgs, gcEnv);
                break;
            case PRIMITIVE_COND:
                *gcObject = evalCond(interp, gcArgs, gcEnv);
                break;
            case PRIMITIVE_LAMBDA:
                GC_RETURN(evalLambda(interp, gcArgs, gcEnv));
            case PRIMITIVE_MACRO:
                GC_RETURN(evalMacro(interp, gcArgs, gcEnv));
            case PRIMITIVE_MACROEXPAND:
                GC_RETURN(evalMacroExpand(interp, gcArgs, gcEnv));
            default:
                *gcArgs = evalList(interp, gcArgs, gcEnv);
                GC_RETURN(primitive->eval(interp, gcArgs, gcEnv));
            }
        } else {
            exceptionWithObject(interp, *gcFunc, FLISP_WRONG_TYPE, "is not a function");
        }
    }
}

Object *primitiveEval(Interpreter *interp, Object **args, Object **env)
{
    return evalExpr(interp, &(*args)->car, env);
}


// Write /////////////////////////////////////////////////////////////////////////////////

// Output ////////

/** writeChar - write character to file descriptor
 *
 * @param interp  fLisp interpreter
 * @param fd      open writeable file descriptor
 * @param ch      character to write
 *
 * throws: FLISP_IO_ERROR
 */
void writeChar(Interpreter *interp, FILE *fd, char ch)
{
    if (fd == NULL) return;

    if(fputc(ch, fd) == EOF)
        exception(interp, FLISP_IO_ERROR, "failed to write character %c, errno: %d", ch, errno);
}

/** writeString - write string to file descriptor
 *
 * @param interp  fLisp interpreter
 * @param fd      open writeable file descriptor
 * @param str     string to write
 *
 * throws: FLISP_IO_ERROR
 *
 */
void writeString(Interpreter *interp, FILE *fd, char *str)
{
    if (fd == NULL) return;

    int len = strlen(str);
    if(fprintf(fd, "%s", str) != len)
        exception(interp, FLISP_IO_ERROR, "failed to write %d files, errno: %d", len, errno);
}
/** writeFmt - write printf formatted string to file descriptor
 *
 * @param interp  fLisp interpreter
 * @param fd      open writeable file descriptor
 * @param format ... printf like format string
 *
 * throws: FLISP_IO_ERROR
 */
#ifdef __GNUC__
void writeFmt(Interpreter *, FILE *, char *format, ...)
    __attribute__ ((format(printf, 3, 4)));
#endif
void writeFmt(Interpreter *interp, FILE *fd, char *format, ...)
{
    int len;

    if (fd == NULL) return;

    va_list(args);
    va_start(args, format);
    len = vfprintf(fd, format, args);
    va_end(args);
    if (len < 0)
        exception(interp, FLISP_IO_ERROR, "failed to fprintf, errno: %d", errno);
}


// WRITING OBJECTS ////////////////////////////////////////////////////////////

/** lisp_write_object - format and write object to file descriptor
 *
 * @param interp  fLisp interpreter
 * @param fd      open writeable file descriptor
 * @param object  object to be serialized
 * @param readably  if true, write in a format which can be read back
 *
 * throws: FLISP_GC_ERROR, FLISP_IO_ERROR
 *
 */
void lisp_write_object(Interpreter *interp, FILE *fd, Object *object, bool readably)
{
    if (fd == NULL) return;

    switch (object->type) {
#define CASE(type, ...)                         \
        case type:                              \
            writeFmt(interp, fd, __VA_ARGS__);     \
            break
        CASE(TYPE_NUMBER, "%g", object->number);
        CASE(TYPE_SYMBOL, "%s", object->string);
        CASE(TYPE_PRIMITIVE, "#<Primitive %s>", object->name);
        CASE(TYPE_STREAM, "#<Stream %p, %s>", (void *) object->fd, object->path->string);
#undef CASE
    case TYPE_STRING:
        if (readably) {
            writeChar(interp, fd, '"');
            char *string;
            for (string = object->string; *string; ++string) {
                switch (*string) {
                case '"':
                    writeString(interp, fd, "\\\"");
                    break;
                case '\t':
                    writeString(interp, fd, "\\t");
                    break;
                case '\r':
                    writeString(interp, fd, "\\r");
                    break;
                case '\n':
                    writeString(interp, fd, "\\n");
                    break;
                case '\\':
                    writeString(interp, fd, "\\\\");
                    break;
                default:
                    writeChar(interp, fd, *string);
                    break;
                }
            }
            writeChar(interp, fd, '"');
        } else
            writeFmt(interp, fd, "%s", object->string);
        break;
    case TYPE_CONS:
        writeChar(interp, fd, '(');
        lisp_write_object(interp, fd, object->car, readably);
        while (object->cdr != nil) {
            object = object->cdr;
            if (object->type == TYPE_CONS) {
                writeChar(interp, fd, ' ');
                lisp_write_object(interp, fd, object->car, readably);
            } else {
                writeString(interp, fd, " . ");
                lisp_write_object(interp, fd, object, readably);
                break;
            }
        }
        writeChar(interp, fd, ')');
        break;
#define CASE(type, name, object)                                        \
        case type:                                                      \
            writeFmt(interp, fd, "#<%s ", name);                        \
            lisp_write_object(interp, fd, object, readably);            \
            writeChar(interp, fd, '>');                                 \
            break
        CASE(TYPE_LAMBDA, "Lambda", object->params);
        CASE(TYPE_MACRO, "Macro", object->params);
        //CASE(TYPE_ENV, "Env", object->vars);
#undef CASE
    case TYPE_ENV:
        writeFmt(interp, fd, "<#Env ");
        lisp_write_object(interp, fd, object->vars, readably);
        writeFmt(interp, fd, " - ");
        lisp_write_object(interp, fd, object->vals, readably);
        writeChar(interp, fd, '>');
        break;
    case TYPE_MOVED:
        exception(interp, FLISP_GC_ERROR, "won't write a garbage collected item");
        break;
    }
    fflush(fd);
}

/** (write object [[key value] ..]) - write object
 *
 * @param object         object to write.
 * @param key :stream    optional, use *value* as output stream.
 * @param key :readably  optional, if *value* not nil escape strings.
 *
 * returns: object
 *
 * throws: FLISP_PARAMETER_ERROR, FLISP_IO_ERROR, FLISP_GC_ERROR
 *
 * If no stream is specified the interpreters output file descriptor is used.
 * If the interpreters output file descriptor is NULL, no output is written.
 */
Object *primitiveWrite(Interpreter *interp, Object **args, Object **env)
{
    Object *stream = nil;
    bool readably = false;
    Object *obj = (*args)->car;
    FILE *fd;

    if ((*args)->cdr == nil)
        goto write;

    Object *list = (*args)->cdr;
    Object *key = nil;
    Object *value = nil;
    for (;list != nil; list = list->cdr) {
        key = list->car;
        if (key->type != TYPE_SYMBOL)
            exceptionWithObject(interp, *args, FLISP_PARAMETER_ERROR, "(write obj [[key val] ..]) - key is not a symbol");
        if (list->cdr == nil)
            exceptionWithObject(interp, *args, FLISP_PARAMETER_ERROR, "(write obj [[key val] ..]) - val is missing");
        list = list->cdr;
        value = list->car;
        if (!strcmp("stream", key->string)) {
            if (value->type != TYPE_STREAM)
                exceptionWithObject(interp, *args, FLISP_PARAMETER_ERROR, "(write obj [[key val] ..]) - value of key stream is not a stream");
            stream = value;
        } else if (!strcmp("readably", key->string)) {
            readably = (value != nil);
        } else
            exceptionWithObject(interp, *args, FLISP_PARAMETER_ERROR, "(write obj [[key val] ..]) - unknown key: %s", key->string);
    }
write:

    if (stream != nil)
        fd = stream->fd;
    else if (interp->output != NULL)
        fd = interp->output;
    else
        return obj;
    lisp_write_object(interp, fd, obj, readably);
    return obj;
}


// PRIMITIVES /////////////////////////////////////////////////////////////////

Object *primitiveNullP(Interpreter *interp, Object **args, Object **env)
{
    return ((*args)->car == nil) ? t : nil;
}

Object *primitiveConsP(Interpreter *interp, Object ** args, Object **env)
{
    return ((*args)->car->type == TYPE_CONS) ? t : nil;
}

Object *primitiveNumberP(Interpreter *interp, Object ** args, Object **env)
{
    Object *first = (*args)->car;
    return (first != nil && first->type == TYPE_NUMBER) ? t : nil;
}

Object *primitiveStringP(Interpreter *interp, Object ** args, Object **env)
{
    Object *first = (*args)->car;
    return (first != nil && first->type == TYPE_STRING) ? t : nil;
}

Object *primitiveSymbolP(Interpreter *interp, Object ** args, Object **env)
{
    return ((*args)->car->type == TYPE_SYMBOL) ? t : nil;
}

Object *primitiveSymbolName(Interpreter *interp, Object ** args, Object **env)
{
    Object *first = (*args)->car;
    if (first->type !=  TYPE_SYMBOL)
        exceptionWithObject(interp, first, FLISP_WRONG_TYPE, "(symbolp arg) - arg is not a symbol");
    size_t len = strlen(first->string);
    GC_CHECKPOINT;
    GC_TRACE(gcFirst, first);
    Object *string = newObjectWithString(interp, TYPE_STRING, len+1);
    GC_RELEASE;
    memcpy(string->string, (*gcFirst)->string, len+1); 
    return string;
}

Object *primitiveEq(Interpreter *interp, Object ** args, Object **env)
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
Object *primitiveCar(Interpreter *interp, Object ** args, Object **env)
{
    Object *first = (*args)->car;

    if (first == nil)
        return nil;
    else if (first->type == TYPE_CONS)
        return first->car;

    exceptionWithObject(interp, first, FLISP_WRONG_TYPE, "(car arg) - arg must be a list");
}
Object *primitiveCdr(Interpreter *interp, Object ** args, Object **env)
{
    Object *first = (*args)->car;

    if (first == nil)
        return nil;
    else if (first->type == TYPE_CONS)
        return first->cdr;

    exceptionWithObject(interp, first, FLISP_WRONG_TYPE, "(cdr arg) - arg must be a list");
}
Object *primitiveCons(Interpreter *interp, Object ** args, Object **env)
{
    return newCons(interp, &(*args)->car, &(*args)->cdr->car);
}

#if DEBUG_GC
// Introspection ///////
Object *primitiveGc(Interpreter *interp, Object ** args, Object **env)
{
    // Note:
    gc(interp);
    return t;
}
Object *primitiveGcTrace(Interpreter *interp, Object ** args, Object **env)
{
    return interp->gcTop;
}
Object *primitiveSymbols(Interpreter *interp, Object ** args, Object **env)
{
    return interp->symbols;
}
Object *primitiveGlobal(Interpreter *interp, Object ** args, Object **env)
{
    return interp->global;
}
Object *primitiveEnv(Interpreter *interp, Object ** args, Object **env)
{
    return *env;
}
#endif
Object *primitiveSignal(Interpreter *interp, Object ** args, Object **env)
{
    Object *first = (*args)->car;
    Object *second = (*args)->cdr;

    if (first->type != TYPE_SYMBOL)
        exceptionWithObject(interp, first , FLISP_WRONG_TYPE, "(signal type data) - type is not a symbol");

    GC_CHECKPOINT;
    GC_TRACE(gcFirst, first);
    Object *e = newCons(interp, gcFirst, &second);
    GC_RELEASE;
    exceptionWithObject(interp, e, FLISP_USER, "%s", (*gcFirst)->string);
}

// Math ///////

#define DEFINE_PRIMITIVE_ARITHMETIC(name, op, init)                     \
    Object *name(Interpreter *interp, Object **args, Object **env) {    \
        if (*args == nil)                                               \
            return newNumber(interp, init);                             \
        if ((*args)->car->type != TYPE_NUMBER)                          \
            exceptionWithObject(interp, (*args)->car, FLISP_WRONG_TYPE, "(" CPP_XSTR(op) " arg ..) - arg is not a number"); \
        Object *object;                                                 \
        GC_CHECKPOINT;                                                  \
        GC_TRACE(gcRest, *args);                                        \
        if ((*gcRest)->cdr == nil) {                                    \
            object = newNumber(interp, init);                           \
        } else {                                                        \
            object = newObjectFrom(interp, &(*gcRest)->car);              \
            *gcRest = (*gcRest)->cdr;                                   \
        }                                                               \
        GC_RELEASE;                                                     \
        for (; *gcRest != nil; *gcRest = (*gcRest)->cdr) {              \
            if ((*gcRest)->car->type != TYPE_NUMBER)                    \
                exceptionWithObject(interp, (*gcRest)->car, FLISP_WRONG_TYPE, "(" CPP_XSTR(op) " number arg ..) - arg is not a number"); \
                                                                        \
            object->number = object->number op (*gcRest)->car->number;  \
        }                                                               \
        return object;                                                  \
    }
    
DEFINE_PRIMITIVE_ARITHMETIC(primitiveAdd, +, 0)
DEFINE_PRIMITIVE_ARITHMETIC(primitiveSubtract, -, 0)
DEFINE_PRIMITIVE_ARITHMETIC(primitiveMultiply, *, 1)
DEFINE_PRIMITIVE_ARITHMETIC(primitiveDivide, /, 1)

Object *primitiveMod(Interpreter *interp, Object **args, Object **env) {
    if (*args == nil)
        return one;
    if ((*args)->car->type != TYPE_NUMBER)
        exceptionWithObject(interp, (*args)->car, FLISP_WRONG_TYPE, "(%% dividend ..) - dividend is not a number");

    Object *object;
    GC_CHECKPOINT;
    GC_TRACE(gcRest, *args);
    if ((*gcRest)->cdr == nil) {
        object = one;
    } else {
        object = newObjectFrom(interp, &(*gcRest)->car);
        *gcRest = (*args)->cdr;
    }
    GC_RELEASE;
    for (; *gcRest != nil; *gcRest = (*gcRest)->cdr) {
        if ((*gcRest)->car->type != TYPE_NUMBER)
            exceptionWithObject(interp, (*gcRest)->car, FLISP_WRONG_TYPE, "(%% dividend divisor ..) - divisor is not a number");
        
        object->number = (int)object->number % (int)(*gcRest)->car->number;
    }
    
    return object;
}

#define DEFINE_PRIMITIVE_RELATIONAL(name, op)                           \
    Object *name(Interpreter *interp, Object **args, Object **env) {    \
        if ((*args)->car->type != TYPE_NUMBER)                          \
            exceptionWithObject(interp, (*args)->car, FLISP_WRONG_TYPE, "(" CPP_XSTR(op) " arg ..) - arg is not a number"); \
                                                                        \
        Object *rest = *args;                                           \
        bool result = true;                                             \
        for (; result && rest->cdr != nil; rest = rest->cdr) {          \
            if (rest->cdr->car->type != TYPE_NUMBER)                    \
                exceptionWithObject(interp, rest->cdr->car, FLISP_WRONG_TYPE, "(" CPP_XSTR(op) " number arg ..) - arg is not a number"); \
            result &= rest->car->number op rest->cdr->car->number;      \
        }                                                               \
        return result ? t : nil;                                        \
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
 * throws: FLISP_OOM
 *
 */
Object *file_outputMemStream(Interpreter *interp)
{
    Object *stream = newStreamObject(interp, NULL, ">STRING");
    if (NULL == (stream->fd = open_memstream(&stream->buf, &stream->len)))
        exception(interp, FLISP_OOM, "failed to open_memstream() for memory output stream: %s", strerror(errno));
    fflush(stream->fd); // Note: sets stream->buf and stream->len to initial values.
    return stream;
}
/** file_inputMemStream - convert string to Lisp stream object
 *
 * @param interp  fLisp interpreter
 * @param string  string to read
 *
 * returns: Lisp stream object or nil on failure
 *
 * throws: FLISP_OOM
 */
Object *file_inputMemStream(Interpreter *interp, char *string)
{
    size_t len = strlen(string);
    char *buf = malloc(len+1);
    if (NULL == buf)
        exception(interp, FLISP_OOM, "failed to allocate string buffer for memory input stream: %s", strerror(errno));
    strncpy(buf, string, len);
    buf[len] = '\0';
    Object *stream = newStreamObject(interp, NULL, "<STRING");
    stream->buf = buf;
    stream->len = len;
    if (NULL == (stream->fd = fmemopen(stream->buf, stream->len, "r"))) {
        free(stream->buf);
        exception(interp, FLISP_OOM, "failed to fmemopen string for memory input stream: %s", strerror(errno));
    }
    return stream;
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
 * throws: FLISP_IO_ERROR, FLISP_INVALID_VALUE, FLISP_OOM
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
    return newStreamObject(interp, fd, path);
}
/** (fopen path mode) - return open stream object
 *
 * @param path    path to a file to open, string for memory input
 *     or "<num" / ">num" for file descriptor input / output.
 * @param mode    see fopen(3p). Additionally "<" / ">" for memory
 *     input / output.
 *
 * returns: stream object
 *
 * throws: FLISP_IO_ERROR, FLISP_INVALID_VALUE, FLISP_OOM
 */ 
 Object *primitiveFopen(Interpreter *interp, Object ** args, Object **env)
{
    TWO_STRING_ARGS(fopen);
    return file_fopen(interp, first->string, second->string);
}
/** file_fclose() - closes stream object
 *
 * @param interp  fLisp interpreter
 * @param stream  stream to close
 *
 * returns: 0 on success, else errno of fclose()
 */
int file_fclose(Interpreter *interp, Object *stream)
{
    fflush(stream->fd);
    int result = fclose(stream->fd) ? errno : 0;
    stream->fd = NULL;
    if (stream->buf != NULL) {
        free(stream->buf);
        stream->buf = NULL;
        stream->len = 0;
    }
    return result;
}
/** (fclose stream) - closes stream object
 *
 * @param interp  fLisp interpreter
 * @param stream  stream to close
 *
 * throws: FILSP_INVALID_VALUE, FLISP_IO_ERROR
 */
Object *primitiveFclose(Interpreter *interp, Object** args, Object **env)
{
    int result;

    ONE_STREAM_ARG(fclose);
    if (stream->type != TYPE_STREAM)
        exceptionWithObject(interp, stream, FLISP_INVALID_VALUE, "(fclose stream) - stream is not a stream object");
    if (stream->fd == NULL)
        exceptionWithObject(interp, stream, FLISP_INVALID_VALUE, "(fclose stream) - stream already closed");
    if ((result = file_fclose(interp, stream)))
        exceptionWithObject(interp, stream, FLISP_IO_ERROR, "(fclose stream) - failed to close: %s", strerror(result));
    return newNumber(interp, result);
}

#ifdef FLISP_FILE_EXTENSION
#include "file.c"
#endif

/* OS interface */

Object *fl_system(Interpreter *interp, Object ** args, Object **env) {

    ONE_STRING_ARG(system);
    return newNumber(interp, (double) system(arg->string));
}

Object *os_getenv(Interpreter *interp, Object ** args, Object **env)
{
    ONE_STRING_ARG(os.getenv);

    char *e = getenv(arg->string);
    if (e == NULL) return nil;
    return newStringWithLength(interp, e, strlen(e));
}

/* Strings */

Object *stringAppend(Interpreter *interp, Object ** args, Object **env)
{
    TWO_STRING_ARGS(string.append);

    int len1 = strlen(first->string);
    int len2 = strlen(second->string);
    char *new = strdup(first->string);
    new = realloc(new, len1 + len2 + 1);
    assert(new != NULL);
    memcpy(new + len1, second->string, len2);
    new[len1 + len2] = '\0';

    Object * str = newStringWithLength(interp, new, len1 + len2);
    free(new);

    return str;
}

Object *stringSubstring(Interpreter *interp, Object ** args, Object **env)
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
    Object * new = newStringWithLength(interp, sub, newlen);
    free(sub);

    return new;
}

Object *stringLength(Interpreter *interp, Object ** args, Object **env)
{
    ONE_STRING_ARG(string.length);

    return newNumber(interp, strlen(arg->string));
}

/* String/Number conversion */

Object *stringToNumber(Interpreter *interp, Object ** args, Object **env)
{
    Object *first = (*args)->car;

    ONE_STRING_ARG(string-to-number);

    double num = strtod(first->string, NULL);
    return newNumber(interp, num);
}

/*
 * XXX could be improved to handle integers and decimals better
 * for example 121323.000000 (%f) is ugly but so is 1.213230e+05 (%g)
 */
Object *numberToString(Interpreter *interp, Object ** args, Object **env)
{
    char buf[40];

    ONE_NUMBER_ARG(number-to-string);

    if (num->number == (long)num->number)
        sprintf(buf, "%ld", (long)num->number);
    else
        sprintf(buf, "%lf", num->number);

    return newStringWithLength(interp, buf, strlen(buf));
}


Object *asciiToString(Interpreter *interp, Object ** args, Object **env)
{
    char ch[2];
    ONE_NUMBER_ARG(ascii)
    if (num->type < 0 || num->type > 255)
        exceptionWithObject(interp, num, FLISP_INVALID_VALUE, "(ascii num) - num is not in range 0-255");

    ch[0] = (unsigned char)num->number;
    ch[1] = '\0';
    return newStringWithLength(interp, ch, 1);
}

Object *asciiToNumber(Interpreter *interp, Object ** args, Object **env)
{
    ONE_STRING_ARG(ascii->number);

    if (strlen(arg->string) < 1)
        exceptionWithObject(interp, arg, FLISP_INVALID_VALUE, "(ascii->number string) - string is empty");

    return newNumber(interp, (double)*arg->string);
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
    {"numberp", 1, 1, primitiveNumberP},
    {"stringp", 1, 1, primitiveStringP},
    {"symbolp", 1, 1, primitiveSymbolP},
    {"symbol-name", 1, 1, primitiveSymbolName},
    {"eq", 2, 2, primitiveEq},
    {"car", 1, 1, primitiveCar},
    {"cdr", 1, 1, primitiveCdr},
    {"cons", 2, 2, primitiveCons},
    {"fopen", 2, 2, primitiveFopen},
    {"fclose", 1, 1, primitiveFclose},
    {"fread", 0, 3, primitiveFread},
    {"eval", 1, 1, primitiveEval},
    {"write", 1, -1, primitiveWrite},
#if DEBUG_GC
    {"gc", 0, 0, primitiveGc},
    {"gctrace", 0, 0, primitiveGcTrace},
    {"symbols", 0, 0, primitiveSymbols},
    {"global", 0, 0, primitiveGlobal},
    {"env", 0, 0, primitiveEnv},
#endif
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
    {"string.length", 1, 1, stringLength},
    {"string.append", 2, 2, stringAppend},
    {"string.substring", 3, 3, stringSubstring},
    {"string-to-number", 1, 1, stringToNumber},
    {"number-to-string", 1, 1, numberToString},
    {"ascii", 1, 1, asciiToString},
    {"ascii->number", 1, 1, asciiToNumber},
    {"os.getenv", 1, 1, os_getenv},
    {"system", 1, 1, fl_system},
    FLISP_REGISTER_FILE_EXTENSION
#ifdef FLISP_FEMTO_EXTENSION
#include "femto.register.c"
#endif
};


// MAIN ///////////////////////////////////////////////////////////////////////

void initRootEnv(Interpreter *interp)
{
    GC_CHECKPOINT;
    GC_TRACE(gcEnv, newEnv(interp, &nil, &nil));
    
    interp->global = *gcEnv;

    // add constants
    envSet(interp, &nil, &nil, &interp->global);
    envSet(interp, &t, &t, &interp->global);

    // add primitives
    int nPrimitives = sizeof(primitives) / sizeof(primitives[0]);

    int i;
    GC_TRACE(gcVar, nil);
    GC_TRACE(gcVal, nil);
    for (i = 0; i < nPrimitives; ++i) {
        *gcVar = newSymbol(interp, primitives[i].name);
        *gcVal = newPrimitive(interp, i, primitives[i].name);

        envSet(interp, gcVar, gcVal, &interp->global);
    }
    GC_RELEASE;
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
    Interpreter *interp;

    if (lisp_interpreters != NULL)
        return NULL;

    interp = malloc(sizeof(Interpreter));
    if (interp == NULL) return NULL;

    if (size/2 < FLISP_MIN_MEMORY) {
        interp->result = FLISP_INVALID_VALUE;
        strncpy(interp->message,
                "fLisp needs at least" CPP_STR(FLISP_MIN_MEMORY)  "bytes to start up", sizeof(interp->message));
        return NULL;
    }

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

    // dynamic gc trace stack
    interp->gcTop = nil;

    /* gc setup */
    /*   symbols */
    Object *object;
    object = newCons(interp, &nil, &nil);
    object = newCons(interp, &t, &object);
    interp->symbols = object;
    /* global environment */
    initRootEnv(interp);
    /* gc can start from here */

    interp->catch = &interp->exceptionEnv;

    interp->next = interp;
    lisp_interpreters = interp;

    /* Add argv0 to the environment */
    Object *var = newSymbol(interp, "argv0");
    Object *val = newString(interp, *argv);
    (void)envSet(interp, &var, &val, &interp->global);

    /* Add argv to the environement */
    var = newSymbol(interp, "argv");
    val = nil;
    Object **i;
    for (i = &val; *++argv; i = &(*i)->cdr) {
        *i = newCons(interp, &nil, &nil);
        (*i)->car = newString(interp, *argv);
    }
    (void)envSet(interp, &var, &val, &interp->global);

    /* Add library_path to the environment */
    var = newSymbol(interp, "script_dir");
    val = newString(interp, library_path);
    envSet(interp, &var, &val, &interp->global);

    /* input stream */
    if (input) {
        interp->input = input;
        val = newStreamObject(interp, input, "STDIN");
        var = newSymbol(interp, "*INPUT*");
        (void)envSet(interp, &var, &val, &interp->global);
    }
    /* output stream */
    if (output) {
        interp->output = output;
        val = newStreamObject(interp, output, "STDOUT");
        var = newSymbol(interp, "*OUTPUT*");
        (void)envSet(interp, &var, &val, &interp->global);
    }
#if DEBUG_GC && DEBUG_GC_ALWAYS
    gc_always = true;
#endif
    return interp;
}

void lisp_destroy(Interpreter *interp)
{
    Interpreter *i;
    for (i=lisp_interpreters; i->next != interp; i=i->next);
    i->next = interp->next;
    i = NULL;

#if 0
    if (interp->memory->fromSpace)
        (void)munmap(interp->memory->fromSpace, interp->memory->capacity * 2);
    // Note: we do not know which one it is, so we free both.
    if (interp->memory->toSpace)
        (void)munmap(interp->memory->toSpace, interp->memory->capacity * 2);
#else
    if (interp->memory->fromSpace && interp->memory->toSpace) {
        if (interp->memory->fromSpace < interp->memory->toSpace)
            (void)munmap(interp->memory->fromSpace, interp->memory->capacity * 2);
        else
            (void)munmap(interp->memory->toSpace, interp->memory->capacity * 2);
    }
#endif
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
ResultCode lisp_eval(Interpreter *interp)
{
    if (interp->input == NULL) {
        interp->result = FLISP_INVALID_VALUE;
        strncpy(interp->message, "no input stream to evaluate", sizeof(interp->message));
        interp->object = nil;
        return interp->result;
    }

    interp->result = FLISP_OK;
    interp->message[0] = '\0';

    // start the garbage collector
    interp->gcTop = nil;
    GC_CHECKPOINT;
    GC_TRACE(gcObject, nil); // will not be released at all

    for (;;) {

        switch (setjmp(*interp->catch)) {
        case FLISP_OK:     break;
        case FLISP_RETURN: return FLISP_OK;
        default:           return FLISP_ERROR;
        }

        if ((*gcObject = readExpr(interp, interp->input)) == NULL)
            break;
        // lisp_write_object(interp, interp->output, *gcObject, true);
        *gcObject = evalExpr(interp, gcObject, &interp->global);
        interp->object = *gcObject;
        lisp_write_object(interp, interp->output, *gcObject, true);
        writeChar(interp, interp->output, '\n');
        if (interp->output) fflush(interp->output);
    }
    longjmp(*interp->catch, FLISP_RETURN);
    GC_RELEASE; // make the compiler happy
}

/** lisp_write_error - format error message and write to file
 *
 * @param interp  fLisp interpreter
 * @param fd      open writable file descriptor
 *
 * Formats the error message and inserts the error object if not nil,
 * then writes it to the given file descriptor.
 *
 * It is an error to use an interpreter without error.
 */
void lisp_write_error(Interpreter *interp, FILE *fd)
{
    if (interp->object == nil)
        fprintf(fd, "error: %s\n", interp->message);
    else {
        fprintf(fd, "%s", "error: '");
        lisp_write_object(interp, fd, interp->object, true);
        fprintf(fd, "', %s\n", interp->message);
    }
    fflush(fd);
}

/** lisp_eval_string() - interpret a string in Lisp
 *
 * @param interp  fLisp interpreter
 * @param input   string to evaluate
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
ResultCode lisp_eval_string(Interpreter *interp, char * input)
{
    FILE *fd, *prev;
    ResultCode result;
    char *buf;
    size_t len;

    fl_debug(interp, "lisp_eval_string(\"%s\")", input);

    if (NULL == (fd = fmemopen(input, strlen(input), "r")))  {
        strncpy(interp->message, "failed to allocate input stream", sizeof(interp->message));
        goto io_error;
    }
    prev = interp->input;
    interp->input = fd;
    result = lisp_eval(interp);
    interp->input = prev;
    (void)fclose(fd);

    fl_debug(interp, "lisp_eval_string() => %d", interp->result);
    if (result) {
        if (NULL == (fd = open_memstream(&buf, &len)))  {
            strncpy(interp->message, "failed to allocate output stream", sizeof(interp->message));
            goto io_error;
        }
        lisp_write_error(interp, fd);
    }
    return result;
io_error:
    interp->result = FLISP_IO_ERROR;
    interp->object = nil;
    return interp->result;
}

/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
