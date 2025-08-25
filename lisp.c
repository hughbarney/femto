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

#ifdef FLISP_DOUBLE_EXTENSION
#include "double.h"
#endif

#if !defined(MAP_ANONYMOUS) && defined(MAP_ANON)
#define MAP_ANONYMOUS        MAP_ANON
#endif

#define CPP_XSTR(s) CPP_STR(s)
#define CPP_STR(s) #s


/* Constants */
/* Fundamentals */
Object *nil =                    &(Object) { NULL, .string = "nil" };
Object *t =                      &(Object) { NULL, .string = "t" };
/* Types */
Object *type_integer =           &(Object) { NULL, .string  = "type-integer" };
Object *type_string =            &(Object) { NULL, .string  = "type-string" };
Object *type_symbol =            &(Object) { NULL, .string  = "type-symbol" };
Object *type_cons =              &(Object) { NULL, .string  = "type-cons" };
Object *type_lambda =            &(Object) { NULL, .string  = "type-lambda" };
Object *type_macro =             &(Object) { NULL, .string  = "type-macro" };
Object *type_primitive =         &(Object) { NULL, .string  = "type-primitive" };
Object *type_stream =            &(Object) { NULL, .string  = "type-stream" };
/* Exceptions */
Object *end_of_file =            &(Object) { NULL, .string = "end-of-file" };
Object *read_incomplete =        &(Object) { NULL, .string = "read-incomplete" };
Object *invalid_read_syntax =    &(Object) { NULL, .string = "invalid-read-syntax" };
Object *range_error =            &(Object) { NULL, .string = "range-error" };
Object *wrong_type_argument =    &(Object) { NULL, .string = "wrong-type-argument" };
Object *invalid_value =          &(Object) { NULL, .string = "invalid-value" };
Object *wrong_num_of_arguments = &(Object) { NULL, .string = "wrong-num-of-arguments" }; /* 'number' is two characters to long */
Object *arith_error =            &(Object) { NULL, .string = "arith-error"};
Object *io_error =               &(Object) { NULL, .string = "io-error" };
Object *out_of_memory =          &(Object) { NULL, .string = "out-of-memory" };
Object *gc_error =               &(Object) { NULL, .string = "gc-error" };
/* Internal */
Object *type_env =               &(Object) { NULL, .string  = "type-env" };
Object *type_moved =             &(Object) { NULL, .string  = "type-moved" };
Object *empty =                  &(Object) { NULL, .string = "\0" };
Object *one =                    &(Object) { NULL, .integer = 1 };
Object *okObject =               &(Object) { NULL };
Object *okMessage =              &(Object) { NULL };
Object *ok =                     &(Object) { NULL };

Constant flisp_constants[] = {
    /* Fundamentals */
    { &nil, &nil },
    { &t, &t, },
    /* Types */
    { &type_integer,   &type_integer  },
#ifdef FLISP_DOUBLE_EXTENSION
    { &type_double,    &type_double   },
#endif
    { &type_string,    &type_string   },
    { &type_symbol,    &type_symbol   },
    { &type_cons,      &type_cons     },
    { &type_lambda,    &type_lambda   },
    { &type_macro,     &type_macro    },
    { &type_primitive, &type_primitive},
    { &type_stream,    &type_stream   },
    /* Exceptions */
    { &end_of_file, &end_of_file },
    { &read_incomplete, &read_incomplete },
    { &invalid_read_syntax, &invalid_read_syntax },
    { &range_error, &range_error },
    { &wrong_type_argument, &wrong_type_argument },
    { &invalid_value, &invalid_value },
    { &wrong_num_of_arguments, &wrong_num_of_arguments },
    { &arith_error, &arith_error },
    { &io_error, &io_error },
    { &out_of_memory, &out_of_memory },
    { &gc_error, &gc_error }
};

/** Type codes:
 *
 * The public type of an object is a Lisp object type.  This cannot be
 * initialized statically so a map to the ObjectType enum is
 * maintained for type checking of arguments.
 */

Object **flisp_object_type[] = {
    &type_moved,
    &type_integer,
#ifdef FLISP_DOUBLE_EXTENSION
    &type_double,
#endif
    &type_string,
    &type_symbol,
    &type_cons,
    &type_lambda,
    &type_macro,
    &type_primitive,
    &type_env,
    &type_stream
};


typedef enum ResultCode {
    FLISP_OK,
    FLISP_ERROR,
    FLISP_RETURN,         /* successful return */
} ResultCode;

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


// EXCEPTION HANDLING /////////////////////////////////////////////////////////

void resetBuf(Interpreter *);

/** exceptionWithObject - break out of errors
 *
 * @param interp  interpreter in which the error occured.
 * @param object  object on which an error occured, set to nil if none.
 * @param result  result symbol corresponding to error type.
 * @param format ... printf style human readable error message
 *
 * *object* and *result* are stored in the interpreter structure.
 * The return code for longjmp is FLISP_ERROR
 *
 * The error message is formatted into the message buffer of the interpreter. If it has to
 * be truncated the last three characters are overwritten with "..."
 */
#ifdef __GNUC__
void exceptionWithObject(Interpreter *, Object *object, Object *result, char *format, ...)
    __attribute__ ((noreturn, format(printf, 4, 5)));
#endif
void exceptionWithObject(Interpreter *interp, Object *object, Object *result, char *format, ...)
{
    size_t written;

    interp->object = object;
    interp->result = result;
    resetBuf(interp);

    size_t len = sizeof(interp->msg_buf);
    va_list(args);
    va_start(args, format);
    written = vsnprintf(interp->msg_buf, len, format, args);
    va_end(args);
    if (written > len)
        strcpy(interp->msg_buf+len-4, "...");
    else if (written < 0)
        strncpy(interp->msg_buf, "failed to format error message", len);

    assert(interp->catch != NULL);
    longjmp(*interp->catch, FLISP_ERROR);
}

/** exception - break out of errors
 *
 * @param interp  interpreter in which the error occurred.
 * @param result  result symbol corresponding to error type.
 *
 * *result* is stored in the interpreter structures result field, nil in the object field
 * The longjmp return code is FLISP_ERROR.
 *
 *
 * The error message is formatted into the message buffer of the interpreter. If it has to
 * be truncated the last three characters are overwritten with "..."
 */
// #define exception(interp, result, ...)       exceptionWithObject(interp, nil, result, __VA_ARGS__)


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

Object *gcReturn(Interpreter *interp, Object *gcTop, Object *result)
{
    GC_RELEASE;
    return result;
}

/** gcCollectableObject - check if object is on heap
 *
 * @param interp  fLisp interpreter
 * @param object  object to inspect
 *
 * returns: true if object is on heap, false otherwise.
 *
 */
bool gcCollectableObject(Interpreter *interp, Object *object) {
    return (object >= (Object *) interp->memory->fromSpace &&
            object < (Object *) ((char *)interp->memory->fromSpace + interp->memory->fromOffset));
}

/** gcMoveObject - save a single object from garbage collection
 *
 * @param interp  fLisp interpreter
 * @param object  object to save
 *
 * returns: object at new location
 *
 */
typedef struct gcStats { size_t moved, constant, skipped; } gcStats;
Object *gcMoveObject(Interpreter *interp, Object *object, gcStats *stats)
{
    // skip object if it is not within from-space (i.e. on the stack)
    if (!gcCollectableObject(interp, object)) {
        stats->constant++;
        return object;
    }
    // if the object has already been moved, return its new location
    if (object->type == type_moved) {
        stats->skipped++;
        return object->forward;
    }
    stats->moved++;
    // copy object to to-space
    Object *forward = (Object *) ((char *)interp->memory->toSpace + interp->memory->toOffset);
    memcpy(forward, object, object->size);
    interp->memory->toOffset += object->size;

#if DEBUG_GC
    if (object->type == type_stream)
        fl_debug(interp, "moved stream %p, path %p/%s %s to %p",
                 (void *)object, (void *)object->path, object->path->string, object->path->type->string, (void *)forward);
    if (object->type == type_symbol)
        fl_debug(interp, "moved symbol %s", object->string);
#endif
    // mark object as moved and set forwarding pointer
    object->type = type_moved;
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
        fl_debug(interp, "moving gc traced object %p of type %s", (void *)object->car, object->car->type->string);
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

        if (object->type == type_stream) {
#if DEBUG_GC
            fl_debug(interp, "moving path %p/%s of stream %p", (void *)object->path, object->path->string, (void *)object);
#endif
            object->path = gcMoveObject(interp, object->path, &stats);
        } else if (object->type == type_cons) {
            object->car = gcMoveObject(interp, object->car, &stats);
            object->cdr = gcMoveObject(interp, object->cdr, &stats);
        } else if (object->type == type_lambda || object->type == type_macro) {
            object->params = gcMoveObject(interp, object->params, &stats);
            object->body = gcMoveObject(interp, object->body, &stats);
            object->env = gcMoveObject(interp, object->env, &stats);
        } else if (object->type == type_env) {
            object->parent = gcMoveObject(interp, object->parent, &stats);
            object->vars = gcMoveObject(interp, object->vars, &stats);
            object->vals = gcMoveObject(interp, object->vals, &stats);
        } else if (object->type == type_moved) {
            exceptionWithObject(interp, object, gc_error, "object already moved");
        } else if (object->type == type_integer
#ifdef FLISP_DOUBLE_EXTENSION
                   || object->type == type_double
#endif
                   || object->type == type_string || object->type == type_symbol
                   || object->type == type_primitive) {
        } else
            exception(interp, gc_error, "unidentified object: %s", object->type->string);
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

Object *memoryAllocObject(Interpreter *interp, Object *type, size_t size)
{
    size = memoryAlign(size, sizeof(void *));

    // allocate from- and to-space
    if (!interp->memory->fromSpace) {
        if (!(interp->memory->fromSpace = mmap(NULL, interp->memory->capacity * 2, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0))) {
            if (!interp->catch) return nil;
            exception(interp, out_of_memory, "mmap() failed, %s", strerror(errno));
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
        exception(interp, out_of_memory, "out of memory, %lu bytes", (unsigned long)size);
    }
    // allocate object in from-space
    Object *object = (Object *) ((char *)interp->memory->fromSpace + interp->memory->fromOffset);
    object->type = type;
    object->size = size;
    interp->memory->fromOffset += size;

    return object;
}


// CONSTRUCTING OBJECTS ///////////////////////////////////////////////////////

Object *newObject(Interpreter *interp, Object *type)
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

Object *newInteger(Interpreter *interp, int64_t number)
{
    Object *object = newObject(interp, type_integer);
    object->integer = number;
    return object;
}

Object *newObjectWithString(Interpreter *interp, Object *type, size_t size)
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

    Object *object = newObjectWithString(interp, type_string,
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
    Object *object = newObject(interp, type_cons);
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
    GC_TRACE(gcObject, newObjectWithString(interp, type_symbol, length + 1));
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

Object *newObjectWithClosure(Interpreter *interp, Object *type, Object ** params, Object ** body, Object **env)
{
    Object *list;

    for (list = *params; list->type == type_cons; list = list->cdr) {
        if (list->car->type != type_symbol)
            exceptionWithObject(interp, list->car, wrong_type_argument, "(lambda|macro params body) - param is not a symbol");
        if (list->car == nil || list->car == t)
            exceptionWithObject(interp, list->car, invalid_value, "(lambda|macro params body) - param cannot be used as a parameter");
    }

    if (list != nil && list->type != type_symbol)
        exceptionWithObject(interp, list, wrong_type_argument, "(lambda|macro params body) - param is not a symbol");

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

Object *newLambda(Interpreter *interp, Object ** params, Object ** body, Object **env)
{
    return newObjectWithClosure(interp, type_lambda, params, body, env);
}

Object *newMacro(Interpreter *interp, Object ** params, Object ** body, Object **env)
{
    return newObjectWithClosure(interp, type_macro, params, body, env);
}

Object *newPrimitive(Interpreter *interp, Primitive* primitive)
{
    Object *object = newObject(interp, type_primitive);
    object->primitive = primitive;
    return object;
}

Object *newEnv(Interpreter *interp, Object ** func, Object ** vals)
{
    int nArgs;
    Object *object = newObject(interp, type_env);
    if ((*func) == nil) {
        object->parent = object->vars = object->vals = nil;
        return object;
    }
    Object *param = (*func)->params, *val = *vals;

    for (nArgs = 0;; param = param->cdr, val = val->cdr, ++nArgs) {
        if (param == nil && val == nil)
            break;
        else if (param != nil && param->type == type_symbol)
            break;
        else if (val != nil && val->type != type_cons)
            exceptionWithObject(interp, val, wrong_type_argument, "(env) is not a list: val %d", nArgs);
        else if (param == nil && val != nil)
            exceptionWithObject(interp, *func, wrong_num_of_arguments, "(env) expects at most %d arguments", nArgs);
        else if (param != nil && val == nil) {
            for (; param->type == type_cons; param = param->cdr, ++nArgs);
            exceptionWithObject(interp, *func, wrong_num_of_arguments, "(env) expects at least %d arguments", nArgs);
        }
    }

    object->parent = (*func)->env;
    object->vars = (*func)->params;
    object->vals = *vals;

    return object;
}

/** newStreamObject - create stream object from file descriptor and path
 *
 * @param fd .. FILE * stream descriptor to register
 * @param name .. NULL or name of the file associated with fd
 * @param buf .. NULL or string to convert into an input file stream.
 */
Object *newStreamObject(Interpreter *interp, FILE *fd, char *path)
{
    char *buf;
    size_t len = strlen(path);

    if (!(buf = malloc(len+1)))
        exception(interp, out_of_memory, "failed to allocate %lu bytes for stream path", len);
    memcpy(buf, path, len+1);

    GC_CHECKPOINT;
    GC_TRACE(gcPath, newString(interp, buf));
    free(buf);
    Object *stream = newObject(interp, type_stream);
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

        for (; vars->type == type_cons; vars = vars->cdr, vals = vals->cdr)
            if (vars->car == var)
                return vals->car;

        if (vars == var)
            return vals;
    }

    exceptionWithObject(interp, var, invalid_value, "has no value");
}

Object *envAdd(Interpreter *interp, Object ** var, Object ** val, Object **env)
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

/** envSet sets a list of variables to corresponding values
 *
 * @param interp .. fLisp interpreter
 * @param var .. List of variable names
 * @param val .. List of values
 * @param env .. Environment in which to set the variables
 * @param top .. If true, set undefined variables in the top level environement, otherwise in the current.
 *
 * returns: the last assigned value.
 */
Object *envSet(Interpreter *interp, Object ** var, Object ** val, Object **env, bool top)
{

    for (;;) {
        Object *vars = (*env)->vars, *vals = (*env)->vals;

        for (; vars->type == type_cons; vars = vars->cdr, vals = vals->cdr) {
            if (vars->car == *var)
                return vals->car = *val;
            if (vars->cdr == *var)
                return vals->cdr = *val;
        }

        if ((*env)->parent == nil || !top) {
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
 * throws: io-error
 */
int streamGetc(Interpreter *interp, FILE *fd)
{
    int c;
    if ((c = fgetc(fd)) == EOF)
        if (ferror(fd))
            exception(interp, io_error, "failed to fgetc, errno: %d", errno);
    return c;
}
/** streamUngetc - push back the last streamGetc'd to file descriptor
 *
 * @param interp  fLisp interpreter
 * @param fd      open readable file descriptor
 *
 * returns: pushed back character or EOF on error
 *
 * throws: io-error
 *
 */
int streamUngetc(Interpreter *interp, FILE *fd, int c)
{
    if ((c = ungetc(c, fd)) == EOF)
        exception(interp, io_error, "failed to ungetc, errno: %d", errno);
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
            exception(interp, out_of_memory, "failed to allocate %ld bytes for readString buffer", interp->capacity);
        interp->buf = r;
    }
    interp->buf[interp->len++] = c;
    return interp->len;
}

/** readInteger - add an integer from the read buffer to the
 *     interpreter
 *
 * @param interp  fLisp interpreter
 *
 * returns: number object
 *
 * throws: range-error
 */
Object *readInteger(Interpreter *interp)
{
    int64_t n;

    Object *number;

    addCharToBuf(interp, '\0');
    errno = 0;
    /* Note: we allow hex and octal scanning, but the reader does not support it */
    n = strtoimax(interp->buf, NULL, 0);
    if (errno == ERANGE)
        exception(interp, range_error, "integer out of range,: %"PRId64, n);
    number = newInteger(interp, n);
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
 * throws: io-error
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
 * throws: io-error
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
 * throws: io-error
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
 * throws: io-error
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
 * throws: io-error, read-incomplete, out-of-memory
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
            exception(interp, read_incomplete, "unexpected end of stream in string literal");
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
 * throws: io-error, read-incomplete, range-error, out-of-memory
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
    // Note: readInteger support hex and octal, but we do not support it here.
    if (ch == '.' || isdigit(ch)) {
        if (isdigit(ch))
            ch = readWhile(interp, fd, isdigit);
        if (!isSymbolChar(ch))
            return readInteger(interp);
        if (ch == '.') {
            addCharToBuf(interp, ch);
            ch = streamGetc(interp, fd);
            if (isdigit(streamPeek(interp, fd))) {
                ch = readWhile(interp, fd, isdigit);
                if (!isSymbolChar(ch))
                    return readDouble(interp);
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
 * throws: io-error, read-incomplete, range-error, out-of-memory
 */
Object *readList(Interpreter *interp, FILE *fd)
{
    Object *last = nil;
    Object *list = nil;
    for (;;) {
        int ch = readNext(interp, fd);
        if (ch == EOF)
            exception(interp, read_incomplete, "unexpected end of stream in list");
        if (ch == ')')
            return (list == nil) ? nil : reverseList(list);
        if (ch == '.' && !isSymbolChar(streamPeek(interp, fd))) {
            if (last == nil)
                exception(interp, invalid_read_syntax, "unexpected dot at start of list");
            if ((ch = peekNext(interp, fd)) == ')')
                exception(interp, invalid_read_syntax, "expected object at end of dotted list");
            GC_CHECKPOINT;
            GC_TRACE(gcList, list);
            last = readExpr(interp, fd);
            GC_RELEASE;
            if (!last)
                exception(interp, read_incomplete, "unexpected end of stream in dotted list");
            if ((ch = peekNext(interp, fd)) != ')')
                exception(interp, invalid_read_syntax, "unexpected object at end of dotted list");
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
 * throws: io-error, read-incomplete, range-error,
 *     out-of-memory
 */
Object *readUnary(Interpreter *interp, FILE *fd, char *symbol)
{
    if (peekNext(interp, fd) == EOF)
        exception(interp, read_incomplete, "unexpected end of stream in readUnary(%s)", symbol);

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
 * throws: io-error, read-incomplete, range-error,
 *     out-of-memory
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
            exception(interp, invalid_read_syntax, "unexpected character, `%c'", ch);
    }
}

/** (read [stream [eofv]]) - read one object from input stream
 *
 * @param interp  fLisp interpreter.
 * @param stream  input stream to read, if nil, use interp input stream.
 * @param eofv    On EOF: if nil throw exception, else value to return.
 *
 * returns: Object
 *
 * throws: invalid-value, io-error, end-of-file
 */
Object *primitiveRead(Interpreter *interp, Object **args, Object **env)
{
    Object *eofv = nil;
    Object *stream = nil;
    FILE *fd = interp->input;

    GC_CHECKPOINT;
    if (*args != nil) {
        stream = (*args)->car;
        if (stream->type != type_stream)
            exceptionWithObject(interp, stream, invalid_value, "(read [fd ..]) - arg 1 expected %s, got: %s", type_stream->string, stream->type->string);
        fd = stream->fd;

        if ((*args)->cdr != nil)
            eofv = (*args)->cdr->car;
    }
    GC_TRACE(gcStream, stream);
    GC_TRACE(gcEofv, eofv);
    Object *result = readExpr(interp, fd);
    GC_RELEASE;

    if (result == NULL) {
        if (*gcEofv == nil)
            exceptionWithObject(interp, *gcStream, end_of_file , "(read [..]) input exhausted");
        else
            result = *gcEofv;
    }
    return result;
}


// EVALUATION /////////////////////////////////////////////////////////////////

// Special forms handled by evalExpr. Must be in the same order as above.
enum {
    PRIMITIVE_QUOTE,
    PRIMITIVE_SETQ,
    PRIMITIVE_DEFINE,
    PRIMITIVE_PROGN,
    PRIMITIVE_COND,
    PRIMITIVE_LAMBDA,
    PRIMITIVE_MACRO,
    PRIMITIVE_MACROEXPAND,
    PRIMITIVE_CATCH
};

/* Scheme-style tail recursive evaluation. evalProgn and evalCond
 * return the object in the tail recursive position to be evaluated by
 * evalExpr. Macros are expanded in-place the first time they are evaluated.
 */

/** evalSetVar sets or defines zero or more variables
 *
 * evalSetVar() is used to implement both (setq) and (define)
 *
 * @param interp .. fLisp interpeter
 * @param args .. List of arguments (var val ..)
 * @param env .. Environment where to set the variable

 * If top is set to true, an undefined variable is set in the top
 * level environment, otherwise it is set in the current environment.
 *
 * evalSetVar() is used to implement both (setq) and (define).
 *
 * throws: wrong-type-argument
 */
Object *evalSetVar(Interpreter *interp, Object **args, Object **env, bool top)
{
    if (*args == nil)
        return nil;

    Object * var = (*args)->car;

    if (var->type != type_symbol)
        exceptionWithObject(interp, var, wrong_type_argument, "(setq/define name value) - name is not a symbol");
    /* Note: we want to check for all constants here */
    //if (var == nil || var == t)
    if (!gcCollectableObject(interp, var))
        exceptionWithObject(interp, var, wrong_type_argument, "(setq/define name value) name is a constant and cannot be redefined");

    GC_CHECKPOINT;
    GC_TRACE(gcEnv, *env);
    GC_TRACE(gcVar, var);
    GC_TRACE(gcRest, (*args)->cdr);
    GC_TRACE(gcVal, (*gcRest)->car);
    *gcVal = evalExpr(interp, gcVal, gcEnv);
    envSet(interp, gcVar, gcVal, gcEnv, top);
    GC_RELEASE;
    if ((*gcRest)->cdr == nil)
        return *gcVal;
    else
        return evalSetVar(interp, &(*gcRest)->cdr, gcEnv, top);
}

Object *evalProgn(Interpreter *interp, Object **args, Object **env)
{
    if (*args == nil)
        return nil;

    if ((*args)->type != type_cons)
        exceptionWithObject(interp, *args, wrong_type_argument, "(progn args) args is not a list");

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
Object *evalCond(Interpreter *interp, Object **args, Object **env)
{
    if (*args == nil)
        return nil;

    Object *clause = (*args)->car;
    Object *next_clause = (*args)->cdr;

    if (clause == nil)
        goto next_clause;

    if (clause->type != type_cons)
        exceptionWithObject(interp, clause, wrong_type_argument, "(cond clause ..) - is not a list: clause");

    Object *action = clause->cdr;
    if (action != nil && action->type != type_cons)
        exceptionWithObject(interp, clause, wrong_type_argument, "(cond (pred action) ..) action is not a list");

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

    if ((*gcAction)->type == type_cons)
        return evalProgn(interp, gcAction, gcEnv);
    return evalExpr(interp, gcAction, gcEnv);

next_clause:
    if (next_clause == nil) return nil;
    return evalCond(interp, &next_clause, env);
}

Object *evalLambda(Interpreter *interp, Object **args, Object **env)
{
    return newLambda(interp, &(*args)->car, &(*args)->cdr, env);
}

Object *evalMacro(Interpreter *interp, Object **args, Object **env)
{
    return newMacro(interp, &(*args)->car, &(*args)->cdr, env);
}

Object *expandMacro(Interpreter *interp, Object ** macro, Object **args)
{
    GC_CHECKPOINT;
    GC_TRACE(gcBody, (*macro)->body);
    GC_TRACE(gcEnv, newEnv(interp, macro, args));

    GC_TRACE(gcObject, evalProgn(interp, gcBody, gcEnv));
    GC_RETURN(evalExpr(interp, gcObject, gcEnv));
}

Object *expandMacroTo(Interpreter *interp, Object ** macro, Object **args)
{
    Object *object = expandMacro(interp, macro, args);

    if (object->type == type_cons)
        return object;

    GC_CHECKPOINT;
    GC_TRACE(gcBody, object);
    GC_TRACE(gcCons, newCons(interp, gcBody, &nil));
    GC_TRACE(gcProg, newSymbol(interp, "progn"));
    GC_RETURN(newCons(interp, gcProg, gcCons));
}

Object *evalMacroExpand(Interpreter *interp, Object **args, Object **env)
{
    if ((*args)->type != type_cons)
        return evalExpr(interp, args, env);

    GC_CHECKPOINT;
    GC_TRACE(gcArgs, (*args)->cdr);
    GC_TRACE(gcMacro, evalExpr(interp, &(*args)->car, env));
    if ((*gcMacro)->type != type_macro)
        GC_RETURN(*gcMacro);

    GC_RETURN(expandMacro(interp, gcMacro, gcArgs));
}

Object *evalList(Interpreter *interp, Object **args, Object **env)
{
    if ((*args)->type != type_cons)
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

Object *evalCatch(Interpreter *interp, Object **args, Object **env)
{
    jmp_buf exceptionEnv, *prevEnv;
    Object *object;

    prevEnv = interp->catch;
    interp->catch = &exceptionEnv;
    interp->msg_buf[0] = '\0';
    interp->result = nil;
    if (setjmp(exceptionEnv)) {
        fl_debug(interp, "catch: %s, '%s'", interp->result->string, interp->msg_buf);
    } else {
        do {
            interp->object = evalExpr(interp, &(*args)->car, env);
        } while(0);
    }
    interp->catch = prevEnv;
    GC_CHECKPOINT;
    object = newCons(interp, &interp->object, &nil);
    GC_TRACE(gcObj, object);
    object = newString(interp, interp->msg_buf);
    GC_TRACE(gcMessage, object);
    *gcObj = newCons(interp, gcMessage, gcObj);
    object = interp->result;
    *gcObj = newCons(interp, &object, gcObj);
    GC_RELEASE;
    interp->object = *gcObj;
    interp->result = nil;
    return *gcObj;
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
        if ((*gcObject)->type == type_symbol)
            GC_RETURN(envLookup(interp, *gcObject, *gcEnv));
        if ((*gcObject)->type != type_cons)
            GC_RETURN(*gcObject);

        *gcFunc = (*gcObject)->car;
        *gcArgs = (*gcObject)->cdr;

        *gcFunc = evalExpr(interp, gcFunc, gcEnv);
        *gcBody = nil;

        if ((*gcFunc)->type == type_lambda) {
            *gcBody = (*gcFunc)->body;
            *gcArgs = evalList(interp, gcArgs, gcEnv);
            *gcEnv = newEnv(interp, gcFunc, gcArgs);
            *gcObject = evalProgn(interp, gcBody, gcEnv);
        } else if ((*gcFunc)->type == type_macro) {
            *gcObject = expandMacroTo(interp, gcFunc, gcArgs);
        } else if ((*gcFunc)->type == type_primitive) {
            Primitive *primitive = (*gcFunc)->primitive;
            int nArgs = 0;
            Object *args;

            for (args = *gcArgs; args != nil; args = args->cdr, nArgs++) {
                if (args->type != type_cons)
                    exceptionWithObject(interp, args, wrong_type_argument, "(%s args) - args is not a list: arg %d", primitive->name, nArgs);
                if (args->car->type == type_moved || args->cdr->type == type_moved)
                    exceptionWithObject(interp, args->car, gc_error, "(%s args) - arg %d is already disposed off", primitive->name, nArgs);
            }
            if (nArgs < primitive->nMinArgs)
                exceptionWithObject(interp, *gcFunc, wrong_num_of_arguments, "expects at least %d arguments", primitive->nMinArgs);
            if (nArgs > primitive->nMaxArgs && primitive->nMaxArgs >= 0)
                exceptionWithObject(interp, *gcFunc, wrong_num_of_arguments, "expects at most %d arguments", primitive->nMaxArgs);
            if (primitive->nMaxArgs < 0 && nArgs % -primitive->nMaxArgs)
                exceptionWithObject(interp, *gcFunc, wrong_num_of_arguments, "expects a multiple of %d arguments", -primitive->nMaxArgs);

            switch ((uintptr_t)primitive->eval) {
            case PRIMITIVE_QUOTE:
                GC_RETURN((*gcArgs)->car);
            case PRIMITIVE_SETQ:
                GC_RETURN(evalSetVar(interp, gcArgs, gcEnv, true));
            case PRIMITIVE_DEFINE:
                GC_RETURN(evalSetVar(interp, gcArgs, gcEnv, false));
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
            case PRIMITIVE_CATCH:
                GC_RETURN(evalCatch(interp, gcArgs, gcEnv));
            default:
                *gcArgs = evalList(interp, gcArgs, gcEnv);
                if (primitive->type_check)
                    for (args = *gcArgs; args != nil; args = args->cdr, nArgs++)
                        if (args->car->type != *flisp_object_type[primitive->type_check])
                            exceptionWithObject(interp, args->car, wrong_type_argument, "(%s args) - arg %d expected %s, got: %s",
                                                primitive->name, nArgs-1,
                                                (*flisp_object_type[primitive->type_check])->string,
                                                args->car->type->string
                                );
                GC_RETURN(primitive->eval(interp, gcArgs, gcEnv));
            }
        } else {
            exceptionWithObject(interp, *gcFunc, wrong_type_argument, "is not a function");
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
 * throws: io-error
 */
void writeChar(Interpreter *interp, FILE *fd, char ch)
{
    if (fd == NULL) return;

    if(fputc(ch, fd) == EOF)
        exception(interp, io_error, "failed to write character %c, errno: %d", ch, errno);
}

/** writeString - write string to file descriptor
 *
 * @param interp  fLisp interpreter
 * @param fd      open writeable file descriptor
 * @param str     string to write
 *
 * throws: io-error
 *
 */
void writeString(Interpreter *interp, FILE *fd, char *str)
{
    if (fd == NULL) return;

    int len = strlen(str);
    if(fprintf(fd, "%s", str) != len)
        exception(interp, io_error, "failed to write %d files, errno: %d", len, errno);
}
/** writeFmt - write printf formatted string to file descriptor
 *
 * @param interp  fLisp interpreter
 * @param fd      open writeable file descriptor
 * @param format ... printf like format string
 *
 * throws: io-error
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
        exception(interp, io_error, "failed to fprintf, errno: %d", errno);
}


// WRITING OBJECTS ////////////////////////////////////////////////////////////

/** lisp_write_object - format and write object to file descriptor
 *
 * @param interp  fLisp interpreter
 * @param fd      open writeable file descriptor
 * @param object  object to be serialized
 * @param readably  if true, write in a format which can be read back
 *
 * throws: gc-error, io-error
 *
 */
void lisp_write_object(Interpreter *interp, FILE *fd, Object *object, bool readably)
{
    if (fd == NULL) return;

    if (object->type == type_integer)
        writeFmt(interp, fd, "%"PRId64, object->integer);
#ifdef FLISP_DOUBLE_EXTENSION
    else if (object->type == type_double)
        writeFmt(interp, fd, "%g", object->number);
#endif
    else if (object->type == type_symbol)
        writeFmt(interp, fd, "%s", object->string);
    else if (object->type == type_primitive)
        writeFmt(interp, fd, "#<Primitive %s>", object->primitive->name);
    else if (object->type == type_stream)
        writeFmt(interp, fd, "#<Stream %p, %s>", (void *) object->fd, object->path->string);
    else if (object->type == type_string)
        if (!readably) writeFmt(interp, fd, "%s", object->string); else {
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
        }
    else if (object->type == type_cons) {
        writeChar(interp, fd, '(');
        lisp_write_object(interp, fd, object->car, readably);
        while (object->cdr != nil) {
            object = object->cdr;
            if (object->type == type_cons) {
                writeChar(interp, fd, ' ');
                lisp_write_object(interp, fd, object->car, readably);
            } else {
                writeString(interp, fd, " . ");
                lisp_write_object(interp, fd, object, readably);
                break;
            }
        }
        writeChar(interp, fd, ')');
    } else if (object->type == type_lambda) {
        writeFmt(interp, fd, "#<Lambda ");
        lisp_write_object(interp, fd, object->params, readably);
        writeChar(interp, fd, '>');
    } else if (object->type == type_macro) {
        writeFmt(interp, fd, "#<Macro ");
        lisp_write_object(interp, fd, object->params, readably);
        writeChar(interp, fd, '>');
    } else if (object->type == type_env) {
        /* Note: rather print as name = value pairs */
        writeFmt(interp, fd, "<#Env ");
        lisp_write_object(interp, fd, object->vars, readably);
        writeFmt(interp, fd, " - ");
        lisp_write_object(interp, fd, object->vals, readably);
        writeChar(interp, fd, '>');
    } else if (object->type == type_moved)
        exception(interp, gc_error, "won't write a garbage collected item");
    else
        exception(interp, gc_error, "unidentified object: %s", object->type->string);

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
 * throws: wrong-num-of-arguments, io-error, gc-error
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
        if (key->type != type_symbol)
            exceptionWithObject(interp, *args, wrong_num_of_arguments, "(write obj [[key val] ..]) - key is not a symbol");
        if (list->cdr == nil)
            exceptionWithObject(interp, *args, wrong_num_of_arguments, "(write obj [[key val] ..]) - val is missing");
        list = list->cdr;
        value = list->car;
        if (!strcmp("stream", key->string)) {
            if (value->type != type_stream)
                exceptionWithObject(interp, *args, wrong_num_of_arguments, "(write obj [[key val] ..]) - value of key stream is not a stream");
            stream = value;
        } else if (!strcmp("readably", key->string)) {
            readably = (value != nil);
        } else
            exceptionWithObject(interp, *args, wrong_num_of_arguments, "(write obj [[key val] ..]) - unknown key: %s", key->string);
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
    return (FLISP_ARG_ONE == nil) ? t : nil;
}

Object *primitiveTypeOf(Interpreter *interp, Object **args, Object **env)
{
    return (FLISP_ARG_ONE->type);
}

Object *primitiveConsP(Interpreter *interp, Object **args, Object **env)
{
    return (FLISP_ARG_ONE->type == type_cons) ? t : nil;
}

Object *primitiveSymbolName(Interpreter *interp, Object **args, Object **env)
{
    size_t len = strlen(FLISP_ARG_ONE->string);
    GC_CHECKPOINT;
    GC_TRACE(gcFirst, FLISP_ARG_ONE);
    Object *string = newObjectWithString(interp, type_string, len+1);
    GC_RELEASE;
    memcpy(string->string, (*gcFirst)->string, len+1);
    return string;
}

/** (same o1 o2) - object comparison
 *
 * @param o1  object
 * @param o2  object
 *
 * @returns t if o1 is the same object as o2, nil otherwise.
 */
Object *primitiveSame(Interpreter *interp, Object **args, Object **env)
{
    return (FLISP_ARG_ONE == FLISP_ARG_TWO) ? t : nil;
}

Object *primitiveCar(Interpreter *interp, Object **args, Object **env)
{
    if (FLISP_ARG_ONE == nil)
        return nil;
    if (FLISP_ARG_ONE->type == type_cons)
        return FLISP_ARG_ONE->car;
    exceptionWithObject(interp, FLISP_ARG_ONE, wrong_type_argument, "(car args) - arg 1 expected %s, got: %s", type_cons->string, FLISP_ARG_ONE->type->string);
}
Object *primitiveCdr(Interpreter *interp, Object **args, Object **env)
{
    if (FLISP_ARG_ONE == nil)
        return nil;
    if (FLISP_ARG_ONE->type == type_cons)
        return FLISP_ARG_ONE->cdr;
    exceptionWithObject(interp, FLISP_ARG_ONE, wrong_type_argument, "(cdr args) - arg 1 expected %s, got: %s", type_cons->string, FLISP_ARG_ONE->type->string);

}
Object *primitiveCons(Interpreter *interp, Object **args, Object **env)
{
    return newCons(interp, &(*args)->car, &(*args)->cdr->car);
}

#if DEBUG_GC
// Introspection ///////
Object *primitiveGc(Interpreter *interp, Object **args, Object **env)
{
    // Note:
    gc(interp);
    return t;
}
Object *primitiveGcTrace(Interpreter *interp, Object **args, Object **env)
{
    return interp->gcTop;
}
Object *primitiveSymbols(Interpreter *interp, Object **args, Object **env)
{
    return interp->symbols;
}
Object *primitiveGlobal(Interpreter *interp, Object **args, Object **env)
{
    return interp->global;
}
Object *primitiveEnv(Interpreter *interp, Object **args, Object **env)
{
    return *env;
}
#endif
Object *primitiveThrow(Interpreter *interp, Object **args, Object **env)
{
    Object *result = (*args)->car;
    Object *message = (*args)->cdr->car;
    Object *object = (*args)->cdr->cdr;

    if (result->type != type_symbol)
        exceptionWithObject(interp, result , wrong_type_argument, "(throw result message object) - result is not a symbol");
    if (message->type != type_string)
        exceptionWithObject(interp, message , wrong_type_argument, "(throw result message object) - message is not a string");

    if (object->type == type_cons)
        object = object->car;

    exceptionWithObject(interp, object, result, "%s", message->string);
}

// Integer Math //////

Object *integerAdd(Interpreter *interp, Object **args, Object **env)
{
    return newInteger(interp, FLISP_ARG_ONE->integer + FLISP_ARG_TWO->integer);
}
Object *integerSubtract(Interpreter *interp, Object **args, Object **env)
{
    return newInteger(interp, FLISP_ARG_ONE->integer - FLISP_ARG_TWO->integer);
}

Object *integerMultiply(Interpreter *interp, Object **args, Object **env)
{
    return newInteger(interp, FLISP_ARG_ONE->integer * FLISP_ARG_TWO->integer);
}

Object *integerDivide(Interpreter *interp, Object **args, Object **env)
{
    if (FLISP_ARG_TWO->integer)
        return newInteger(interp, FLISP_ARG_ONE->integer / FLISP_ARG_TWO->integer);
    exceptionWithObject(interp, FLISP_ARG_TWO, arith_error, "(i/ q d) - d: division by zero");
}

Object *integerMod(Interpreter *interp, Object **args, Object **env)
{
    if (FLISP_ARG_TWO->integer)
        return newInteger(interp, FLISP_ARG_ONE->integer % FLISP_ARG_TWO->integer);
    exceptionWithObject(interp, FLISP_ARG_TWO, arith_error, "(i%% q d) - d: division by zero");
}

Object *integerEqual(Interpreter *interp, Object **args, Object **env)
{
    return (FLISP_ARG_ONE->integer == FLISP_ARG_TWO->integer) ? t : nil;
}

Object *integerLess(Interpreter *interp, Object **args, Object **env)
{
    return (FLISP_ARG_ONE->integer < FLISP_ARG_TWO->integer) ? t : nil;
}

Object *integerLessEqual(Interpreter *interp, Object **args, Object **env)
{
    return (FLISP_ARG_ONE->integer <= FLISP_ARG_TWO->integer) ? t : nil;
}

Object *integerGreater(Interpreter *interp, Object **args, Object **env)
{
    return (FLISP_ARG_ONE->integer > FLISP_ARG_TWO->integer) ? t : nil;
}

Object *integerGreaterEqual(Interpreter *interp, Object **args, Object **env)
{
    return (FLISP_ARG_ONE->integer >= FLISP_ARG_TWO->integer) ? t : nil;
}



// STREAMS //////////////////////////////////////////////////

/* Minimal stream C-API for interpreter operation */

/** file_outputMemStream - create a memory based output stream
 *
 * @param interp  fLisp Interpreter
 *
 * returns: lisp stream object
 *
 * throws: out-of-memory
 *
 */
Object *file_outputMemStream(Interpreter *interp)
{
    Object *stream = newStreamObject(interp, NULL, ">STRING");
    if (NULL == (stream->fd = open_memstream(&stream->buf, &stream->len)))
        exception(interp, out_of_memory, "failed to open_memstream() for memory output stream: %s", strerror(errno));
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
 * throws: out-of-memory
 */
Object *file_inputMemStream(Interpreter *interp, char *string)
{
    size_t len = strlen(string);
    char *buf = malloc(len+1);
    if (NULL == buf)
        exception(interp, out_of_memory, "failed to allocate string buffer for memory input stream: %s", strerror(errno));
    strncpy(buf, string, len);
    buf[len] = '\0';
    Object *stream = newStreamObject(interp, NULL, "<STRING");
    stream->buf = buf;
    stream->len = len;
    if (NULL == (stream->fd = fmemopen(stream->buf, stream->len, "r"))) {
        free(stream->buf);
        exception(interp, out_of_memory, "failed to fmemopen string for memory input stream: %s", strerror(errno));
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
 * throws: io-error, invalid-value, out-of-memory
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
            exception(interp, io_error, "failed to open string as memory input stream: %d", errno);
        return stream;
    }
    if (strcmp(">", mode) == 0) {
        if (nil == (stream = file_outputMemStream(interp)))
            exception(interp, io_error, "failed to open memory output stream: %d", errno);
        return stream;
    }
    char c = path[0];
    if (c == '<' || c == '>') {
        char *end;
        errno = 0;
        long d = strtol(&path[1], &end, 0);
        if (errno || *end != '\0' || d < 0 || d > _POSIX_OPEN_MAX)
            exception(interp, invalid_value, "invalid I/O stream number: %s", &path[1]);
        if (NULL == (fd = fdopen((int)d, c == '<' ? "r" : "a")))
            exception(interp, io_error, "failed to open I/O stream %ld for %s", d, c == '<' ? "reading" : "writing");
    } else {
        if (NULL == (fd = fopen(path, mode)))
            exception(interp, io_error, "failed to open file '%s' with mode '%s', errno: %d", path, mode, errno);
    }
    return newStreamObject(interp, fd, path);
}
/** (open path[ mode]) - return open stream object
 *
 * @param path    path to a file to open, string for memory input
 *     or "<num" / ">num" for file descriptor input / output.
 * @param mode    see fopen(3p). Additionally "<" / ">" for memory
 *     input / output.
 *
 * returns: stream object
 *
 * throws: io-error, invalid-value, out-of-memory
 */
 Object *primitiveFopen(Interpreter *interp, Object **args, Object **env)
{
    char *mode = "r";

    if ((*args)->cdr != nil)
        mode = FLISP_ARG_TWO->string;
    return file_fopen(interp, FLISP_ARG_ONE->string, mode);
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
/** (close stream) - closes stream object
 *
 * @param interp  fLisp interpreter
 * @param stream  stream to close
 *
 * throws: FILSP_INVALID_VALUE, io-error
 */
Object *primitiveFclose(Interpreter *interp, Object**args, Object **env)
{
    int result;

    if (FLISP_ARG_ONE->fd == NULL)
        exceptionWithObject(interp, FLISP_ARG_ONE, invalid_value, "(fclose stream) - stream already closed");
    if ((result = file_fclose(interp, FLISP_ARG_ONE)))
        exceptionWithObject(interp, FLISP_ARG_ONE, io_error, "(fclose stream) - failed to close: %s", strerror(result));
    return newInteger(interp, result);
}

 Object *primitiveFinfo(Interpreter *interp, Object **args, Object **env)
{
    GC_CHECKPOINT;
    GC_TRACE(gcObject, (FLISP_ARG_ONE->fd == NULL) ?
             nil : newInteger(interp, (int64_t)FLISP_ARG_ONE->fd));
    *gcObject = newCons(interp, gcObject, &nil);
    GC_TRACE(gcBuffer, (FLISP_ARG_ONE->buf == NULL) ? nil : newString(interp, FLISP_ARG_ONE->buf));
    *gcObject = newCons(interp, gcBuffer, gcObject);
    GC_RETURN(newCons(interp, &(FLISP_ARG_ONE->path), gcObject));
}

#ifdef FLISP_FILE_EXTENSION
#include "file.c"
#endif

/* OS interface */

Object *fl_system(Interpreter *interp, Object **args, Object **env)
{
    return newInteger(interp, system(FLISP_ARG_ONE->string));
}

Object *os_getenv(Interpreter *interp, Object **args, Object **env)
{
    char *e = getenv(FLISP_ARG_ONE->string);
    if (e == NULL) return nil;
    return newStringWithLength(interp, e, strlen(e));
}

/* Strings */

// (string-append s a) 
Object *stringAppend(Interpreter *interp, Object **args, Object **env)
{
    int len1 = strlen(FLISP_ARG_ONE->string);
    int len2 = strlen(FLISP_ARG_TWO->string);
    char *new = strdup(FLISP_ARG_ONE->string);
    new = realloc(new, len1 + len2 + 1);
    assert(new != NULL);
    memcpy(new + len1, FLISP_ARG_TWO->string, len2);
    new[len1 + len2] = '\0';

    Object * str = newStringWithLength(interp, new, len1 + len2);
    free(new);

    return str;
}

// (substring ...)
Object *stringSubstring(Interpreter *interp, Object **args, Object **env)
{
    int64_t start = 0, end, len;

    if (FLISP_ARG_ONE->type != type_string)
        exceptionWithObject(interp, FLISP_ARG_ONE, wrong_type_argument, "(substring str [start [end]]) - arg 1 expected %s, got: %s", type_string->string, FLISP_ARG_ONE->type->string);
    len = strlen(FLISP_ARG_ONE->string);

    if (len == 0)
        return empty;

    end = start + len;

    if ((*args)->cdr != nil) {
        CHECK_TYPE(FLISP_ARG_TWO, type_integer, "(substring str [start [end]]) - start");
        start = (FLISP_ARG_TWO->integer);
        if (start < 0)
            start = end + start;
        if ((*args)->cdr->cdr != nil) {
            CHECK_TYPE(FLISP_ARG_THREE, type_integer, "(substring str [start [end]]) - end");
            if (FLISP_ARG_THREE->integer < 0)
                end = end + FLISP_ARG_THREE->integer;
            else
                end = FLISP_ARG_THREE->integer;
        }
    }

    if (start < 0 || start > len)
        exceptionWithObject(interp, FLISP_ARG_TWO, range_error, "(substring str [start [end]]) - start out of range");
    if (end < 0 || end > len)
        exceptionWithObject(interp, FLISP_ARG_THREE, range_error, "(substring str [start [end]]) - end out of range");
    if (start > end)
        exceptionWithObject(interp, FLISP_ARG_TWO, range_error, "(substring str [start [end]]) - end > start");
    if (start == end)
        return empty;

    char *sub = strdup(FLISP_ARG_ONE->string);
    int newlen = end - start;

    memcpy(sub, (FLISP_ARG_ONE->string + start), newlen+1);
    *(sub + newlen + 1) = '\0';
    Object * new = newStringWithLength(interp, sub, newlen);
    free(sub);

    return new;
}

// (eq s1 s2)
Object *stringEqual(Interpreter *interp, Object **args, Object **env)
{
    return !strcmp(FLISP_ARG_ONE->string, FLISP_ARG_TWO->string) ? t : nil;
}

// (length s)
Object *stringLength(Interpreter *interp, Object **args, Object **env)
{
    return newInteger(interp, strlen(FLISP_ARG_ONE->string));
}

/** (string-search needle haystack)
 *
 */
Object *stringSearch(Interpreter *interp, Object **args, Object **env)
{
    char *pos;
    
    pos = strstr(FLISP_ARG_TWO->string, FLISP_ARG_ONE->string);
    if (pos)
        return newInteger(interp, pos - FLISP_ARG_TWO->string);
    return nil;
}

Object *asciiToString(Interpreter *interp, Object **args, Object **env)
{
    char ch[2];
    if (FLISP_ARG_ONE->integer < 0 || FLISP_ARG_ONE->integer > 255)
        exceptionWithObject(interp, FLISP_ARG_ONE, range_error, "(ascii num) - num is not in range 0-255");

    ch[0] = (unsigned char)FLISP_ARG_ONE->integer;
    ch[1] = '\0';
    return newStringWithLength(interp, ch, 1);
}

Object *asciiToInteger(Interpreter *interp, Object **args, Object **env)
{
    if (strlen(FLISP_ARG_ONE->string) < 1)
        exceptionWithObject(interp, FLISP_ARG_ONE, invalid_value, "(ascii->number string) - string is empty");

    return newInteger(interp, (int64_t)*FLISP_ARG_ONE->string);
}


#ifdef FLISP_FEMTO_EXTENSION
#include "femto.primitives.c"
#endif

Primitive primitives[] = {
    {"quote",         1,  1, 0, (LispEval) PRIMITIVE_QUOTE /* special form */ },
    {"setq",          0, -2, 0, (LispEval) PRIMITIVE_SETQ  /* special form */ },
    {"define",        0, -2, 0, (LispEval) PRIMITIVE_DEFINE /* special form */ },
    {"progn",         0, -1, 0, (LispEval) PRIMITIVE_PROGN /* special form */ },
    {"cond",          0, -1, 0, (LispEval) PRIMITIVE_COND  /* special form */ },
    {"lambda",        1, -1, 0, (LispEval) PRIMITIVE_LAMBDA /* special form */ },
    {"macro",         1, -1, 0, (LispEval) PRIMITIVE_MACRO  /* special form */ },
    {"macroexpand-1", 1,  2, 0, (LispEval) PRIMITIVE_MACROEXPAND /* special form */ },
    {"catch",         1,  1, 0, (LispEval) PRIMITIVE_CATCH  /*special form */ },
    {"null",          1,  1, 0,         primitiveNullP},
    {"type-of",       1,  1, 0,         primitiveTypeOf},
    {"consp",         1,  1, 0,         primitiveConsP},
    {"symbol-name",   1,  1, TYPE_SYMBOL, primitiveSymbolName},
    {"same",          2,  2, 0,         primitiveSame},
    {"car",           1,  1, 0,         primitiveCar}, /* Note: nil|cons */
    {"cdr",           1,  1, 0,         primitiveCdr}, /* Note: nil|cons */
    {"cons",          2,  2, 0,         primitiveCons},
    {"open",          1,  2, TYPE_STRING, primitiveFopen},
    {"close",         1,  1, TYPE_STREAM, primitiveFclose},
    {"file-info",     1,  1, TYPE_STREAM, primitiveFinfo},
    {"read",          0,  2, 0,         primitiveRead},
    {"eval",          1,  1, 0,         primitiveEval},
    {"write",         1, -1, 0,         primitiveWrite},
#if DEBUG_GC
    {"gc",            0,  0, 0,         primitiveGc},
    {"gctrace",       0,  0, 0,         primitiveGcTrace},
    {"symbols",       0,  0, 0,         primitiveSymbols},
    {"global",        0,  0, 0,         primitiveGlobal},
    {"env",           0,  0, 0,         primitiveEnv},
#endif
    {"throw",         2,  3, 0,         primitiveThrow},
    {"i+",            2,  2, TYPE_INTEGER, integerAdd},
    {"i-",            2,  2, TYPE_INTEGER, integerSubtract},
    {"i*",            2,  2, TYPE_INTEGER, integerMultiply},
    {"i/",            2,  2, TYPE_INTEGER, integerDivide},
    {"i%",            2,  2, TYPE_INTEGER, integerMod},
    {"i=",            2,  2, TYPE_INTEGER, integerEqual},
    {"i<",            2,  2, TYPE_INTEGER, integerLess},
    {"i<=",           2,  2, TYPE_INTEGER, integerLessEqual},
    {"i>",            2,  2, TYPE_INTEGER, integerGreater},
    {"i>=",           2,  2, TYPE_INTEGER, integerGreaterEqual},
    {"string-equal",  2,  2, TYPE_STRING, stringEqual},
    {"string-length", 1,  1, TYPE_STRING, stringLength},
    {"string-append", 2,  2, TYPE_STRING, stringAppend},
    {"substring",     1,  3, 0,           stringSubstring},
    {"string-search", 2,  2, TYPE_STRING, stringSearch}, 
    {"ascii",         1,  1, TYPE_INTEGER, asciiToString},
    {"ascii->number", 1,  1, TYPE_STRING, asciiToInteger},
    {"os.getenv",     1,  1, TYPE_STRING, os_getenv},
    {"system",        1,  1, TYPE_STRING, fl_system},
    FLISP_REGISTER_FILE_EXTENSION
#ifdef FLISP_FEMTO_EXTENSION
#include "femto.register.c"
#endif
};


// MAIN ///////////////////////////////////////////////////////////////////////

void initRootEnv(Interpreter *interp)
{
    int i;

    GC_CHECKPOINT;
    GC_TRACE(gcEnv, newEnv(interp, &nil, &nil));

    interp->global = *gcEnv;

    // add constants
    int nConstants = sizeof(flisp_constants) / sizeof(flisp_constants[0]);
    for (i = 0; i < nConstants; i++) {
        (*flisp_constants[i].symbol)->type = type_symbol;
        envSet(interp, flisp_constants[i].symbol, flisp_constants[i].value, &interp->global, true);
        interp->symbols = newCons(interp, flisp_constants[i].symbol, &interp->symbols);
    }

    /* Add internal objects */
    type_env->type = type_symbol;
    type_moved->type = type_symbol;
    empty->type = type_string;
    one->type = type_integer;
#ifdef FLISP_DOUBLE_EXTENSION
    double_one->type = type_double;
#endif
    okObject->type = type_cons;
    okObject->car = okObject->cdr = nil;
    okMessage->type = type_cons;
    okMessage->car = empty;
    okMessage->cdr = okObject;
    ok->type = type_cons;
    ok->car = nil;
    ok->cdr = okMessage;
    
    // add primitives
    GC_TRACE(gcVar, nil);
    GC_TRACE(gcVal, nil);
    int nPrimitives = sizeof(primitives) / sizeof(primitives[0]);
    for (i = 0; i < nPrimitives; ++i) {
        *gcVar = newSymbol(interp, primitives[i].name);
        *gcVal = newPrimitive(interp, &primitives[i]);

        envSet(interp, gcVar, gcVal, &interp->global, true);
    }

    for (Primitive *entry = flisp_double_primitives; entry->name != NULL; entry++) {
        *gcVar = newSymbol(interp, entry->name);
        *gcVal = newPrimitive(interp, entry);

        envSet(interp, gcVar, gcVal, &interp->global, true);
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
        interp->result = invalid_value;
        strncpy(interp->msg_buf,
                "fLisp needs at least" CPP_STR(FLISP_MIN_MEMORY)  "bytes to start up", sizeof(interp->msg_buf));
        return NULL;
    }

    interp->debug = debug;

    Memory *memory = newMemory(size);
    if (memory == NULL) {
        interp->result = out_of_memory;
        strncpy(interp->msg_buf,
                "failed to allocate memory for the interpreter",  sizeof(interp->msg_buf));
        return NULL;
    }
    interp->memory = memory;

    interp->object = ok;
    interp->msg_buf[0] = '\0';
    interp->result = nil;

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
    fl_debug(interp, "lisp_init: %lu/%lu bytes allocated before gc", interp->memory->fromOffset, interp->memory->capacity);
    /* gc region start */

    /* global environment */
    initRootEnv(interp);

    interp->catch = &interp->exceptionEnv;

    interp->next = interp;
    lisp_interpreters = interp;

    /* Add argv0 to the environment */
    Object *var = newSymbol(interp, "argv0");
    Object *val = newString(interp, *argv);
    (void)envSet(interp, &var, &val, &interp->global, true);

    /* Add argv to the environement */
    var = newSymbol(interp, "argv");
    val = nil;
    Object **i;
    for (i = &val; *++argv; i = &(*i)->cdr) {
        *i = newCons(interp, &nil, &nil);
        (*i)->car = newString(interp, *argv);
    }
    (void)envSet(interp, &var, &val, &interp->global, true);

    /* Add library_path to the environment */
    var = newSymbol(interp, "script_dir");
    val = newString(interp, library_path);
    envSet(interp, &var, &val, &interp->global, true);

    /* input stream */
    if (input) {
        interp->input = input;
        val = newStreamObject(interp, input, "STDIN");
        var = newSymbol(interp, "*INPUT*");
        (void)envSet(interp, &var, &val, &interp->global, true);
    }
    /* output stream */
    if (output) {
        interp->output = output;
        val = newStreamObject(interp, output, "STDOUT");
        var = newSymbol(interp, "*OUTPUT*");
        (void)envSet(interp, &var, &val, &interp->global, true);
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

    if (interp->debug)
        fclose(interp->debug);
    free(interp->memory);
    free(interp);
}

/** lisp_eval - protected evaluation of input stream
 *
 * @param interp  fLisp interpreter
 * @param stream  open readable stream object
 * @param gcRoots gc root object
 *
 */
void lisp_eval(Interpreter *interp)
{
    if (interp->input == NULL) {
        interp->result = invalid_value;
        strncpy(interp->msg_buf, "no input stream to evaluate", sizeof(interp->msg_buf));
        interp->object = nil;
        return;
    }

    interp->result = nil;
    interp->msg_buf[0] = '\0';

    // start the garbage collector
    interp->gcTop = nil;
    GC_CHECKPOINT;
    GC_TRACE(gcObject, nil); // will not be released at all

    for (;;) {
        interp->result = nil;
        interp->msg_buf[0] = '\0';
        switch (setjmp(*interp->catch)) {
        case FLISP_OK: break;
        case FLISP_RETURN: return;
        default:
            fl_debug(interp, "error: %s, '%s'", interp->result->string, interp->msg_buf);
            GC_RELEASE;
            return;
        }
        if ((*gcObject = readExpr(interp, interp->input)) == NULL) {
            interp->result = nil;
            interp->msg_buf[0] = '0';
            return;
        }
        lisp_write_object(interp, interp->debug, *gcObject, true);
        writeChar(interp, interp->debug, '\n');
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
        fprintf(fd, "error: %s\n", interp->msg_buf);
    else {
        fprintf(fd, "%s", "error: '");
        lisp_write_object(interp, fd, interp->object, true);
        fprintf(fd, "', %s\n", interp->msg_buf);
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
 * - interp->result is set to the result symbol of the evaluation, nil if succesful.
 * - interp->object is set to the resulting object
 *
 * If an error occurs during evaluation:
 *
 * - interp->object is set to the object causing the exception, or nil.
 * - interp->msg_buf is set to an error message.
 *
 */
void lisp_eval_string(Interpreter *interp, char * input)
{
    FILE *fd, *prev;

    fl_debug(interp, "lisp_eval_string(\"%s\")", input);

    if (NULL == (fd = fmemopen(input, strlen(input), "r")))  {
        strncpy(interp->msg_buf, "failed to allocate input stream", sizeof(interp->msg_buf));
        goto io_error;
    }
    prev = interp->input;
    interp->input = fd;
    lisp_eval(interp);
    interp->input = prev;
    (void)fclose(fd);
    if (interp->result == nil) {
        fl_debug(interp, "lisp_eval_string() result: nil");
        lisp_write_object(interp, interp->debug, interp->object, true);
    }
    else
        fl_debug(interp, "lisp_eval_string() result: error: %s", interp->msg_buf);
    return;

io_error:
    interp->result = io_error;
    interp->object = nil;
    return;
}


void lisp_write_error2(Interpreter *interp, FILE *fd)
{
    fprintf(fd, "error: '");
    lisp_write_object(interp, fd, interp->object->cdr->cdr->car, true);
    fprintf(fd, "', %s\n", interp->object->cdr->car->string);
    fflush(fd);
}

void lisp_eval2(Interpreter *interp)
{
    // reset the garbage collector
    interp->gcTop = nil;
    GC_CHECKPOINT;
    GC_TRACE(gcObject, nil);
    Primitive readPrimitive = { "read", 0, 2, 0, primitiveRead };
    Object *readObject =     &(Object) { type_primitive, .primitive = &readPrimitive, .type_check = nil };
    Object *readInvocation = &(Object) { type_cons, .car = readObject, .cdr = nil };
    Object *readApply =      &(Object) { type_cons, .car = readInvocation, .cdr = nil };

    Object *okObject = &(Object) { type_cons, .car = nil, .cdr = nil };
    Object *okString = &(Object) { type_cons, .car = empty, .cdr = okObject };
    Object *result = &(Object) { type_cons, .car = nil, .cdr = okString };

    for (;;) {
        /* read */
        *gcObject = evalCatch(interp, &readApply, &interp->global);
        if (interp->object->car == end_of_file) {
            interp->object = result;
            break;
        }
        if (interp->object->car != nil)
            break;
        /* eval */
        *gcObject = newCons(interp, &interp->object->cdr->cdr->car, &nil);
        *gcObject = evalCatch(interp, gcObject, &interp->global);
        if ((*gcObject)->car != nil)
            break;
        lisp_write_object(interp, interp->output, (*gcObject)->cdr->cdr->car, true);
        writeChar(interp, interp->output, '\n');
        if (interp->output) fflush(interp->output);
        result = *gcObject;
    }
    GC_RELEASE;
}
void lisp_eval_string2(Interpreter *interp, char *input)
{

    FILE *fd, *prev;
    fl_debug(interp, "lisp_eval_string2(\"%s\")", input);

    if (NULL == (fd = fmemopen(input, strlen(input), "r")))  {
        strncpy(interp->msg_buf, "failed to allocate input stream", sizeof(interp->msg_buf));
        interp->result = io_error;
        interp->object = nil;
        return;
    }
    prev = interp->input;
    interp->input = fd;

    lisp_eval2(interp);
    interp->input = prev;
    (void)fclose(fd);
}


/** (catch (eval (read f)))
 */
void cerf(Interpreter *interp, FILE *fd)
{
    Primitive readPrimitive =  { "read",  0, 2, 0, primitiveRead };
    Primitive evalPrimitive =  { "eval",  1, 1, 0, primitiveEval };

    Object f =         (Object) { type_stream, .path = nil, .fd = fd };
    Object fCons =     (Object) { type_cons, .car = &f, .cdr = nil };
    Object read =      (Object) { type_primitive, .primitive = &readPrimitive, .type_check = nil };
    Object readCons =  (Object) { type_cons, .car = &read, .cdr = &fCons };
    if (fd == NULL)
        readCons.cdr = nil;
    Object readApply =  (Object) { type_cons, .car = &readCons, .cdr = nil };
    
    Object eval =      (Object) { type_primitive, .primitive = &evalPrimitive, .type_check = nil };
    Object evalCons =  (Object) { type_cons, .car = &eval, .cdr = &readApply };
    Object *evalApply = &(Object) { type_cons, .car = &evalCons, .cdr = nil };
    
    (void) evalCatch(interp, &evalApply, &interp->global);
}

Object *openInputStreamError(void)
{
    Object *m = &(Object) { type_string, .string = "alloc stream failed" };
    Object *o = &(Object) { type_cons, .car = nil, .cdr = nil };
    o =         &(Object) { type_cons, .car = m, .cdr = o };
    return      &(Object) { type_cons, .car = io_error, .cdr = o };
}

void lisp_eval3(Interpreter *interp, char *input)
{
    FILE *fd = NULL;
    Object *result;

    if (input != NULL) {
        fl_debug(interp, "lisp_eval3(\"%s\")", input);
        if (NULL == (fd = fmemopen(input, strlen(input), "r")))  {
            interp->object = openInputStreamError();
            return;
        }
    }
    interp->gcTop = nil;
    interp->object = result = ok;
    for (;;) {
        cerf(interp, fd);
        if (interp->object->car == end_of_file) {
            interp->object = result;
            break;
        }
        if (interp->object->car != nil)
            break;
        result = interp->object;
        lisp_write_object(interp, interp->output, result->cdr->cdr->car, true);
        writeChar(interp, interp->output, '\n');
    }
    if (interp->output) fflush(interp->output);
    if (fd) fclose(fd);
}


/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
