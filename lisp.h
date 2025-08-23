#ifndef LISP_H
#define LISP_H
/*
 * lisp.h, femto, Georg Lehner, 2024
 * fLisp header file
 *
 */
#include <setjmp.h>
#include <stdio.h>
#include <stdbool.h>
#include <inttypes.h>

#define FL_NAME     "fLisp"
#define FL_VERSION  "0.10"

#define FL_INITFILE "flisp.rc"
#define FL_LIBDIR "/usr/local/share/flisp"

/* minimal Lisp object space size */
#define FLISP_MIN_MEMORY  26624UL  /* currently ~26k */

/* buffersize for Lisp eval input */
#define INPUT_FMT_BUFSIZ 2048
/* buffersize for Lisp result output */
#define WRITE_FMT_BUFSIZ 2048

/* Debugging */
#define DEBUG_GC 0
#define DEBUG_GC_ALWAYS 0

/* Lisp objects */

typedef struct Object Object;
typedef struct Interpreter Interpreter;
typedef Object *(*LispEval) (Interpreter *, Object **, Object **);
/* Constants */
/* Fundamentals */
extern Object *nil;
extern Object *t;
/* Types */
extern Object *type_integer;
extern Object *type_double;
extern Object *type_string;
extern Object *type_symbol;
extern Object *type_cons;
extern Object *type_lambda;
extern Object *type_macro;
extern Object *type_primitive;
extern Object *type_stream;
/* internal */
extern Object *type_env;
extern Object *type_moved;
/* Exceptions */
extern Object *end_of_file;
extern Object *range_error;
extern Object *wrong_type_argument;
extern Object *invalid_value;
extern Object *wrong_num_of_arguments;
extern Object *io_error;
extern Object *out_of_memory;

typedef enum ObjectType {
    TYPE_MOVED,
    TYPE_INTEGER,
#ifdef FLISP_DOUBLE_EXTENSION
    TYPE_DOUBLE,
#endif
    TYPE_STRING,
    TYPE_SYMBOL,
    TYPE_CONS,
    TYPE_LAMBDA,
    TYPE_MACRO,
    TYPE_PRIMITIVE,
    TYPE_ENV,
    TYPE_STREAM,
} ObjectType;

typedef struct Primitive {
    char *name;
    int nMinArgs, nMaxArgs;
    ObjectType type_check;
    LispEval eval;
} Primitive;


struct Object {
    Object *type;
    size_t size;
    union {
        struct { int64_t integer; };                               // integer
        struct { double number; };                                 // double
        struct { char string[sizeof (Object *[3])]; };             // string, symbol
        struct { Object *car, *cdr; };                             // cons
        struct { Object *params, *body, *env; };                   // lambda, macro
        struct { Primitive *primitive; Object *type_check; };      // primitive
        struct { Object *parent, *vars, *vals; };                  // env
        struct { Object *path; FILE *fd; char *buf; size_t len; }; // file descriptor/stream
        struct { Object *forward; };                               // forwarding pointer
    };
};

typedef struct Constant {
    Object **symbol;
    Object **value;
} Constant;

/* Internal */
typedef struct Memory {
    size_t capacity, fromOffset, toOffset;
    void *fromSpace, *toSpace;
} Memory;

typedef struct Interpreter {
    Object *object;                  /* result or error object */
    Object *result;                  /* result symbol */
    char msg_buf[WRITE_FMT_BUFSIZ];  /* error string */

    /* private */
    FILE *input;                     /* default input stream object */
    FILE *output;                    /* default output file descriptor */
    FILE *debug;                     /* debug stream */

    /* globals */
    Object *symbols;                 /* symbols list */
    Object *global;                   /* global environment */
    /* GC */
    Object *gcTop;                   /* dynamic gc trace stack */
    Memory *memory;                  /* memory available for object
                                      * allocation, cleaned up by
                                      * garbage collector */
    /* exeptions */
    jmp_buf exceptionEnv;  /* exception handling */
    jmp_buf *catch;
    /* reader */
    struct { char *buf; size_t len; size_t capacity; };  /* read buffer */
    /* interpreters */
    struct Interpreter *next;    /* linked list of interpreters */
} Interpreter;

/*@null@*/extern Interpreter *lisp_interpreters;

// PROGRAMMING INTERFACE ////////////////////////////////////////////////

extern Object * newObject(Interpreter *, Object *);
extern Object *newObjectFrom(Interpreter *, Object **);
extern Object *newInteger(Interpreter *, int64_t);
extern size_t addCharToBuf(Interpreter *, int);
extern void resetBuf(Interpreter *);
extern void exceptionWithObject(Interpreter *, Object *, Object *, char *, ...);
#define exception(interp, result, ...)       exceptionWithObject(interp, nil, result, __VA_ARGS__)
#define GC_PASTE1(name, id)  name ## id
#define GC_PASTE2(name, id)  GC_PASTE1(name, id)
#define GC_UNIQUE(name)      GC_PASTE2(name, __LINE__)

#define GC_CHECKPOINT Object *gcTop = interp->gcTop
#define GC_RELEASE interp->gcTop = gcTop
extern Object *gcReturn(Interpreter *, Object *, Object *);
#define GC_RETURN(expr)  return gcReturn(interp, gcTop, expr)

#define GC_TRACE(name, init)                                            \
    Object GC_UNIQUE(gcTrace) = { type_cons, .car = init, .cdr = interp->gcTop }; \
    interp->gcTop = &GC_UNIQUE(gcTrace);                                \
    Object **name = &GC_UNIQUE(gcTrace).car;

void fl_debug(Interpreter *, char *, ...);


#define FLISP_ARG_ONE (*args)->car
#define FLISP_ARG_TWO (*args)->cdr->car
#define FLISP_ARG_THREE (*args)->cdr->cdr->car

#define FLISP_HAS_ARG_TWO ((*args)->cdr != nil)
#define FLISP_HAS_ARG_THREE ((*args)->cdr->cdr != nil)

#define CHECK_TYPE(PARAM, TYPE, SIGNATURE) \
    if (PARAM->type != TYPE)               \
        exceptionWithObject(interp, PARAM, wrong_type_argument, \
                            SIGNATURE " expected %s, got: %s", TYPE->string, PARAM->type->string)

// PUBLIC INTERFACE ///////////////////////////////////////////////////////
extern Interpreter *lisp_new(size_t, char**, char*, FILE*, FILE*, FILE*);
extern void lisp_destroy(Interpreter *);
extern void lisp_eval(Interpreter *);
extern void lisp_eval_string(Interpreter *, char *);
extern void lisp_write_object(Interpreter *, FILE *, Object *, bool);
extern void lisp_write_error(Interpreter *, FILE *);

extern void lisp_write_error2(Interpreter *, FILE *);
extern void lisp_eval2(Interpreter *);
extern void lisp_eval3(Interpreter *, char *);
extern void lisp_eval_string2(Interpreter *, char *);

#ifdef FLISP_FILE_EXTENSION
#define FLISP_REGISTER_FILE_EXTENSION \
    {"fflush", 1, 1, TYPE_STREAM, primitiveFflush},     \
    {"ftell",  1, 1, TYPE_STREAM, primitiveFtell},   \
    {"fgetc",  1, 1, TYPE_STREAM, primitiveFgetc},
#else
#define FLISP_REGISTER_FILE_EXTENSION
#endif


#endif
/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
