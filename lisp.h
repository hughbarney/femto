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
#define FL_VERSION  "0.8"

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

/* Constants */
/* Fundamentals */
extern Object *nil;
extern Object *t;
/* Types */
extern Object *type_integer;
extern Object *type_number;
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


/** Type codes:
 *
 * The public type of an object is a Lisp object type.  Internally an
 * enum is used in three places:
 * - Object creation
 * - Garbage collection
 * - Printing
 *
 * The following list must be kept in sync with flisp_object_type in lisp.c
 */
typedef enum ObjectType {
    TYPE_MOVED,
    TYPE_INTEGER,
    TYPE_NUMBER,
    TYPE_STRING,
    TYPE_SYMBOL,
    TYPE_CONS,
    TYPE_LAMBDA,
    TYPE_MACRO,
    TYPE_PRIMITIVE,
    TYPE_ENV,
    TYPE_STREAM
} ObjectType;

struct Object {
    Object *type;
    ObjectType type_code;
    size_t size;
    union {
        struct { int64_t integer; };                               // integer
        struct { double number; };                                 // number
        struct { char string[sizeof (Object *[3])]; };             // string, symbol
        struct { Object *car, *cdr; };                             // cons
        struct { Object *params, *body, *env; };                   // lambda, macro
        struct { int primitive; char *name; Object *type_check; }; // primitive
        struct { Object *parent, *vars, *vals; };                  // env
        struct { Object *path; FILE *fd; char *buf; size_t len; }; // file descriptor/stream
        struct { Object *forward; };                               // forwarding pointer
    };
};

typedef struct Constant {
    Object **symbol;
    Object **value;
} Constant;

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

#define TWO_STRING_ARGS(func)                                   \
    Object *first = (*args)->car;                               \
    Object *second = (*args)->cdr->car;                         \
    if (first->type != type_string)                             \
        exceptionWithObject(interp, first, wrong_type_argument, "(" CPP_XSTR(func) "  first second) - first is not a string"); \
    if (second->type != type_string)                            \
        exceptionWithObject(interp, second, wrong_type_argument, "(" CPP_XSTR(func) " first second) - second is not a string");

#define ONE_STRING_ARG(func)                                  \
    Object *arg = (*args)->car;                               \
    if (arg->type != type_string)                             \
        exceptionWithObject(interp, arg, wrong_type_argument, "(" CPP_XSTR(func) " arg) - arg is not a string");

#define ONE_NUMBER_ARG(func)                                  \
    Object *num = (*args)->car;                               \
    if (num->type != type_number)                             \
        exceptionWithObject(interp, num, wrong_type_argument, "(" CPP_XSTR(func) " num) - num is not a number");

#define ONE_STREAM_ARG(func)                                  \
    Object *stream = (*args)->car;                            \
    if (stream->type != type_stream)                          \
        exceptionWithObject(interp, stream, wrong_type_argument, "(" CPP_XSTR(func) " fd) - fd is not a stream");

#define FLISP_ARG_ONE (*args)->car
#define FLISP_ARG_TWO (*args)->cdr->car
#define FLISP_ARG_THREE (*args)->cdr->cdr->car

// PUBLIC INTERFACE ///////////////////////////////////////////////////////
extern Interpreter *lisp_new(size_t, char**, char*, FILE*, FILE*, FILE*);
extern void lisp_destroy(Interpreter *);
extern void lisp_eval(Interpreter *);
extern void lisp_eval_string(Interpreter *, char *);
extern void lisp_write_object(Interpreter *, FILE *, Object *, bool);
extern void lisp_write_error(Interpreter *, FILE *);

extern void lisp_eval2(Interpreter *);
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
