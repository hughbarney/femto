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

#define FL_NAME     "fLisp"
#define FL_VERSION  "0.3"

#define FL_INITFILE "flisp.rc"
#define FL_LIBDIR "/usr/local/share/flisp"

/* minimal Lisp object space size */
#define FLISP_MIN_MEMORY  26624UL  /* currently ~26k */

/* buffersize for Lisp eval input */
#define INPUT_FMT_BUFSIZ 2048
/* buffersize for Lisp result output */
#define WRITE_FMT_BUFSIZ 2048

/* Lisp objects */

typedef struct Object Object;

typedef enum ObjectType {
    TYPE_NUMBER,
    TYPE_STRING,
    TYPE_SYMBOL,
    TYPE_CONS,
    TYPE_LAMBDA,
    TYPE_MACRO,
    TYPE_PRIMITIVE,
    TYPE_ENV,
    TYPE_STREAM,
    TYPE_MOVED = -1
} ObjectType;

struct Object {
    ObjectType type;
    size_t size;
    union {
        struct { double number; };                      // number
        struct { char string[sizeof (Object *[3])]; };  // string, symbol
        struct { Object *car, *cdr; };                  // cons
        struct { Object *params, *body, *env; };        // lambda, macro
        struct { int primitive; char *name; };          // primitive
        struct { Object *parent, *vars, *vals; };       // env
        struct { Object *path; FILE *fd; char *buf; size_t len; }; // file descriptor/stream
        struct { Object *forward; };                    // forwarding pointer
    };
};

extern Object *nil;
extern Object *t;


typedef enum ResultCode {
    FLISP_OK,
    FLISP_ERROR,
    FLISP_RETURN,         /* successful return */
    FLISP_EOF,            /* eof upon read */
    FLISP_USER,           /* user generated exception */
    /* Parser/reader */
    FLISP_READ_INCOMPLETE,
    FLISP_READ_INVALID,
    FLISP_READ_RANGE,     /* number range over/underflow */
    /* Parameter */
    FLISP_WRONG_TYPE,
    FLISP_INVALID_VALUE,
    FLISP_PARAMETER_ERROR,
    /* System */
    FLISP_IO_ERROR,
    FLISP_OOM,
    /* Internal */
    FLISP_GC_ERROR,
} ResultCode;

// Note: WIP, relevant procedures must get a handle to the
//   Interpreter, instead of accessing the static allocated flisp.

typedef struct Memory {
    size_t capacity, fromOffset, toOffset;
    void *fromSpace, *toSpace;
} Memory;

typedef struct Interpreter Interpreter;
typedef struct Interpreter {
    Object *object;                  /* result or error object */
    char message[WRITE_FMT_BUFSIZ];  /* error string */
    ResultCode result;               /* result of last evaluation */
    /* private */
    Object *input;                  /* default input stream object */
    Object *output;                 /* default output stream object */
    FILE *debug;                    /* debug stream */
    
    Object *theRoot;      /* root object */
    Object **theEnv;      /* environment object */
    Object *symbols;      /* symbols list */
    Object root;          /* reified root node */
    Memory *memory;       /* memory available for object allocation,
                             cleaned up by garbage collector */
    jmp_buf exceptionEnv; /* exception handling */
    jmp_buf *catch;
    struct { char *buf; size_t len; size_t capacity; };  /* read buffer */
    Interpreter *next;    /* linked list of interpreters */
} Interpreter;

extern Interpreter *lisp_interpreters;

// PROGRAMMING INTERFACE ////////////////////////////////////////////////

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
    Object *stream = (*args)->car;                            \
    if (stream->type != TYPE_STREAM)                          \
        exceptionWithObject(interp, stream, FLISP_WRONG_TYPE, "(" CPP_XSTR(func) " fd) - fd is not a stream");

// PUBLIC INTERFACE ///////////////////////////////////////////////////////
extern Interpreter *lisp_new(size_t, char**, char*, FILE*, FILE*, FILE*);
extern void lisp_destroy(Interpreter *);
extern ResultCode lisp_eval(Interpreter *, Object *, Object *);
extern ResultCode lisp_eval_string(Interpreter *, char *, Object *);

extern Object *lisp_stream(Interpreter *, FILE *, char *);
extern int file_fclose(Interpreter *, Object *);
extern int file_fflush(Interpreter *, Object *);

extern void writeObject(Interpreter *, Object *, Object *, bool, Object *);


#ifdef FLISP_FILE_EXTENSION
#define FLISP_REGISTER_FILE_EXTENSION \
    {"fopen", 2, 2, primitiveFopen}, \
    {"fclose", 1, 1, primitiveFclose}, \
    {"fflush", 1, 1, primitiveFflush}, \
    {"ftell", 1, 1, primitiveFtell}, \
    {"fgetc", 1, 1, primitiveFgetc},
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
