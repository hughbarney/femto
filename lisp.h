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
#define FL_VERSION  "0.2"

#define FL_INITFILE "flisp.rc"
#define FL_LIBDIR "/usr/local/share/flisp"

//#define FLISP_MEMORY_SIZE          131072UL
//#define FLISP_MEMORY_SIZE          262144UL  /* 256k */
//#define FLISP_MEMORY_SIZE          524288UL
#define FLISP_MEMORY_SIZE         4194304UL  /* 4M */

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
    FLISP_RETURN,         /* successfull return */
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
//   init_lisp() must allocate the memory by itself and return an
//   Interpreter to be used by call_lisp().

typedef struct Memory {
    size_t capacity, fromOffset, toOffset;
    void *fromSpace, *toSpace;
} Memory;

typedef struct Interpreter Interpreter;
typedef struct Interpreter {
    Object *input;                   /* input stream */
    Object *output;                  /* output stream */
    Object *object;                  /* result or error object */
    char message[WRITE_FMT_BUFSIZ];  /* error string */
    ResultCode result;               /* result of last evaluation */
    Object *debug;                   /* debug stream */
    /* private */
    Object *theRoot;      /* root object */
    Object **theEnv;      /* environment object */
    Object *symbols;      /* symbols list */
    Object root;          /* reified root node */
    Memory *memory;       /* memory available for object allocation,
                             cleaned up by garbage collector */
    jmp_buf *stackframe;  /* exception handling */
    struct { char *buf; size_t len; size_t capacity; };  /* read buffer */
    Interpreter *next;    /* linked list of interpreters */
} Interpreter;

extern Interpreter *lisp_interpreters;

extern Interpreter *lisp_init(int, char**, char*);
extern ResultCode lisp_eval(Interpreter *);
extern ResultCode lisp_eval_string(Interpreter *, char *);

Object *file_fopen(Interpreter *, char *, char*);
void lisp_destroy(Interpreter *);
int file_fclose(Interpreter *, Object *);
int file_fflush(Interpreter *, Object *);

void writeChar(Object *, char);
void writeString(Object *, char *);
void writeObject(Object *, Object *, bool);


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
