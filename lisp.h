#ifndef LISP_H
#define LISP_H
/*
 * lisp.h, femto, Georg Lehner, 2024
 * fLisp header file
 *
 */
#include <setjmp.h>
#include <stdio.h>

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
    RESULT_OK,
    RESULT_ERROR
} ResultCode;

// Note: WIP, relevant procedures must get a handle to the
//   Interpreter, instead of accessing the static allocated flisp.
//   init_lisp() must allocate the memory by itself and return an
//   Interpreter to be used by call_lisp().

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

typedef struct Memory {
    size_t capacity, fromOffset, toOffset;
    void *fromSpace, *toSpace;
} Memory;

typedef struct Interpreter Interpreter;
typedef struct Interpreter {
    char * output;                   /* output of last evaluation, NULL if writing to STDOUT */
    ResultCode result;               /* result of last evaluation */
    char message[WRITE_FMT_BUFSIZ];  /* last error message */
    /* private */
    Object *theRoot;      /* root object */
    Object **theEnv;      /* environment object */
    Object *symbols;      /* symbols list */
    Object root;          /* reified root node */
    Stream *istream;      /* Lisp input stream */
    Stream ostream;       /* Lisp output stream */
    Memory *memory;       /* memory available for object allocation,
                             cleaned up by garbage collector */
    jmp_buf *stackframe;  /* exception handling */
    Interpreter *next;    /* linked list of interpreters */
} Interpreter;

extern Interpreter *lisp_interpreters;

extern Interpreter *lisp_init(int, char**, char*);
extern ResultCode lisp_eval(Interpreter*, char *, ...);

#endif
/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
