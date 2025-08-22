#ifndef DOUBLE_C
#define DOUBLE_C

#include <errno.h>
#include <stdlib.h>

#include "lisp.h"
#include "double.h"

/* Constants */
/* Types */
Object *type_double =            &(Object) { NULL, .string  = "type-double" };
/* Internal */
Object *double_one =            &(Object) { NULL, .number  = 1.0 };

Primitive flisp_double_primitives[] = {
    {"integer",       1,  1, TYPE_DOUBLE,  integerFromDouble},
    {"double",       1,  1, TYPE_INTEGER, doubleFromInteger},
    {"d+",            0, -1, TYPE_DOUBLE, doubleAdd},
    {"d-",            0, -1, TYPE_DOUBLE, doubleSubtract},
    {"d*",            0, -1, TYPE_DOUBLE, doubleMultiply},
    {"d/",            1, -1, TYPE_DOUBLE, doubleDivide},
    {"d%",            1, -1, TYPE_DOUBLE, doubleMod},
    {"d=",            1, -1, TYPE_DOUBLE, doubleEqual},
    {"d<",            1, -1, TYPE_DOUBLE, doubleLess},
    {"d<=",           1, -1, TYPE_DOUBLE, doubleLessEqual},
    {"d>",            1, -1, TYPE_DOUBLE, doubleGreater},
    {"d>=",           1, -1, TYPE_DOUBLE, doubleGreaterEqual},
    {NULL,            0,  0, 0,           NULL}
};

Object *newDouble(Interpreter *interp, double number)
{
    Object *object = newObject(interp, type_double);
    object->number = number;
    return object;
}

/** readDouble - add a float from the read buffer to the interpreter
 *
 * @param interp  fLisp interpreter
 *
 * returns: double object
 *
 * throws: range-error
 */
Object *readDouble(Interpreter *interp)
{
    double d;
    Object *number;

    addCharToBuf(interp, '\0');
    errno = 0;
    d = strtod(interp->buf, NULL);
    if (errno == ERANGE)
        exception(interp, range_error, "integer out of range,: %f", d);
    // Note: purposely not dealing with NaN
    number = newDouble(interp, d);
    resetBuf(interp);
    return number;
}

// Number Type Conversion /////

Object *integerFromDouble(Interpreter *interp, Object **args, Object **env)
{
    return newInteger(interp, (int64_t) FLISP_ARG_ONE->number);
}

Object *doubleFromInteger(Interpreter *interp, Object **args, Object **env)
{
    return newDouble(interp, (double) FLISP_ARG_ONE->integer);
}
// Double Math ///////

#define DEFINE_PRIMITIVE_ARITHMETIC(name, op, init)                     \
    Object *name(Interpreter *interp, Object **args, Object **env) {    \
        if (*args == nil)                                               \
            return newDouble(interp, init);                             \
        Object *object;                                                 \
        GC_CHECKPOINT;                                                  \
        GC_TRACE(gcRest, *args);                                        \
        if ((*gcRest)->cdr == nil) {                                    \
            object = newDouble(interp, init);                           \
        } else {                                                        \
            object = newObjectFrom(interp, &(*gcRest)->car);              \
            *gcRest = (*gcRest)->cdr;                                   \
        }                                                               \
        GC_RELEASE;                                                     \
        for (; *gcRest != nil; *gcRest = (*gcRest)->cdr)                \
            object->number = object->number op (*gcRest)->car->number;  \
        return object;                                                  \
    }

DEFINE_PRIMITIVE_ARITHMETIC(doubleAdd, +, 0)
DEFINE_PRIMITIVE_ARITHMETIC(doubleSubtract, -, 0)
DEFINE_PRIMITIVE_ARITHMETIC(doubleMultiply, *, 1)
DEFINE_PRIMITIVE_ARITHMETIC(doubleDivide, /, 1)

Object *doubleMod(Interpreter *interp, Object **args, Object **env) {
    if (*args == nil)
        return double_one;

    Object *object;
    GC_CHECKPOINT;
    GC_TRACE(gcRest, *args);
    if ((*gcRest)->cdr == nil) {
        object = double_one;
    } else {
        object = newObjectFrom(interp, &(*gcRest)->car);
        *gcRest = (*args)->cdr;
    }
    GC_RELEASE;
    for (; *gcRest != nil; *gcRest = (*gcRest)->cdr)
        object->number = (int)object->number % (int)(*gcRest)->car->number;

    return object;
}

#define DEFINE_PRIMITIVE_RELATIONAL(name, op)                           \
    Object *name(Interpreter *interp, Object **args, Object **env) {    \
        Object *rest = *args;                                           \
        bool result = true;                                             \
        for (; result && rest->cdr != nil; rest = rest->cdr)            \
            result &= rest->car->number op rest->cdr->car->number;      \
        return result ? t : nil;                                        \
    }

DEFINE_PRIMITIVE_RELATIONAL(doubleEqual, ==)
DEFINE_PRIMITIVE_RELATIONAL(doubleLess, <)
DEFINE_PRIMITIVE_RELATIONAL(doubleLessEqual, <=)
DEFINE_PRIMITIVE_RELATIONAL(doubleGreater, >)
DEFINE_PRIMITIVE_RELATIONAL(doubleGreaterEqual, >=)

#endif

/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
