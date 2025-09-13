#include <errno.h>
#include <stdlib.h>
#include <math.h>

#include "lisp.h"
#include "double.h"

/* Constants */
/* Types */
Object *type_double =            &(Object) { NULL, .string  = "type-double" };
/* Internal */
Object *double_one =            &(Object) { NULL, .number  = 1.0 };

Primitive flisp_double_primitives[] = {
    {"integer", 1,  1, TYPE_DOUBLE,  integerFromDouble},
    {"double",  1,  1, TYPE_INTEGER, doubleFromInteger},
    {"d+",      2,  2, TYPE_DOUBLE, doubleAdd},
    {"d-",      2,  2, TYPE_DOUBLE, doubleSubtract},
    {"d*",      2,  2, TYPE_DOUBLE, doubleMultiply},
    {"d/",      2,  2, TYPE_DOUBLE, doubleDivide},
    {"d%",      2,  2, TYPE_DOUBLE, doubleMod},
    {"d=",      2,  2, TYPE_DOUBLE, doubleEqual},
    {"d<",      2,  2, TYPE_DOUBLE, doubleLess},
    {"d<=",     2,  2, TYPE_DOUBLE, doubleLessEqual},
    {"d>",      2,  2, TYPE_DOUBLE, doubleGreater},
    {"d>=",     2,  2, TYPE_DOUBLE, doubleGreaterEqual},
    {NULL,      0,  0, 0,           NULL}
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
#define FLISP_DOUBLE_MATHOP(name, op)                                         \
Object *name(Interpreter *interp, Object **args, Object **env)                \
{                                                                             \
    return newDouble(interp, FLISP_ARG_ONE->number op FLISP_ARG_TWO->number); \
}
FLISP_DOUBLE_MATHOP(doubleAdd, +)
FLISP_DOUBLE_MATHOP(doubleSubtract, -)
FLISP_DOUBLE_MATHOP(doubleMultiply, *)
FLISP_DOUBLE_MATHOP(doubleDivide, /)
FLISP_DOUBLE_MATHOP(doubleEqual, ==)
FLISP_DOUBLE_MATHOP(doubleLess, <)
FLISP_DOUBLE_MATHOP(doubleLessEqual, <=)
FLISP_DOUBLE_MATHOP(doubleGreater, >)
FLISP_DOUBLE_MATHOP(doubleGreaterEqual, >=)

Object *doubleMod(Interpreter *interp, Object **args, Object **env)
{
    return newDouble(interp, fmod(FLISP_ARG_ONE->number, FLISP_ARG_TWO->number));
}

/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
