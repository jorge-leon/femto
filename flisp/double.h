#ifndef DOUBLE_H
#define DOUBLE_H

#include "lisp.h"

extern Object *newDouble(Interpreter *, double);
extern Object *readDouble(Interpreter *);

extern Object *integerFromDouble(Interpreter *, Object **, Object **);
extern Object *doubleFromInteger(Interpreter *, Object **, Object **);
extern Object *doubleAdd(Interpreter *, Object **, Object **);
extern Object *doubleSubtract(Interpreter *, Object **, Object **);
extern Object *doubleMultiply(Interpreter *, Object **, Object **);
extern Object *doubleDivide(Interpreter *, Object **, Object **);
extern Object *doubleMod(Interpreter *, Object **, Object **);
extern Object *doubleEqual(Interpreter *, Object **, Object **);
extern Object *doubleLess(Interpreter *, Object **, Object **);
extern Object *doubleLessEqual(Interpreter *, Object **, Object **);
extern Object *doubleGreater(Interpreter *, Object **, Object **);
extern Object *doubleGreaterEqual(Interpreter *, Object **, Object **);

extern void lisp_double_register(Interpreter *);

#endif
/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
