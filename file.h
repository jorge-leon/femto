#ifndef FILE_H
#define FILE_H

#include "lisp.h"

extern Primitive flisp_file_primitives[];

extern Object *primitiveFflush(Interpreter *, Object** , Object **);
extern Object *primitiveFtell(Interpreter *, Object** , Object **);
extern Object *primitiveFgetc(Interpreter *, Object** , Object **);

#endif
/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
