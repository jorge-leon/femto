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
#define FL_VERSION  "0.14"

#ifndef FLISPLIB
#define FLISPLIB /usr/local/share/flisp
#endif
#ifndef FLISPRC
#define FLISPRC  FLISPRC/init.lsp
#endif

/* For inserting FLISPLIP, FLISPRC */
#define CPP_XSTR(s) CPP_STR(s)
#define CPP_STR(s) #s

#ifndef FLISP_MEMORY_INC_SIZE
#define FLISP_MEMORY_INC_SIZE 8192UL  /* Increase memory by this amount if not enough */
#endif

/* buffersize for Lisp eval input */
#define INPUT_FMT_BUFSIZ 2048
/* buffersize for Lisp result output */
#define WRITE_FMT_BUFSIZ 2048

/* Debugging */
#define DEBUG_GC 0
#define DEBUG_GC_ALWAYS 0
#define FLISP_TRACE 0
#define FLISP_TRACK_GCTOP 0

/* Lisp objects */

typedef struct Object Object;
typedef struct Interpreter Interpreter;
typedef Object *(*LispEval) (Interpreter *, Object **, Object **);

/* Note: Only used, because we do not know how to statically
 *       initialize the required Lisp type objects
 */
typedef enum ObjectType {
    TYPE_MOVED,
    TYPE_INTEGER,
    TYPE_DOUBLE,
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

    /* private */
    Object *error;                   /* error code cons */
    struct { Object * type; size_t size; char string[WRITE_FMT_BUFSIZ]; } message;
    Object *result;                  /* result or error object */

    Object input;                    /* input stream object */
    FILE *output;                    /* default output file descriptor */
    FILE *debug;                     /* debug file descriptor */

    /* globals */
    Object *symbols;                 /* symbols list */
    Object *global;                  /* global environment */
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
/* I/O */
extern Object *permission_denied;
extern Object *not_found;
extern Object *file_exists;
extern Object *read_only;
extern Object *is_directory;
/* utility */
extern Object *lisp_empty_string;

/* "Object" type for initializing constants with long names.
 * Note: currently wrong-number-of-arguments is the longest, if you
 *       need a longer put it here.
 */
typedef struct { Object *type; size_t size; char string[sizeof("wrong-number-of-arguments")]; } Symbol;

extern Object *newObject(Interpreter *, Object *);
extern Object *newObjectFrom(Interpreter *, Object **);
extern Object *newInteger(Interpreter *, int64_t);
extern Object *newStringWithLength(Interpreter *, char *, size_t);
extern Object *newString(Interpreter *, char *);
extern Object *newCons(Interpreter *, Object **, Object **);
extern Object *newSymbol(Interpreter *, char *);
extern Object *newStreamObject(Interpreter *, FILE *, char *);

extern int streamGetc(Interpreter *interp, FILE *fd);
extern void resetBuf(Interpreter *);
extern size_t addCharToBuf(Interpreter *, int);

extern void setInterpreterResult(Interpreter *, Object *, Object *, char *, ...);
#define exceptionWithObject(interp, object, error, ...)           \
    do {                                                          \
        resetBuf(interp);                                         \
        setInterpreterResult(interp, object, error, __VA_ARGS__); \
        longjmp(*interp->catch, 2);                               \
    } while(0)
#define exception(interp, error, ...)       exceptionWithObject(interp, nil, error, __VA_ARGS__)

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

#define FLISP_HAS_ARGS *args != nil
#define FLISP_HAS_ARG_TWO ((*args)->cdr != nil)
#define FLISP_HAS_ARG_THREE ((*args)->cdr->cdr != nil)

#define CHECK_TYPE(PARAM, TYPE, SIGNATURE) \
    if (PARAM->type != TYPE)               \
        exceptionWithObject(interp, PARAM, wrong_type_argument, \
                            SIGNATURE " expected %s, got: %s", TYPE->string, PARAM->type->string)

// PUBLIC INTERFACE ///////////////////////////////////////////////////////
extern Interpreter *lisp_new(size_t size, char **, char*, FILE*, FILE*, FILE*);
extern void lisp_destroy(Interpreter *);
extern void lisp_eval(Interpreter *, char *);
extern void lisp_write_object(Interpreter *, FILE *, Object *, bool);
extern void lisp_write_error(Interpreter *, FILE *);

extern void lisp_register_constant(Interpreter *, Object *, Object *);
extern void lisp_register_primitive(Interpreter *, Primitive *);

#define FLISP_RESULT_CODE(INTERPRETER) INTERPRETER->error
#define FLISP_RESULT_MESSAGE(INTERPRETER) ((Object *)&INTERPRETER->message)
#define FLISP_RESULT_OBJECT(INTERPRETER) INTERPRETER->result

#endif
/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
