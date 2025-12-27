/*
 * flisp.c, Georg Lehner, Public Domain, 2024
 */

#include <stdlib.h>
#include "lisp.h"

#ifdef FLISP_DOUBLE_EXTENSION
#include "double.h"
#endif
#include "file.h"


void fatal(char *msg)
{
    fputs("\n" FL_NAME " " FL_VERSION ": ", stderr);
    fputs(msg, stderr);
    fputc('\n', stderr);
    exit(1);
}

int main(int argc, char **argv)
{
    char *rcfile, *debug_file;
    FILE *debug_fd = NULL, *input_fd = stdin;
    Interpreter *interp;

    if ((rcfile = getenv("FLISPRC")) == NULL)
        rcfile = CPP_XSTR(FLISPRC);

    if (*rcfile != '\0')
        if (!(input_fd = fopen(rcfile, "r")))
            fatal("failed to open rcfile, FLISPRC or: " CPP_XSTR(FLISPRC));

    if ((debug_file=getenv("FLISP_DEBUG")) != NULL)
        if ((debug_fd = fopen(debug_file, "w")) == NULL)
            fatal("failed to open debug file");

    interp = lisp_new(0, argv, NULL, input_fd, stdout, debug_fd);
    if (interp == NULL)
        fatal("fLisp interpreter initialization failed");

#ifdef FLISP_DOUBLE_EXTENSION
    lisp_double_register(interp);
#endif
    lisp_file_register(interp);

    lisp_eval(interp, NULL);
    if (FLISP_RESULT_CODE(interp) != nil) {
        lisp_write_error(interp, stderr);
        return 1;
    }
    return 0;
}

/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
