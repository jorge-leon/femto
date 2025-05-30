/*
 * flisp.c, Georg Lehner, Public Domain, 2024
 */

#include <stdlib.h>
#include <stdarg.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include "lisp.h"

// Specify in kByte.
#define FLISP_MEMORY_SIZE   300
// less then this is too small for femto.lsp


#define CPP_XSTR(s) CPP_STR(s)
#define CPP_STR(s) #s

ResultCode result;

// Note: wouldn't need, if we could implement the repl in fLisp
#define INPUT_BUFSIZE 4095
char input[INPUT_BUFSIZE+1]; // Note: termios paste limit or so

void fatal(char *msg)
{
    fprintf(stderr, "\n%s %s:\n%s\n", FL_NAME, FL_VERSION, msg);
    exit(1);
}

// Note: we'd like to implement the repl() in fLisp itself, for this we'd need:
// - isatty()
// - exception handling in fLisp
// - file output for error messages
int repl(Interpreter *interp)
{
    size_t i;
    ResultCode result;

    puts(FL_NAME " " FL_VERSION);
    puts("exit with Ctrl+D");
    while (true) {
        printf("> ");
        fflush(stdout);

        if (!fgets(input, sizeof(input), stdin)) break;
        i=strlen(input);
        if (input[i-1] == '\n')
            input[i-1] = '\0';
        else {
            fprintf(stderr, "error: more then " CPP_STR(INPUT_BUFSIZ) "read, skipping...\n");
            fflush(stderr);
            continue;
        }

        if (lisp_eval_string(interp, input))
            lisp_write_error(interp, stderr);
    }
    result = interp->result;
    // Note: close output, error?
    lisp_destroy(interp);
    return result;
}

int main(int argc, char **argv)
{
    char *library_path, *init_file, *debug_file;
    FILE *fd = NULL;
    Interpreter *interp;

    if ((init_file = getenv("FLISPRC")) == NULL)
        init_file = FL_LIBDIR "/" FL_INITFILE;

    if ((library_path=getenv("FLISPLIB")) == NULL)
        library_path = FL_LIBDIR;

    debug_file=getenv("FLISP_DEBUG");
    if (debug_file != NULL) {
        if (!(fd = fopen(debug_file, "w")))
            fprintf(stderr, "failed to open debug file %s for writing: %d\n", debug_file, errno);
    }

    interp = lisp_new(FLISP_MEMORY_SIZE*1024, argv, library_path, stdin, stdout, fd);
    if (interp == NULL)
        fatal("fLisp interpreter initialization failed");

    if (strlen(init_file)) {
        if (!(fd = fopen(init_file, "r")))
            fprintf(stderr, "failed to open inifile %s: %d\n", init_file, errno);
        else {
            // load inifile
            interp->input = fd;
            if (lisp_eval(interp))
                    fprintf(stderr, "failed to load inifile %s:%d: %s\n", init_file, interp->result, interp->message);
            // Note: if we could implement the repl in fLisp itself we'd bail out here.
            if (fclose(fd))
                fprintf(stderr, "failed to close inifile %s:%d %s\n", init_file, interp->result, interp->message);
        }
    }
    // Start repl
    //Note: could be omitted if we could implement the repl in fLisp itself.
    if (isatty(0))
        return repl(interp);

    // Just eval the standard input
    interp->input = stdin;
    if (lisp_eval(interp))
        lisp_write_error(interp, stderr);

    result = interp->result;
    //lisp_destroy(interp);
    return result;
}

/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
