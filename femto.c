/*
 * femto.c, femto, Hugh Barney, Public Domain, 2017
 * Derived from: Anthony's Editor January 93, (Public Domain 1991, 1993 by Anthony Howe)
 */

#include <stdlib.h>
#include <stdarg.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <locale.h>

#include <curses.h>

#include "femto.h"
#include "window.h"
#include "buffer.h"
#include "key.h"
#include "display.h"
#include "hilite.h"
#include "command.h"

#include "flisp/lisp.h"
#include "flisp/string.h"
#include "flisp/posix.h"
#include "flisp/double.h"

void gui(void); /* The GUI loop used in interactive mode */

Object *interp, *err_stream;
char debug_file[] = "debug.out";
FILE *debug_fp = NULL;
int flisp_input_pipe[2];
char *flisp_error_output;
size_t flisp_error_size;

/** lisp_init() - initialize fLisp interpreter and load rc file
 *
 * @param: argv .. Array of commandline arguments.
 *
 * Determines the path to the rc file and opens it, optionally opens
 * the debug file and instantiates an fLisp interpreter with this files.
 *
 * Output and Errors are logged to the debug file descriptor.
 *
 */
void lisp_init(char **argv)
{
    FILE *init_fd = NULL;
    char *init_file;
    Object *e = nil;

    if ((init_file = getenv("FEMTORC")) == NULL)
        init_file = CPP_XSTR(E_INITFILE);

    if ((init_fd = fopen(init_file, "r")) == NULL) {
        debug("failed to open rc file %s: %s\n", init_file, strerror(errno));
        if (!batch_mode)
            fatal("No init file, exiting..");
    }

    do {
        FLISP_UNLESS_ERR(interp = flisp_interpreter(FLISP_INITIAL_MEMORY, argv, NULL, init_fd, debug_fp, debug_fp));
        FLISP_UNLESS_ERR(flisp_register_extension(interp, "string", flisp_string_init));
        FLISP_UNLESS_ERR(flisp_register_extension(interp, "posix", flisp_posix_init));
        FLISP_UNLESS_ERR(flisp_register_extension(interp, "femto", femto_init));
        debug("femto primitives and constants registered\n");
        FLISP_UNLESS_ERR(femto_init(interp, interp->self.extensions->car));
        debug("femto extension loaded\n");
        FLISP_UNLESS_ERR(flisp_register_extension(interp, "double", flisp_double_init));
        debug("double extension registered\n");
    } while(0);
    if (FLISP_IS_ERR(e))
        fatal("fLisp initialization failed");
        
    if (!init_fd)
        return;
    debug("evaluating rc file %s\n", init_file);
    FLISP_STANDARD_INPUT.fd=init_fd;
    e = flisp_eval_input(interp, nil);
    if (FLISP_IS_ERR(e)) {
        flisp_write_object(interp, e, t, interp->self.stderr);
        debug("failed to load rc file %s:\n", init_file);
        if (FLISP_IS_OOM(e))
            fatal("exiting..");
    }

    if ((FLISP_STDERR.fd = open_memstream(&flisp_error_output, &flisp_error_size)) == NULL)
        fatal("Failed to create fLisp error stream");
    /* fLisp's input is a pipe, Femto writes commands to it */
    if (pipe(flisp_input_pipe) == -1)
        fatal("Failed to create fLisp input pipe");
    if ((FLISP_STANDARD_INPUT.fd = fdopen(flisp_input_pipe[0], "r")) == NULL)
        fatal("Failed to open fLisp input pipe read stream");
    debug("fLisp input pipe set up\n");
}

int main(int argc, char **argv)
{
    char *envv;
    batch_mode = ((envv=getenv("FEMTO_BATCH")) != NULL && strcmp(envv, "0"));
    debug_mode = ((envv=getenv("FEMTO_DEBUG")) != NULL && strcmp(envv, "0"));

    if (debug_mode)
        if ((debug_fp = fopen(debug_file, "w")) == NULL)
            fatal("could not open debug file");

    /* buffers */
    setlocale(LC_ALL, "") ; /* required for 3,4 byte UTF8 chars */
    curbp = new_buffer(str_scratch);
    if (curbp == NULL)
        fatal("failed to allocate memory for sratch buffer");
    /* windows */
    curwp = wheadp = new_window();
    if (curwp == NULL)
        fatal("failed to allocate memory for first window");

    associate_b2w(curbp, curwp);

    /* keymaps */
    setup_keys();

    lisp_init(argv);

    debug("start\n");

    if (batch_mode) {
        FLISP_STANDARD_INPUT.fd = stdin;
        FLISP_STANDARD_OUTPUT.fd = stdout;
        FLISP_STDERR.fd = stderr;
        /* Note: we might need to set interp->self.print here */
        while (!FLISP_IS_EOF(flisp_eval_input(interp, nil)));
        return 0;
    }

    err_stream = file_fopen(interp, "", ">");
    if (FLISP_IS_ERR(err_stream))
        fatal("failed to open error formatting stream");
    
    /* GUI */
    gui();

    debug("main(): shutdown\n");
    if (file_fclose(interp, err_stream))
        fatal("failed to close error formatting stream");

    // Note: exit frees all memory, do we need this here?
    // Note: we can't do, or we can again
    //flisp_destroy(interp);
    //here, because we get segfaults in wide character routines.

    // Note: the following lines sometimes free not-allocated memory
    // if (scrap != NULL) free(scrap);
    return 0;
}

/** Handle errors from Lisp scripts
 *
 * @param interp
 */
void msg_lisp_err(Object *interp, Object *e)
{
    rewind(err_stream->stream.fd);
    flisp_write_object(interp, e, nil, err_stream);
    msg("%s", err_stream->stream.buf);
}
void drain_flisp_input_pipe() {
    char buf[PIPE_BUF];
    int size = read(flisp_input_pipe[0], buf, PIPE_BUF);
    if (size == -1)
        debug("eval_string: buffer empty, %\n", strerror(errno));
    else 
        debug("eval_string: emptying %d bytes: %s\n", buf);
}

/** eval_string - Invoke fLisp interpreter and return result as string
 *
 * @param ...        parameters to the format string.
 *
 */
void eval_string(char *format, ...)
{
    Object *e;
    va_list args;
    size_t size;

    va_start(args, format);
    size = vdprintf(flisp_input_pipe[1], format, args);
    va_end(args);
    if (size > PIPE_BUF) {
        msg("input string larger then %d", PIPE_BUF);
        drain_flisp_input_pipe();
        return;
    }
    e = flisp_eval_expr(interp, t);
    if (!FLISP_IS_ERR(e))
        return;
    if (FLISP_IS_OOM(e))
        fatal("OOM wile evaluating expression");    
    if (FLISP_IS_EOF(e))
        fatal("fLisp input pipe closed");
    msg_lisp_err(interp, e);
}

void gui(void)
{
    debug("gui(): init\n");
    if (initscr() == NULL) fatal(f_initscr);
    raw();
    noecho();
    idlok(stdscr, TRUE);

    hilite_init();

    /* windows */
    one_window(curwp);

    debug("gui(): loop\n");
    while (!done) {
        update_display();
        input = get_key(khead, &key_return);

        if (key_return != NULL)
            (key_return->k_func)();
        else {
            /*
             * if first char of input is a control char then
             * key is not bound, except TAB and NEWLINE
             */
            if (*input > 31 || *input == 0x0A || *input == 0x09)
                insert();
            else {
                flushinp(); /* discard without writing in buffer */
                msg(str_not_bound);
            }
        }

        /* debug_stats("main loop:"); */
        match_parens();
    }
    debug("gui(): shutdown\n");
    move(LINES-1, 0);
    refresh();
    noraw();
    endwin();
}

void fatal(char *msg)
{
    if (!batch_mode) {
        if (curscr != NULL) {
            noraw();
            endwin();
        }
    }
    printf("\n%s %s:\n%s\n", E_NAME, E_VERSION, msg);
    exit(1);
}

void msg(char *m, ...)
{
    va_list args;
    va_start(args, m);
    (void) vsprintf(msgline, m, args);
    va_end(args);
    msgflag = TRUE;

    if (batch_mode) {
        puts(msgline);
        fflush(stdout);
    }
}

void debug(char *format, ...)
{
    char buffer[256];
    va_list args;

    if (debug_fp == NULL) return;

    va_start (args, format);

    vsnprintf (buffer, sizeof(buffer), format, args);
    va_end(args);
    fprintf(debug_fp,"%s", buffer);
    fflush(debug_fp);
}

void debug_stats(char *s)
{
    debug("%s bsz=%d p=%d m=%d gap=%d egap=%d\n", s, curbp->b_ebuf - curbp->b_buf, curbp->b_point, curbp->b_mark, curbp->b_gap - curbp->b_buf, curbp->b_egap - curbp->b_buf);
}

/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
