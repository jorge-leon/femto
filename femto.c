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

Object *interp;
char debug_file[] = "debug.out";
FILE *prev, *flisp_input, *debug_fp = NULL;
int flisp_input_pipe[2];

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
    Object *e;

    if ((init_file = getenv("FEMTORC")) == NULL)
        init_file = CPP_XSTR(E_INITFILE);

    if ((init_fd = fopen(init_file, "r")) == NULL)
        debug("failed to open rc file %s: %s\n", init_file, strerror(errno));

    do {
        FLISP_UNLESS_ERR(interp = flisp_new(FLISP_INITIAL_MEMORY, argv, init_fd, debug_fp, debug_fp, debug_fp));
        FLISP_UNLESS_ERR(flisp_register_extension(interp, "string", flisp_string_init));
        FLISP_UNLESS_ERR(flisp_string_init(interp, FLISP_INTERP.extensions->car));
        
        FLISP_UNLESS_ERR(flisp_register_extension(interp, "posix", flisp_posix_init));
        FLISP_UNLESS_ERR(flisp_posix_init(interp, FLISP_INTERP.extensions->car));
        
        FLISP_UNLESS_ERR(flisp_register_extension(interp, "double", flisp_double_init));
        
        FLISP_UNLESS_ERR(flisp_register_extension(interp, "editor", femto_flisp_init));
        FLISP_UNLESS_ERR(femto_flisp_init(interp, FLISP_INTERP.extensions->car));
    } while(0);
    if (FLISP_IS_ERR(e))
        fatal("fLisp initialization failed");

    debug("femto primitives and constants registered\n");
    if (init_fd) {
        debug("evaluating rc file %s\n", init_file);
        e = flisp_eval_input(interp, false);
        if (FLISP_IS_OOM(e))
            fatal("OOM, exiting..");

        if (!FLISP_IS_EOF(e)) {
            /* Note: when we fail here and do not debug Femto doesn't work and the user has no clue about it */
            debug("failed to load rc file %s:\n", init_file);
            flisp_write_object(debug_fp, e, true);
            batch_mode = true;
            fprintf(stderr, "Failed to load rc file, see debug logs. Entering batch mode, Quit with C-d\n");
        }
        if (!fclose(init_fd))
            debug("failed to close rcfile %s: %s\n", init_file, strerror(errno));
    }

    if (pipe(flisp_input_pipe) == -1)
        fatal("Failed to create fLisp input pipe");
    if ((FLISP_STANDARD_INPUT.fd = fdopen(flisp_input_pipe[0], "r")) == NULL)
        fatal("Failed to open fLisp input pipe");
    if (fcntl(flisp_input_pipe[1], F_SETFD, O_NONBLOCK) == -1)
        fatal("Failed to configure fLisp input pipe");
    if ((flisp_input = fdopen(flisp_input_pipe[1], "a")) == NULL)
        fatal("Failed to configure fLisp input");
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
        Object * result = flisp_eval_input(interp, false);
        if (FLISP_IS_ERR(result)) {
            flisp_write_object(stderr, result, true);
            fputs("", stderr);
            return 1;
        }
    } else
        /* GUI */
        gui();

    debug("main(): shutdown\n");
    // Note: exit frees all memory, do we need this here?
    // Note: we can't do
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
void msg_lisp_err(Object *result)
{
    char *buf;
    size_t len;
    FILE *fd;

    if (NULL == (fd = open_memstream(&buf, &len)))
        fatal("failed to allocate error formatting buffer");
    flisp_write_object(fd, result, true);
    msg("%s", buf);
    fclose(fd);
    free(buf);
}

/** eval_string - Invoke fLisp interpreter and return result as string
 *
 * @param do_format  If true, the input string is passed through
 *                   printf style formatting, otherwise it is used directly.
 * @param format     Input string for the interpreter.
 *
 */
#if 0
void eval_string(bool do_format, char *format, ...)
{
    char buf[INPUT_FMT_BUFSIZ], *input;

    int size;
    va_list args;

    if (do_format) {
        va_start(args, format);
        size = vsnprintf (buf, sizeof(buf), format, args);
        va_end(args);
        if (size > INPUT_FMT_BUFSIZ) {
            msg("input string larger then %d", INPUT_FMT_BUFSIZ);
            return;
        }
        input = buf;
    } else {
        input = format;
    }
    Object *result = flisp_eval(interp, input);
    if (result->type != type_error)
        return;
    msg_lisp_err(result);
    if (debug_mode)
        flisp_write_object(debug_fp, result, true);
    if (result->error  == out_of_memory)
        fatal("OOM, exiting..");
    return;
}
#else
void eval_string(char *format, ...)
{
    va_list args;
    va_start(args, format);
    if (fprintf(flisp_input, format, args) < 0) {
        msg("Error sending command to fLisp");
        return;
    }
    Object *result = flisp_eval_input(interp, false);
    if (FLISP_IS_OOM(result))
        fatal("OOM, exiting..");
    if (FLISP_IS_ERR(result)) {
        msg_lisp_err(result);
        if (debug_mode)
            flisp_write_object(debug_fp, result, true);
    }
}
#endif
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
