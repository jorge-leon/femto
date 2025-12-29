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
#include "flisp/file.h"
#ifdef FLISP_DOUBLE_EXTENSION
#include "flisp/double.h"
#endif

void gui(void); /* The GUI loop used in interactive mode */

static Interpreter *interp;
char debug_file[] = "debug.out";
FILE *prev, *debug_fp = NULL;
char* output;
size_t len;

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

    if ((init_file = getenv("FEMTORC")) == NULL)
        init_file = CPP_XSTR(E_INITFILE);

    if ((init_fd = fopen(init_file, "r")) == NULL)
        debug("failed to open rc file %s: %s\n", init_file, strerror(errno));

    interp = lisp_new(FLISP_INITIAL_MEMORY, argv, NULL, init_fd, debug_fp, debug_fp);
    if (interp == NULL)
        fatal("fLisp initialization failed");
    lisp_file_register(interp);
#ifdef FLISP_DOUBLE_EXTENSION
    lisp_double_register(interp);
    debug("double extension registered\n");
#endif
    femto_register(interp);
    debug("evaluating rc file %s\n", init_file);
    lisp_eval(interp, NULL);
    if (FLISP_RESULT_CODE(interp) != nil) {
        debug("failed to load rc file %s:\n", init_file);
        lisp_write_error(interp, debug_fp);
        if (FLISP_RESULT_CODE(interp) == out_of_memory)
            fatal("OOM, exiting..");
    }
    if (fclose(init_fd))
        debug("failed to close rcfile %s: %s\n", init_file, strerror(errno));
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
    femto_register(interp);

    debug("start\n");

    /* GUI */
    if (!batch_mode) gui();

    debug("main(): shutdown\n");
    // Note: exit frees all memory, do we need this here?
    // Note: we can't do
    //lisp_destroy(interp);
    //here, because we get segfaults in wide character routines.

    // Note: the following lines sometimes free not-allocated memory
    // if (scrap != NULL) free(scrap);
    return 0;
}

/** Handle errors from Lisp scripts
 *
 * @param interp
 */
void msg_lisp_err(Interpreter *interp)
{
    char *buf;
    size_t len;
    FILE *fd;

    if (NULL == (fd = open_memstream(&buf, &len)))
        fatal("failed to allocate error formatting buffer");
    lisp_write_error(interp, fd);
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
 * @returns
 * - NULL  if an error occured formatting the input string or
 *         evaluating it.
 * - A pointer to a string buffer with the fLisp interpreter output.
 *   After use this buffer has to be freed with free_lisp_output().
 */
char *eval_string(bool do_format, char *format, ...)
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
            return NULL;
        }
        input = buf;
    } else {
        input = format;
    }

    prev = interp->output;  // Note: save for double invocation with user defined functions.
    interp->output = open_memstream(&output, &len);
    lisp_eval(interp, input);
    if (interp->output)
        fflush(interp->output);
    if (FLISP_RESULT_CODE(interp) == nil)
        return output;
    msg_lisp_err(interp);
    if (debug_mode) {
        lisp_write_error(interp, debug_fp);
        debug("=> %s\n", output);
    }
    if (FLISP_RESULT_CODE(interp) == out_of_memory)
        fatal("OOM, exiting..");
    free_lisp_output();
    return NULL;
}
void free_lisp_output(void)
{
    if (!interp->output)
        return;
    fclose(interp->output);
    free(output);
    interp->output = prev;
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
