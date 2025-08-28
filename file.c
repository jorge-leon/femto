#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "lisp.h"
#include "file.h"

Primitive flisp_file_primitives[] = {
    {"fflush", 1, 1, TYPE_STREAM, primitiveFflush},
    {"ftell",  1, 1, TYPE_STREAM, primitiveFtell},
    {"feof",   1, 1, TYPE_STREAM, primitiveFeof},
    {"fgetc",  1, 1, TYPE_STREAM, primitiveFgetc},
    {"fgets",  1, 1, TYPE_STREAM, primitiveFgets},
    {"popen",  2, 2, TYPE_STRING, primitivePopen},
    {"pclose", 1, 1, TYPE_STREAM, primitivePclose},
};

/** file_fflush - flush output stream
 *
 * @param interp  fLisp interpreter
 * @param stream  open output stream
 *
 * returns: 0 on success, erno otherwise
 *
 * Public C Interface
 */
int file_fflush(Interpreter *interp, Object *stream)
{
    return (fflush(stream->fd) == EOF) ? errno : 0;
}
Object *primitiveFflush(Interpreter *interp, Object** args, Object **env)
{
    if (FLISP_ARG_ONE->fd == NULL)
        exception(interp, invalid_value, "(fflush stream) - stream already closed");
    return newInteger(interp, file_fflush(interp, FLISP_ARG_ONE));
}

off_t file_ftell(Interpreter *interp, Object *stream)
{
    return ftello(stream->fd);
}
Object *primitiveFtell(Interpreter *interp, Object** args, Object **env)
{
    if (FLISP_ARG_ONE->fd == NULL)
        exception(interp, invalid_value, "(ftell stream) - stream already closed");
    return newInteger(interp, file_ftell(interp, FLISP_ARG_ONE));
}
Object *primitiveFeof(Interpreter *interp, Object** args, Object **env)
{
    if (FLISP_ARG_ONE->fd == NULL)
        exception(interp, invalid_value, "(feof stream) - stream already closed");
    return (feof(FLISP_ARG_ONE->fd)) ? t : nil;
}

Object *primitiveFgetc(Interpreter *interp, Object** args, Object **env)
{
    char s[] = "\0\0";

    int c = getc(FLISP_ARG_ONE->fd);
    if (c == EOF)
        return nil;
    s[0] = (char)c;
    return newString(interp, s);
}
Object *primitiveFgets(Interpreter *interp, Object** args, Object **env)
{
    Object *string = nil;
    char *input;

    if (FLISP_ARG_ONE->fd == NULL)
        exception(interp, invalid_value, "(fgets stream) - stream already closed");

    input = malloc(INPUT_FMT_BUFSIZ);
    if(input == NULL)
        exception(interp, out_of_memory, "fgets() failed, %s", strerror(errno));

    *input = '\0';

    if(fgets(input, INPUT_FMT_BUFSIZ, FLISP_ARG_ONE->fd) != NULL) {
        string = newString(interp, input);
        free(input);
        return string;
    }
    free(input);
    if (!feof(FLISP_ARG_ONE->fd))
        exceptionWithObject(interp, FLISP_ARG_ONE, io_error, "fgetc() failed: %s", strerror(errno));
    return end_of_file;
}

Object *primitivePopen(Interpreter *interp, Object** args, Object **env)
{
    FILE *fd;

    if (strcmp(FLISP_ARG_TWO->string, "r") && strcmp(FLISP_ARG_TWO->string, "w"))
         exception(interp, invalid_value,
                   "(popen path mode) - mode must be \"r\" or \"w\", got: %s", FLISP_ARG_TWO->string);

    fd = popen(FLISP_ARG_ONE->string, FLISP_ARG_TWO->string);
    if (fd == NULL)
        exception(interp, io_error, "popen() failed: %s", strerror(errno));

    return newStreamObject(interp, fd, FLISP_ARG_ONE->string);
}
Object *primitivePclose(Interpreter *interp, Object** args, Object **env)
{
    int result = pclose(FLISP_ARG_ONE->fd);

    if (result == -1)
        exceptionWithObject(interp, FLISP_ARG_ONE, io_error, "pclose() failed: %s", strerror(errno));

    return newInteger(interp, result);
}

/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
