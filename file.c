#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "lisp.h"
#include "file.h"

Object *permission_denied = &(Object) { NULL, .string = "permission-denied" };
Object *not_found = &(Object) { NULL, .string = "not-found" };

Constant flisp_file_constants[] = {
    { &permission_denied, &permission_denied },
    { &not_found, &not_found },
    { NULL, NULL }
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
Object *primitiveFseek(Interpreter *interp, Object** args, Object **env)
{
    int result, whence = SEEK_SET;
    CHECK_TYPE(FLISP_ARG_ONE, type_stream,  "(fseek stream offset) - stream");
    if (FLISP_ARG_ONE->fd == NULL)
        exception(interp, invalid_value, "(fseek stream) - stream already closed");
    CHECK_TYPE(FLISP_ARG_TWO, type_integer, "(fseek stream offset) - offset");
    if (FLISP_ARG_TWO->integer < 0)
        whence = SEEK_END;
    result = fseeko(FLISP_ARG_ONE->fd, FLISP_ARG_TWO->integer, whence);
    if (result == -1)
        exception(interp, io_error, "fseeko() failed: %s", strerror(errno));

    return newInteger(interp, ftello(FLISP_ARG_ONE->fd));
}
Object *primitiveFtell(Interpreter *interp, Object** args, Object **env)
{
    if (FLISP_ARG_ONE->fd == NULL)
        exception(interp, invalid_value, "(ftell stream) - stream already closed");
    return newInteger(interp, ftello(FLISP_ARG_ONE->fd));
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

Object *primitiveFstat(Interpreter *interp, Object** args, Object **env)
{
    struct stat info;
    int result;
    Object *object;
    char *type;

    CHECK_TYPE(FLISP_ARG_ONE, type_string,  "(fstat string[ linkp]) - stream");

    if (FLISP_HAS_ARG_TWO && FLISP_ARG_TWO != nil)
        result = lstat(FLISP_ARG_ONE->string, &info);
    else
        result = stat(FLISP_ARG_ONE->string, &info);

    if (result == -1) {
        switch(errno) {
        case EACCES:
            exceptionWithObject(interp, FLISP_ARG_ONE, permission_denied, "(fstat string[ linkp]): %s", strerror(errno));
            break;
        case ENOENT:
        case ENOTDIR:
            exceptionWithObject(interp, FLISP_ARG_ONE, not_found, "(fstat string[ linkp]): %s", strerror(errno));
            break;
        case ENAMETOOLONG:
            exceptionWithObject(interp, FLISP_ARG_ONE, invalid_value, "(fstat string[ linkp]): %s", strerror(errno));
        }
        exceptionWithObject(interp, FLISP_ARG_ONE, io_error, "l/stat() failed: %s", strerror(errno));
    }

    /* (size _size_ type _type_ mode _mode_ uid _uid_ gid _gid_ ) */
    GC_CHECKPOINT;
    if      (S_ISBLK(info.st_mode)) type = "b";
    else if (S_ISCHR(info.st_mode)) type = "c";
    else if (S_ISDIR(info.st_mode)) type = "d";
    else if (S_ISFIFO(info.st_mode)) type = "p";
    else if (S_ISREG(info.st_mode)) type = "f";
    else if (S_ISLNK(info.st_mode)) type = "l";
    else if (S_ISSOCK(info.st_mode)) type = "s";
    else if (S_TYPEISMQ(info)) type = "Q";
    else if (S_TYPEISSEM(info)) type = "S";
    else if (S_TYPEISSHM(info)) type = "M";
    else if (S_TYPEISTMO(info)) type = "T";
    else type = "-";

    object = newString(interp, type);
    GC_TRACE(gcObject, object);
    object = newCons(interp, gcObject, &nil);
    GC_TRACE(gcResult, object);

    *gcObject = newSymbol(interp, "type");
    *gcResult = newCons(interp, gcObject, gcResult);

    *gcObject = newInteger(interp, info.st_gid);
    *gcResult = newCons(interp, gcObject, gcResult);

    *gcObject = newSymbol(interp, "gid");
    *gcResult = newCons(interp, gcObject, gcResult);

    *gcObject = newInteger(interp, info.st_uid);
    *gcResult = newCons(interp, gcObject, gcResult);

    *gcObject = newSymbol(interp, "uid");
    *gcResult = newCons(interp, gcObject, gcResult);

    *gcObject = newInteger(interp, info.st_mode & 07777);
    *gcResult = newCons(interp, gcObject, gcResult);

    *gcObject = newSymbol(interp, "mode");
    *gcResult = newCons(interp, gcObject, gcResult);

    *gcObject = newInteger(interp, info.st_size);
    *gcResult = newCons(interp, gcObject, gcResult);

    *gcObject = newSymbol(interp, "size");
    *gcResult = newCons(interp, gcObject, gcResult);

    GC_RETURN(*gcResult);
}


Primitive flisp_file_primitives[] = {
    {"fflush", 1, 1, TYPE_STREAM, primitiveFflush},
    {"fseek",  2, 3, 0,           primitiveFseek},
    {"ftell",  1, 1, TYPE_STREAM, primitiveFtell},
    {"feof",   1, 1, TYPE_STREAM, primitiveFeof},
    {"fgetc",  1, 1, TYPE_STREAM, primitiveFgetc},
    {"fgets",  1, 1, TYPE_STREAM, primitiveFgets},
    {"popen",  2, 2, TYPE_STRING, primitivePopen},
    {"pclose", 1, 1, TYPE_STREAM, primitivePclose},
    {"fstat",  1, 2, 0,           primitiveFstat},
};


/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
