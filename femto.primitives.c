#include <curses.h>

#include "buffer.h"
#include "gap.h"
#include "key.h"
#include "command.h"
#include "search.h"

/************************* Editor Extensions **************************************/


#define DEFINE_EDITOR_FUNC(name)					\
    extern void name(void);                                             \
    Object *e_##name(Interpreter *interp, Object ** args, Object **env)	\
    {									\
        name();								\
        return t;                                                       \
    }

DEFINE_EDITOR_FUNC(beginning_of_buffer)
    DEFINE_EDITOR_FUNC(end_of_buffer)
    DEFINE_EDITOR_FUNC(left)
    DEFINE_EDITOR_FUNC(right)
DEFINE_EDITOR_FUNC(forward_word)
DEFINE_EDITOR_FUNC(backward_word)
DEFINE_EDITOR_FUNC(up)
DEFINE_EDITOR_FUNC(down)
DEFINE_EDITOR_FUNC(lnbegin)
DEFINE_EDITOR_FUNC(lnend)
DEFINE_EDITOR_FUNC(yank)
DEFINE_EDITOR_FUNC(update_display)
DEFINE_EDITOR_FUNC(clear_message_line)
DEFINE_EDITOR_FUNC(copy_region)
DEFINE_EDITOR_FUNC(set_mark)
DEFINE_EDITOR_FUNC(kill_region)
DEFINE_EDITOR_FUNC(delete)
DEFINE_EDITOR_FUNC(backspace)
DEFINE_EDITOR_FUNC(forward_page)
DEFINE_EDITOR_FUNC(backward_page)
DEFINE_EDITOR_FUNC(suspend)
DEFINE_EDITOR_FUNC(quit)
DEFINE_EDITOR_FUNC(eval_block)
DEFINE_EDITOR_FUNC(delete_other_windows)
DEFINE_EDITOR_FUNC(list_buffers)
DEFINE_EDITOR_FUNC(describe_bindings)
DEFINE_EDITOR_FUNC(describe_functions)
DEFINE_EDITOR_FUNC(split_window)
DEFINE_EDITOR_FUNC(other_window)
DEFINE_EDITOR_FUNC(execute_key)

extern char *get_clipboard(void);
extern void set_scrap(unsigned char *);
extern int add_mode_current_buffer(char *);
extern int delete_mode_current_buffer(char *);
extern int get_mode_current_buffer(char *);
extern int count_buffers(void);
extern void display_prompt_and_response(char *, char *);
extern void msg(char *,...);
extern void clear_message_line(void);
extern void log_message(char *);
extern void insert_string(char *);
extern void move_to_search_result(point_t);
extern point_t search_forward(char *);
extern point_t search_backwards(char *);
extern int goto_line(int);
extern int add_mode_global(char *);
extern char *get_version_string(void);
extern char *get_temp_file(void);

Object *e_get_char(Interpreter *interp, Object **args, Object **env) { return newStringWithLength(interp, get_char(), 1); }
Object *e_get_key(Interpreter *interp, Object **args, Object **env) { return newString(interp, get_input_key()); }
Object *e_get_key_name(Interpreter *interp, Object **args, Object **env) { return newString(interp, get_key_name()); }
Object *e_get_key_funcname(Interpreter *interp, Object **args, Object **env) { return newString(interp, get_key_funcname()); }
Object *e_get_clipboard(Interpreter *interp, Object **args, Object **env) { return newString(interp, get_clipboard()); }
Object *e_get_buffer_count(Interpreter *interp, Object **args, Object **env) { return newInteger(interp, count_buffers()); }

/* Display */
Object *e_refresh(Interpreter *interp, Object ** args, Object **env)
{
    refresh();
    return t;
}

Object *e_set_key(Interpreter *interp, Object **args, Object **env)
{
    return (1 == set_key(FLISP_ARG_ONE->string, FLISP_ARG_TWO->string) ? t : nil);
}


Object *e_set_clipboard(Interpreter *interp, Object **args, Object **env)
{
    /* gets freed by next call to set_clipboard */
    char *sub = strdup(FLISP_ARG_ONE->string);
    set_scrap((unsigned char *)sub);
    return t;
}

Object *e_get_temp_file(Interpreter *interp, Object **args, Object **env)
{
    char *fn = get_temp_file();
    return newStringWithLength(interp, fn, strlen(fn));
}

/* Buffers */
Object *e_add_mode_global(Interpreter *interp, Object **args, Object **env)
{
    return (1 == add_mode_global(FLISP_ARG_ONE->string) ? t : nil);
}

Object *e_add_mode(Interpreter *interp, Object **args, Object **env)
{
    return (1 == add_mode_current_buffer(FLISP_ARG_ONE->string) ? t : nil);
}

Object *e_delete_mode(Interpreter *interp, Object **args, Object **env)
{
    return (1 == delete_mode_current_buffer(FLISP_ARG_ONE->string) ? t : nil);
}

Object *e_get_mode(Interpreter *interp, Object **args, Object **env)
{
    return (1 == get_mode_current_buffer(FLISP_ARG_ONE->string) ? t : nil);
}

/** (buffer-fread stream[ size]) - read size bytes from stream into current buffer at point, return bytes read
 *  If buffer cannot hold size more bytes, -1 is returned.
 *  If size is omitted or nil, read until eof.
 */
Object *e_buffer_fread(Interpreter *interp, Object **args, Object **env)
{
    size_t len, size = 0;

    CHECK_TYPE(FLISP_ARG_ONE, type_stream, "(buffer-fread size stream) - stream");

    if (FLISP_HAS_ARG_TWO && FLISP_ARG_TWO != nil) {
        CHECK_TYPE(FLISP_ARG_TWO, type_integer, "(buffer-fread size stream) - size");
        if (FLISP_ARG_TWO->integer == 0)
            return newInteger(interp, 0);

        if (FLISP_ARG_TWO->integer < 0)
            exceptionWithObject(interp, FLISP_ARG_TWO, invalid_value, "(buffer-read size stream) - size is negative");
        len = buffer_fread(curbp, FLISP_ARG_ONE->fd, FLISP_ARG_TWO->integer);
        if (ferror(FLISP_ARG_ONE->fd))
            exceptionWithObject(interp, FLISP_ARG_ONE, io_error, "buffer_fread() failed: %s", strerror(errno));

        if (len == -1)
            exception(interp, out_of_memory, "buffer_fread() failed, could not grow current buffer");

        return newInteger(interp, len);
    }
    for (;;) {
        len = buffer_fread(curbp, FLISP_ARG_ONE->fd, BUFSIZ);

        if (ferror(FLISP_ARG_ONE->fd))
            exceptionWithObject(interp, FLISP_ARG_ONE, io_error, "buffer_fread() failed: %s", strerror(errno));

        if (len == -1)
            exception(interp, out_of_memory, "buffer_fread() failed, could not grow current buffer");
        size += len;

        if (feof(FLISP_ARG_ONE->fd))
            return newInteger(interp, size);
    }
}

/** (buffer-fwrite size stream) - write size bytes from current buffer at point to stream, return bytes written */
Object *e_buffer_fwrite(Interpreter *interp, Object **args, Object **env)
{
    CHECK_TYPE(FLISP_ARG_ONE, type_integer, "(buffer-fwrite size stream) - size");
    CHECK_TYPE(FLISP_ARG_TWO, type_stream, "(buffer-fwrite size stream) - stream");

    if (FLISP_ARG_ONE->integer == 0)
        return newInteger(interp, 0);

    if (FLISP_ARG_ONE->integer < 0)
        exceptionWithObject(interp, FLISP_ARG_ONE, invalid_value, "(buffer-fwrite size stream) - size is negative");

    size_t len = buffer_fwrite(curbp, FLISP_ARG_ONE->integer, FLISP_ARG_TWO->fd);
    if (ferror(FLISP_ARG_TWO->fd))
        exceptionWithObject(interp, FLISP_ARG_TWO, io_error, "buffer_fwrite() failed: %s", strerror(errno));

    return newInteger(interp, len);
}

Object *e_getfilename(Interpreter *interp, Object **args, Object **env)
{

    if (FLISP_HAS_ARG_TWO)
        strcpy(response_buf, FLISP_ARG_TWO->string);
    else
        response_buf[0] = '\0';

    if (!getfilename(FLISP_ARG_ONE->string, (char*) response_buf, NAME_MAX))
        return nil;

    return newString(interp, response_buf);
}

Object *e_find_buffer_by_fname(Interpreter *interp, Object **args, Object **env)
{
    if (FLISP_ARG_ONE->string[0] == '\0')
        return nil;

    buffer_t *bp = find_buffer_by_fname(FLISP_ARG_ONE->string);

    return bp == NULL ? nil : newString(interp, bp->name);
}

Object *e_set_buffer(Interpreter *interp, Object **args, Object **env)
{
    buffer_t *bp = find_buffer(FLISP_ARG_ONE->string, FALSE);

    if (!bp)
        exceptionWithObject(interp, FLISP_ARG_ONE, invalid_value, "(set-buffer buffer) - buffer does not exist");

    curbp = bp;
    return FLISP_ARG_ONE;
}
Object *e_buffer_next(Interpreter *interp, Object **args,Object **env)
{
    if (!(FLISP_HAS_ARGS))
        return newString(interp, bheadp->name);

    buffer_t *bp = find_buffer(FLISP_ARG_ONE->string, FALSE);

    if (!bp)
        exceptionWithObject(interp, FLISP_ARG_ONE, invalid_value, "(buffer-next buffer) - buffer does not exist");

    if (bp->b_next)
        return newString(interp, bp->b_next->name);

    return nil;
}

Object *e_switch_to_buffer(Interpreter *interp, Object **args, Object **env)
{
    buffer_t *bp = find_buffer(FLISP_ARG_ONE->string, FALSE);
    if (!bp) {
        bp = new_buffer(FLISP_ARG_ONE->string);
        if (!bp)
            exceptionWithObject(interp, FLISP_ARG_ONE, out_of_memory, "(generate-new-buffer name) failed, out of memory");
    }
    switch_to_buffer(bp);
    return FLISP_ARG_ONE;
}

Object *e_set_buffer_name(Interpreter *interp, Object **args, Object **env)
{
    buffer_t *buffer = search_buffer(FLISP_ARG_ONE->string);

    if (buffer != NULL)
        exceptionWithObject(interp, FLISP_ARG_ONE, invalid_value, "(set-buffer-name name) - name, already exists");

    if (!set_buffer_name(curbp, FLISP_ARG_ONE->string))
        exceptionWithObject(interp, FLISP_ARG_ONE, out_of_memory, "(set-buffer-name name) - name, failed to allocate string");
    return FLISP_ARG_ONE;
}

Object *e_delete_buffer(Interpreter *interp, Object **args, Object **env)
{
    buffer_t *buffer = find_buffer(FLISP_ARG_ONE->string, FALSE);
    if (buffer == NULL)
        exceptionWithObject(interp, FLISP_ARG_ONE, invalid_value, "(kill-buffer buffer) - buffer does not exist");

    return delete_buffer(buffer) ? t : nil;
}

Object *e_zero_buffer(Interpreter *interp, Object **args, Object **env)
{
    assert(curbp != NULL);
    zero_buffer(curbp);
    return nil;
}

/** (generate-new-buffer name) */
Object *e_new_buffer(Interpreter *interp, Object **args, Object **env)
{
    if (new_buffer(FLISP_ARG_ONE->string))
        return FLISP_ARG_ONE;
    exceptionWithObject(interp, FLISP_ARG_ONE, out_of_memory, "(generate-new-buffer name) failed, out of memory");
}
/** (set-visited-filename name) */
Object *e_set_buffer_filename(Interpreter *interp, Object **args, Object **env)
{
    if (FLISP_ARG_ONE == nil) {
        if (curbp->fname != NULL)
            free(curbp->fname);
        curbp->fname = NULL;
        return nil;
    }

    CHECK_TYPE(FLISP_ARG_ONE, type_string, "(set-visited-filename name) - name");
    curbp->fname = strdup(FLISP_ARG_ONE->string);
    if (curbp->fname == NULL)
        exception(interp, out_of_memory, "(set-visited-filename name) - name, cannot allocate memory for filename");
    curbp->modified = TRUE;
    return FLISP_ARG_ONE;
}


Object *e_get_buffer_name(Interpreter *interp, Object **args, Object **env)
{
    char buf[40];
    strcpy(buf, get_current_bufname());
    return newStringWithLength(interp, buf, strlen(buf));
}

Object *e_get_buffer_filename(Interpreter *interp, Object **args, Object **env)
{
    if (curbp->fname == NULL)
        return nil;

    return newString(interp, curbp->fname);
}

/* (buffer-flag-modified[ buffer[ bool]]) */
Object *e_buffer_flag_modified(Interpreter *interp, Object **args, Object **env)
{
    buffer_t *bp = curbp;

    if (FLISP_HAS_ARGS) {
        if (FLISP_ARG_ONE != nil) {
            CHECK_TYPE(FLISP_ARG_ONE, type_string, "(buffer-flag-modified[ buffer[ bool]])");
            bp = find_buffer(FLISP_ARG_ONE->string, FALSE);
            if (!bp)
                exceptionWithObject(interp, FLISP_ARG_ONE, invalid_value, "(buffer-flag-modified[ buffer[ bool]]) - buffer does not exist");
        }
        if (FLISP_HAS_ARG_TWO)
            bp->modified =  (FLISP_ARG_TWO != nil);
    }
    return bp->modified ? t : nil;
}

/* Interaction */
Object *e_show_prompt(Interpreter *interp, Object **args, Object **env)
{
    display_prompt_and_response(FLISP_ARG_ONE->string, FLISP_ARG_TWO->string);
    return t;
}

Object *e_prompt(Interpreter *interp, Object **args, Object **env)
{
    char response[81] = "";

    if (FLISP_HAS_ARG_TWO) {
        size_t len = strlen(FLISP_ARG_TWO->string);
        if (len > 80)
            len = 80;
        strncpy(response, FLISP_ARG_TWO->string, len);
        response[len] = '\0';
    }
    if (getinput(FLISP_ARG_ONE->string, response, 80))
        return newStringWithLength(interp, response, strlen(response));
    return nil;
}

Object *e_get_version_string(Interpreter *interp, Object **args, Object **env)
{
    char *ver = get_version_string();
    return newStringWithLength(interp, ver, strlen(ver));
}

Object *e_goto_line(Interpreter *interp, Object **args, Object **env)
{
    int result = goto_line(FLISP_ARG_ONE->integer);
    return (result == 1 ? t : nil);
}
Object *e_search_forward(Interpreter *interp, Object **args, Object **env)
{
    point_t founded = search_forward(FLISP_ARG_ONE->string);
    move_to_search_result(founded);
    return (founded == -1 ? nil : t);
}

Object *e_search_backward(Interpreter *interp, Object **args, Object **env)
{
    point_t founded = search_backwards(FLISP_ARG_ONE->string);
    move_to_search_result(founded);
    return (founded == -1 ? nil : t);
}

Object *e_getch(Interpreter *interp, Object **args, Object **env)
{
    char ch[2];
    ch[0] = (unsigned char)getch();
    ch[1] = '\0';
    return newStringWithLength(interp, ch, 1);
}

Object *e_message(Interpreter *interp, Object **args, Object **env)
{
    msg(FLISP_ARG_ONE->string);
    return t;
}

Object *e_log_message(Interpreter *interp, Object **args, Object **env)
{
    log_message(FLISP_ARG_ONE->string);
    return t;
}

Object *e_log_debug(Interpreter *interp, Object **args, Object **env)
{
    fl_debug(interp, "%s", FLISP_ARG_ONE->string);
    return t;
}

Object *e_insert_string(Interpreter *interp, Object **args, Object **env)
{
    insert_string(FLISP_ARG_ONE->string);
    return t;
}

extern void set_point(point_t);
extern point_t get_mark(void);
extern point_t get_point(void);
extern point_t get_point_max(void);

Object *e_set_point(Interpreter *interp, Object **args, Object **env)
{
    set_point(FLISP_ARG_ONE->integer);
    return t;
}

Object *e_get_mark(Interpreter *interp, Object **args, Object **env)
{
    return newInteger(interp, get_mark());
}

Object *e_get_point(Interpreter *interp, Object **args, Object **env)
{
    return newInteger(interp, get_point());
}

Object *e_get_point_max(Interpreter *interp, Object **args, Object **env)
{
    return newInteger(interp, get_point_max());
}

/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
