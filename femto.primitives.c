#include <stdlib.h>
#include <errno.h>
#include <string.h>

#include <curses.h>

#include "femto.h"
#include "window.h"
#include "buffer.h"
#include "gap.h"
#include "key.h"
#include "command.h"
#include "search.h"

#include "lisp.h"

#include <assert.h>


    
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
DEFINE_EDITOR_FUNC(scroll_up)
DEFINE_EDITOR_FUNC(scroll_down)
DEFINE_EDITOR_FUNC(suspend)
DEFINE_EDITOR_FUNC(quit)
DEFINE_EDITOR_FUNC(eval_block)
DEFINE_EDITOR_FUNC(delete_other_windows)
DEFINE_EDITOR_FUNC(list_buffers)
DEFINE_EDITOR_FUNC(describe_bindings)
DEFINE_EDITOR_FUNC(describe_functions)
DEFINE_EDITOR_FUNC(other_window)
DEFINE_EDITOR_FUNC(execute_key)

extern char *get_clipboard(void);
extern int count_buffers(void);
extern void msg(char *,...);
extern void clear_message_line(void);
extern void log_message(char *);
extern void insert_string(char *);
extern void move_to_search_result(point_t);
extern point_t search_forward(char *);
extern point_t search_backwards(char *);
extern int goto_line(int);
extern char *get_version_string(void);
extern char *get_temp_file(void);

Object *e_get_char(Interpreter *interp, Object **args, Object **env) { return newStringWithLength(interp, get_char(), 1); }
Object *e_get_key(Interpreter *interp, Object **args, Object **env) { return newString(interp, get_input_key()); }
Object *e_get_key_name(Interpreter *interp, Object **args, Object **env) { return newString(interp, get_key_name()); }
Object *e_get_key_funcname(Interpreter *interp, Object **args, Object **env) { return newString(interp, get_key_funcname()); }

Object *e_split_window(Interpreter *interp, Object **args, Object **env) { return (NULL == split_current_window()) ? nil : t; }

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

/* Selection aka Clipboard */
Object *e_get_clipboard(Interpreter *interp, Object **args, Object **env)
{
    if (scrap == NULL)
        return lisp_empty_string;
    return newString(interp, (char *)scrap);
}

Object *e_set_clipboard(Interpreter *interp, Object **args, Object **env)
{
    if (scrap != NULL)  free(scrap);
    scrap = (char_t *) strdup(FLISP_ARG_ONE->string);
    return scrap == NULL ? nil : t;
}

Object *e_get_temp_file(Interpreter *interp, Object **args, Object **env)
{
    char *fn = get_temp_file();
    return newStringWithLength(interp, fn, strlen(fn));
}

/* Buffers */

/* Helper function: return either current buffer or named buffer if first argument exists */
buffer_t *get_buffer_arg_one(Interpreter *interp, Object **args, char *signature)
{
    if (FLISP_ARG_ONE == nil)
        return curbp;
    if (FLISP_ARG_ONE->type != type_string)
        exceptionWithObject(interp, FLISP_ARG_ONE, wrong_type_argument,
                            "%s - expected %s, got: %s", signature,
                            type_string->string, FLISP_ARG_ONE->type->string);
    buffer_t *buffer = find_buffer(FLISP_ARG_ONE->string, false);
    if (buffer == NULL)
        exceptionWithObject(interp, FLISP_ARG_ONE, invalid_value,
                            "%s - buffer does not exist", signature);
    return buffer;
}

/* (buffer-mode[ buffer[ mode]]) => mode - gets or sets mode of buffer.
 * if buffer is not given or nil, use the current buffer
 */
Object *e_buffer_mode(Interpreter *interp, Object **args, Object **env)
{
    buffer_t *buffer = curbp;
    if (FLISP_HAS_ARGS) {
        buffer = get_buffer_arg_one(interp, args, "(buffer-mode[ buffer[ mode]])");
        if (FLISP_HAS_ARG_TWO) {
            CHECK_TYPE(FLISP_ARG_TWO, type_symbol, "buffer-mode[ buffer[ mode]]) - mode");
            buffer->mode = FLISP_ARG_TWO;
        }
    }
    return buffer->mode;
}

/* Buffer flags */
#define GET_SET_BUFFER_FLAG(FLAG)                                       \
    Object *e_buffer_##FLAG## _p(Interpreter *interp, Object **args, Object **env) \
    {                                                                   \
        buffer_t *buffer = curbp;                                       \
        if (FLISP_HAS_ARGS) {                                           \
            buffer = get_buffer_arg_one(interp, args, "(buffer-" #FLAG "-p[ buffer[ p]])"); \
            if (FLISP_HAS_ARG_TWO)                                      \
                buffer->FLAG = (FLISP_ARG_TWO != nil);                  \
        }                                                               \
        return buffer->FLAG ? t : nil;                                  \
    }                                                                   \

/* (buffer-modified-p[ buffer[ bool]]) */
GET_SET_BUFFER_FLAG(modified)
/* (buffer-overwrite-p[ buffer[ bool]]) */
GET_SET_BUFFER_FLAG(overwrite)
/* (buffer-readonly-p[ buffer[ bool]]) */
GET_SET_BUFFER_FLAG(readonly)
/* (buffer-undo-p[ buffer[ bool]]) */
GET_SET_BUFFER_FLAG(undo)
/* (buffer-special-p[ buffer[ bool]]) */
GET_SET_BUFFER_FLAG(special)


/* (buffer-filename[ buffer]) */
Object *e_get_buffer_filename(Interpreter *interp, Object **args, Object **env)
{
    buffer_t *buffer = curbp;

    if (FLISP_HAS_ARGS) {
        buffer = get_buffer_arg_one(interp, args, "(buffer-filename[ buffer])");
    }
    if (buffer->fname == NULL)
        return nil;

    return newString(interp, buffer->fname);
}

/** (buffer-fread stream[ size]) - read size bytes from stream into current buffer at point, return bytes read
 *  If buffer cannot hold size more bytes, -1 is returned.
 *  If size is omitted or nil, read until eof.
 */
Object *e_buffer_fread(Interpreter *interp, Object **args, Object **env)
{
    size_t len, size = 0;

    CHECK_TYPE(FLISP_ARG_ONE, type_stream, "(buffer-fread stream size) - stream");

    if (FLISP_HAS_ARG_TWO && FLISP_ARG_TWO != nil) {
        CHECK_TYPE(FLISP_ARG_TWO, type_integer, "(buffer-fread stream size) - size");
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

        end_of_buffer();

        if (feof(FLISP_ARG_ONE->fd))
            return newInteger(interp, size);
    }
}

/** (buffer-fwrite stream size) - write size bytes from current buffer at point to stream, return bytes written */
Object *e_buffer_fwrite(Interpreter *interp, Object **args, Object **env)
{
    size_t len;

    CHECK_TYPE(FLISP_ARG_ONE, type_stream, "(buffer-fwrite stream size) - stream");
    if (FLISP_HAS_ARG_TWO) {
        CHECK_TYPE(FLISP_ARG_TWO, type_stream, "(buffer-fwrite stream size) - size");
        if (FLISP_ARG_TWO->integer == 0)
            return newInteger(interp, 0);
        if (FLISP_ARG_TWO->integer < 0)
            exceptionWithObject(interp, FLISP_ARG_TWO, invalid_value, "(buffer-fwrite stream size) - size is negative");
        len = FLISP_ARG_TWO->integer;
    } else {
        len = get_point_max() - get_point();
    }
    len = buffer_fwrite(curbp, FLISP_ARG_ONE->fd, len);
    if (ferror(FLISP_ARG_ONE->fd))
        exceptionWithObject(interp, FLISP_ARG_ONE, io_error, "buffer_fwrite() failed: %s", strerror(errno));

    return newInteger(interp, len);
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
    buffer_t *bp = find_buffer(FLISP_ARG_ONE->string, false);

    if (!bp)
        exceptionWithObject(interp, FLISP_ARG_ONE, invalid_value, "(set-buffer buffer) - buffer does not exist");

    curbp = bp;
    return FLISP_ARG_ONE;
}
Object *e_buffer_next(Interpreter *interp, Object **args,Object **env)
{
    if (!(FLISP_HAS_ARGS))
        return newString(interp, curbp->name);

    buffer_t *bp = find_buffer(FLISP_ARG_ONE->string, false);

    if (!bp)
        exceptionWithObject(interp, FLISP_ARG_ONE, invalid_value, "(buffer-next buffer) - buffer does not exist");

    return newString(interp, bp->b_next->name);
}

Object *e_buffer_show(Interpreter *interp, Object **args, Object **env)
{
    buffer_t *bp = find_buffer(FLISP_ARG_ONE->string, true);
    if (!bp)
        exceptionWithObject(interp, FLISP_ARG_ONE, out_of_memory, "(generate-new-buffer name) failed, out of memory");

    disassociate_b(curwp);
    pull_buffer(bp);
    associate_b2w(curbp,curwp);

    return FLISP_ARG_ONE;
}

Object *e_set_buffer_name(Interpreter *interp, Object **args, Object **env)
{
    buffer_t *buffer = find_buffer(FLISP_ARG_ONE->string, false);

    if (buffer != NULL)
        exceptionWithObject(interp, FLISP_ARG_ONE, invalid_value, "(set-buffer-name name) - name, already exists");

    if (!set_buffer_name(curbp, FLISP_ARG_ONE->string))
        exceptionWithObject(interp, FLISP_ARG_ONE, out_of_memory, "(set-buffer-name name) - name, failed to allocate string");
    return FLISP_ARG_ONE;
}

Object *e_delete_buffer(Interpreter *interp, Object **args, Object **env)
{
    buffer_t *buffer = find_buffer(FLISP_ARG_ONE->string, false);
    if (buffer == NULL)
        exceptionWithObject(interp, FLISP_ARG_ONE, invalid_value, "(delete-buffer buffer) - buffer does not exist");
    if (!delete_buffer(buffer))
        exceptionWithObject(interp, FLISP_ARG_ONE, invalid_value, "(delete-buffer buffer) - refused to delete scratch or current buffer");
    return FLISP_ARG_ONE;
}

Object *e_zero_buffer(Interpreter *interp, Object **args, Object **env)
{
    assert(curbp != NULL);
    zero_buffer(curbp);
    return nil;
}

/** (get-buffer-create name) */
Object *e_get_buffer_create(Interpreter *interp, Object **args, Object **env)
{
    if (find_buffer(FLISP_ARG_ONE->string, true))
        return FLISP_ARG_ONE;
    exceptionWithObject(interp, FLISP_ARG_ONE, out_of_memory, "(get-buffer-create name) failed, out of memory");
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


/* Interaction */
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

/** (prompt-filename prompt[ default]) */
Object *e_prompt_filename(Interpreter *interp, Object **args, Object **env)
{

    if (FLISP_HAS_ARG_TWO)
        strcpy(response_buf, FLISP_ARG_TWO->string);
    else
        response_buf[0] = '\0';

    char *prompt = strdup(FLISP_ARG_ONE->string);
    if (!getfilename(prompt, (char*) response_buf, NAME_MAX)) {
        free(prompt);
        return nil;
    }
    free(prompt);
    return newString(interp, response_buf);
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


/* Note: we should move this to Lisp */
void list_buffers(void)
{
    buffer_t *bp;
    buffer_t *list_bp;
    char mod_ch, over_ch;
    char blank[] = " ";
    static char report_line[NAME_MAX + 40];
    char *bn;
    char *fn;

    list_bp = find_buffer(str_buffers, true);
    list_bp->special = 1;

    /* Notes: should'n we use popup-buffer here? */
    disassociate_b(curwp); /* we are leaving the old buffer for a new one */
    pull_buffer(list_bp);
    associate_b2w(curbp, curwp);
    zero_buffer(curbp); /* throw away previous content */

    /*             12 1234567 12345678901234567 */
    insert_string("CO    Size Buffer           File\n");
    insert_string("-- ------- ------           ----\n");

    for (bp = curbp->b_next;  bp != curbp; bp = bp->b_next) {
        mod_ch  = (bp->modified ? '*' : ' ');
        over_ch = (bp->overwrite ? 'O' : ' ');
        bn = (bp->name == NULL) ? blank : bp->name;
        fn = (bp->fname == NULL) ? blank : bp->fname;
        snprintf(report_line, sizeof(report_line),  "%c%c %7d %-16s %s\n",  mod_ch, over_ch, bp->b_size, bn, fn);
        insert_string(report_line);
    }
}

Primitive femto_primitives[] = {
/* Text manipulation: read from, write to buffer text */

    {"backspace",                 0, 0, 0,            e_backspace},
    {"delete",                    0, 0, 0,            e_delete},
    {"erase-buffer",              0, 0, 0,            e_zero_buffer},
    {"get-char",                  0, 0, 0,            e_get_char},
    {"insert-string",             1, 1, TYPE_STRING,  e_insert_string},
    {"kill-region",               0, 0, 0,            e_kill_region},
    {"yank",                      0, 0, 0,            e_yank},

/* Selection */
    {"copy-region",               0, 0, 0,            e_copy_region},
    {"get-clipboard",             0, 0, 0,            e_get_clipboard},
    {"get-mark",                  0, 0, 0,            e_get_mark},
    {"set-clipboard",             1, 1, TYPE_STRING,  e_set_clipboard},
    {"set-mark",                  0, 0, 0,            e_set_mark},

/* Cursor Movement and information */
    {"backward-char",             0, 0, 0,            e_left},
    {"backward-word",             0, 0, 0,            e_backward_word},
    {"beginning-of-buffer",       0, 0, 0,            e_beginning_of_buffer},
    {"beginning-of-line",         0, 0, 0,            e_lnbegin},
    {"end-of-buffer",             0, 0, 0,            e_end_of_buffer},
    {"end-of-line",               0, 0, 0,            e_lnend},
    {"forward-char",              0, 0, 0,            e_right},
    {"forward-word",              0, 0, 0,            e_forward_word},
    {"get-point",                 0, 0, 0,            e_get_point},
    {"get-point-max",             0, 0, 0,            e_get_point_max},
    {"goto-line",                 1, 1, TYPE_INTEGER, e_goto_line},
    {"next-line",                 0, 0, 0,            e_down},
    {"previous-line",             0, 0, 0,            e_up},
    {"scroll-up",                 0, 0, 0,            e_scroll_up},
    {"scroll-down",               0, 0, 0,            e_scroll_down},
    {"search-forward",            1, 1, TYPE_STRING,  e_search_forward},
    {"search-backward",           1, 1, TYPE_STRING,  e_search_backward},
    {"set-point",                 1, 1, TYPE_INTEGER, e_set_point},

/* Buffer Management and information */
    {"find-buffer-visiting",      1, 1, TYPE_STRING,  e_find_buffer_by_fname},
    {"buffer-filename",           0, 1, TYPE_STRING,  e_get_buffer_filename},
    {"buffer-fread",              1, 2, 0,            e_buffer_fread},
    {"buffer-fwrite",             1, 2, 0,            e_buffer_fwrite},
    {"buffer-mode",               0, 2, 0,            e_buffer_mode},
    {"buffer-modified-p",         0, 2, 0,            e_buffer_modified_p},
    {"buffer-overwrite-p",        0, 2, 0,            e_buffer_overwrite_p},
    {"buffer-readonly-p",         0, 2, 0,            e_buffer_readonly_p},
    {"buffer-special-p",          0, 2, 0,            e_buffer_special_p},
    {"buffer-undo-p",             0, 2, 0,            e_buffer_undo_p},
    {"buffer-next",               0, 1, TYPE_STRING,  e_buffer_next},
    {"buffer-show",               1, 1, TYPE_STRING,  e_buffer_show},
    {"delete-buffer",             1, 1, TYPE_STRING,  e_delete_buffer},
    {"get-buffer-create",         1, 1, TYPE_STRING,  e_get_buffer_create},
    {"list-buffers",              0, 0, 0,            e_list_buffers},
    {"set-buffer",                1, 1, TYPE_STRING,  e_set_buffer},
    {"set-buffer-name",           1, 1, TYPE_STRING,  e_set_buffer_name},
    {"set-visited-filename",      1, 1, 0,            e_set_buffer_filename},

/* Window Handling */
    {"delete-other-windows",      0, 0, 0,            e_delete_other_windows},
    {"split-window",              0, 0, 0,            e_split_window},
    {"other-window",              0, 0, 0,            e_other_window},
    {"update-display",            0, 0, 0,            e_update_display},
    {"refresh",                   0, 0, 0,            e_refresh},

/* Message Line */
    {"message",                   1, 1, TYPE_STRING,  e_message},
    {"prompt-filename",           1, 2, TYPE_STRING,  e_prompt_filename},
    {"clear-message-line",        0, 0, 0,            e_clear_message_line},
    {"prompt",                    1, 2, TYPE_STRING,  e_prompt},

/* Keyboard Handling */
    {"describe-bindings",         0, 0, 0,            e_describe_bindings},
    {"describe-functions",        0, 0, 0,            e_describe_functions},
    {"execute-key",               0, 0, 0,            e_execute_key},
    {"getch",                     0, 0, 0,            e_getch},
    {"get-key",                   0, 0, 0,            e_get_key},
    {"get-key-funcname",          0, 0, 0,            e_get_key_funcname},
    {"get-key-name",              0, 0, 0,            e_get_key_name},
    {"set-key",                   2, 2, TYPE_STRING,  e_set_key},

/* Programming and System Interaction */
    {"suspend",                   0, 0, 0,            e_suspend},
    {"exit",                      0, 0, 0,            e_quit},
    {"get-temp-file",             0, 0, 0,            e_get_temp_file},
    {"log-message",               1, 1, TYPE_STRING,  e_log_message},
    {"log-debug",                 1, 1, TYPE_STRING,  e_log_debug},
    {"eval-block",                0, 0, 0,            e_eval_block},
    {"get-version-string",        0, 0, 0,            e_get_version_string}
};
    
void femto_register(Interpreter *interp)
{
    int i;
    
    femto_buffer_register(interp);
    debug("femto buffer module registered\n");

    for (i = 0; i < sizeof(femto_primitives) / sizeof(femto_primitives[0]); i++)
        lisp_register_primitive(interp, &femto_primitives[i]);
    debug("femto primitives registered\n");
}
/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
