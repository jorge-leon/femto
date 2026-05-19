/*
 * command.c, femto, Hugh Barney, Public Domain, 2017
 * Derived from: Anthony's Editor January 93, (Public Domain 1991, 1993 by Anthony Howe)
 */

#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>
#include <curses.h>
#include <signal.h>

#include <assert.h>

#include "femto.h"
#include "window.h"
#include "undo.h"
#include "buffer.h"
#include "gap.h"
#include "key.h"
#include "display.h"
#include "command.h"
#include "search.h"

int prev_utf8_char_size(void)
{
    int n;
    for (n=2;n<5;n++)
        if (-1 < curbp->b_point - n && (utf8_size(*(ptr(curbp, curbp->b_point - n))) == n))
            return n;
    return 1;
}

/* The order of functions corresponds to the order of registration and
 * documentation */

/* Text manipulation: read from, write to buffer text */
void backspace(void)
{
    char_t the_char[7]; /* the deleted char, allow 6 unsigned chars plus a null */
    int n = prev_utf8_char_size();

    curbp->b_point = movegap(curbp, curbp->b_point);

    if (curbp->b_buf < (curbp->b_gap - (n - 1)) ) {
        curbp->b_gap -= n; /* increase start of gap by size of char */
        curbp->modified = TRUE;

        /* record the backspaced chars in the undo structure */
        memcpy(the_char, curbp->b_gap, n);
        the_char[n] = '\0'; /* null terminate, the backspaced char(s) */
        curbp->b_point = pos(curbp, curbp->b_egap);
        //debug("point after bs = %ld\n", curbp->b_point);
        add_undo(curbp, UNDO_T_BACKSPACE, curbp->b_point, the_char, NULL);
    }

    curbp->b_point = pos(curbp, curbp->b_egap);
}
DEFINE_EDITOR_FUNC(backspace)

void delete(void)
{
    char_t the_char[7]; /* the deleted char, allow 6 unsigned chars plus a null */
    int n;

    curbp->b_point = movegap(curbp, curbp->b_point);
    n = utf8_size(*(ptr(curbp, curbp->b_point)));

    if (curbp->b_egap < curbp->b_ebuf) {
        /* record the deleted chars in the undo structure */
        memcpy(the_char, curbp->b_egap, n);
        the_char[n] = '\0'; /* null terminate, the deleted char(s) */
        //debug("deleted = '%s'\n", the_char);
        curbp->b_egap += n;
        curbp->b_point = pos(curbp, curbp->b_egap);
        curbp->modified = TRUE;
        add_undo(curbp, UNDO_T_DELETE, curbp->b_point, the_char, NULL);
    }
}
DEFINE_EDITOR_FUNC(delete)

Object *e_zero_buffer(Object *interp, Object **args, Object **env, size_t nArgs)
{
    assert(curbp != NULL);
    zero_buffer(curbp);
    return nil;
}

Object *e_get_char(Object *interp, Object **args, Object **env, size_t nArgs)
{
    static char ch[2] = "\0";
    ch[0] = (char)*(ptr(curbp, curbp->b_point));
    return newStringWithLength(interp, ch, 1);
}

Object *e_insert_string(Object *interp, Object **args, Object **env, size_t nArgs)
{
    insert_string(FLISP_ARG1->string);
    return t;
}


void unmark(void)
{
    assert(curbp != NULL);
    curbp->b_mark = NOMARK;
}

/* Length of scrap buffer. */
point_t nscrap;

void copy_cut(int cut)
{
    char_t *p;
    /* if no mark or point == marker, nothing doing */
    if (curbp->b_mark == NOMARK || curbp->b_point == curbp->b_mark)
        return;
    if (scrap != NULL) {
        free(scrap);
        scrap = NULL;
    }

    if (curbp->b_point < curbp->b_mark) {
        /* point above mark: move gap under point, region = mark - point */
        (void) movegap(curbp, curbp->b_point);
        /* moving the gap can impact the pointer so sure get the pointer after the move */
        p = ptr(curbp, curbp->b_point);
        nscrap = curbp->b_mark - curbp->b_point;
    } else {
        /* if point below mark: move gap under mark, region = point - mark */
        (void) movegap(curbp, curbp->b_mark);
        /* moving the gap can impact the pointer so sure get the pointer after the move */
        p = ptr(curbp, curbp->b_mark);
        nscrap = curbp->b_point - curbp->b_mark;
    }
    if ((scrap = (char_t*) malloc(nscrap + 1)) == NULL) {
        msg(m_alloc);
    } else {
        (void) memcpy(scrap, p, nscrap * sizeof (char_t));
        *(scrap + nscrap) = '\0';  /* null terminate for insert_string */
        if (cut) {
            //debug("CUT: pt=%ld nscrap=%d\n", curbp->b_point, nscrap);
            add_undo(curbp, UNDO_T_KILL, (curbp->b_point < curbp->b_mark ? curbp->b_point : curbp->b_mark), scrap, NULL);
            curbp->b_egap += nscrap; /* if cut expand gap down */
            curbp->b_point = pos(curbp, curbp->b_egap); /* set point to after region */
            curbp->modified = TRUE;
            msg(m_cut, nscrap);
        } else {
            msg(m_copied, nscrap);
        }
        unmark();
    }
}

int i_check_region(void)
{
    if (curbp->b_mark == NOMARK) {
        msg(m_nomark);
        return FALSE;
    }

    if (curbp->b_point == curbp->b_mark) {
        msg(m_noregion);
        return FALSE;
    }
    return TRUE;
}


void kill_region(void) {
    if (i_check_region() == FALSE) return;
    copy_cut(TRUE);
}
DEFINE_EDITOR_FUNC(kill_region)

void yank(void)
{
    insert_string((char *)scrap);
}
DEFINE_EDITOR_FUNC(yank)

/* Selection */
void copy_region(void) {
    if (i_check_region() == FALSE) return;
    copy_cut(FALSE);
}
DEFINE_EDITOR_FUNC(copy_region)

/* Selection aka Clipboard */
Object *e_get_clipboard(Object *interp, Object **args, Object **env, size_t nArgs)
{
    if (scrap == NULL)
        return flisp_empty_string;
    return newString(interp, (char *)scrap);
}

Object *e_get_mark(Object *interp, Object **args, Object **env, size_t nArgs) { return newInteger(interp, curbp->b_mark); }

Object *e_set_clipboard(Object *interp, Object **args, Object **env, size_t nArgs)
{
    if (scrap != NULL)  free(scrap);
    scrap = (char_t *) strdup(FLISP_ARG1->string);
    return scrap == NULL ? nil : t;
}

void set_mark(void)
{
    curbp->b_mark = (curbp->b_mark == curbp->b_point ? NOMARK : curbp->b_point);
}
DEFINE_EDITOR_FUNC(set_mark)


/* Cursor Movement and information */
void left(void)
{
    int n = prev_utf8_char_size();

    while (0 < curbp->b_point && n-- > 0)
        --curbp->b_point;
}
DEFINE_EDITOR_FUNC(left)

void backward_word(void)
{
    char_t *p;
    while (!isspace(*(p = ptr(curbp, curbp->b_point))) && curbp->b_buf < p)
        --curbp->b_point;
    while (isspace(*(p = ptr(curbp, curbp->b_point))) && curbp->b_buf < p)
        --curbp->b_point;
}
DEFINE_EDITOR_FUNC(backward_word)

void beginning_of_buffer(void)
{
    curbp->b_point = 0;
}
DEFINE_EDITOR_FUNC(beginning_of_buffer)

void lnbegin(void)
{
    curbp->b_point = segstart(curbp, lnstart(curbp,curbp->b_point), curbp->b_point);
}
DEFINE_EDITOR_FUNC(lnbegin)

void end_of_buffer(void)
{
    curbp->b_point = pos(curbp, curbp->b_ebuf);
    if (curbp->b_epage < pos(curbp, curbp->b_ebuf)) curbp->b_reframe = 1;
}
DEFINE_EDITOR_FUNC(end_of_buffer)

void lnend(void)
{
        if (curbp->b_point == pos(curbp, curbp->b_ebuf)) return; /* do nothing if EOF */
    curbp->b_point = dndn(curbp, curbp->b_point);
    point_t p = curbp->b_point;
    left();
    curbp->b_point = (*ptr(curbp, curbp->b_point) == '\n') ? curbp->b_point : p;
}
DEFINE_EDITOR_FUNC(lnend)

void right(void)
{
    int n = utf8_size(*ptr(curbp,curbp->b_point));

    while ((curbp->b_point < pos(curbp, curbp->b_ebuf)) && n-- > 0)
        ++curbp->b_point;
}
DEFINE_EDITOR_FUNC(right)

void forward_word(void)
{
    char_t *p;
    while (!isspace(*(p = ptr(curbp, curbp->b_point))) && p < curbp->b_ebuf)
        ++curbp->b_point;
    while (isspace(*(p = ptr(curbp, curbp->b_point))) && p < curbp->b_ebuf)
        ++curbp->b_point;
}
DEFINE_EDITOR_FUNC(forward_word)

/* return point in current buffer */
point_t get_point(void) { return curbp->b_point; }
Object *e_get_point(Object *interp, Object **args, Object **env, size_t nArgs) { return newInteger(interp, curbp->b_point); }

/* return point in current buffer */
point_t get_point_max(void) { return pos(curbp, curbp->b_ebuf); }
Object *e_get_point_max(Object *interp, Object **args, Object **env, size_t nArgs) { return newInteger(interp, pos(curbp, curbp->b_ebuf)); }

bool goto_line(int line)
{
    point_t p;

    if (line < 0)
        return 1;

    p = line_to_point(line);
    if (p == -1) {
        msg(m_lnot_found, line);
        return false;
    }
    curbp->b_point = p;
    msg(m_line, line);
    return true;
}
Object *e_goto_line(Object *interp, Object **args, Object **env, size_t nArgs)
{
    int line = FLISP_ARG1->value;

    if (line < 0)
        return newError(interp, invalid_value, FLISP_ARG1, "(goto-line line) - line must be positive");
    return goto_line(line) ? t : nil;
}

void down(void)
{
    curbp->b_point = lncolumn(curbp, dndn(curbp, curbp->b_point),curbp->b_col);
}
DEFINE_EDITOR_FUNC(down)

void up(void)
{
    curbp->b_point = lncolumn(curbp, upup(curbp, curbp->b_point),curbp->b_col);
}
DEFINE_EDITOR_FUNC(up)

void scroll_up(void)
{
    curbp->b_page = curbp->b_point = upup(curbp, curbp->b_epage);
    while (0 < curbp->b_row--)
        down();
    /* this stops a reframe in display(), and epage is recalculated during display() */
    curbp->b_epage = pos(curbp, curbp->b_ebuf);
}
DEFINE_EDITOR_FUNC(scroll_up)

void scroll_down(void)
{
    int i = curwp->w_rows;
    while (0 < --i) {
        curbp->b_page = upup(curbp, curbp->b_page);
        up();
    }
}
DEFINE_EDITOR_FUNC(scroll_down)

Object *e_search_forward(Object *interp, Object **args, Object **env, size_t nArgs)
{
    point_t founded = search_forward(FLISP_ARG1->string);
    move_to_search_result(founded);
    return (founded == -1 ? nil : t);
}

Object *e_search_backward(Object *interp, Object **args, Object **env, size_t nArgs)
{
    point_t founded = search_backwards(FLISP_ARG1->string);
    move_to_search_result(founded);
    return (founded == -1 ? nil : t);
}

void set_point(point_t p)
{
    if (p < 0 || p > pos(curbp, curbp->b_ebuf)) return;
    curbp->b_point = p;
}
Object *e_set_point(Object *interp, Object **args, Object **env, size_t nArgs)
{
    set_point(FLISP_ARG1->value);
    return t;
}

/* Buffer Management and information */
Object *e_find_buffer_by_fname(Object *interp, Object **args, Object **env, size_t nArgs)
{
    if (FLISP_ARG1->string[0] == '\0')
        return nil;

    buffer_t *bp = find_buffer_by_fname(FLISP_ARG1->string);

    return bp == NULL ? nil : newString(interp, bp->name);
}

/* Helper function: return either current buffer or named buffer if first argument exists */
Object *get_buffer_arg_one(Object *interp, Object **args, char *signature, buffer_t **bufferp)
{
    /* Note: when buffers are objects return the buffer, for now assume, caller defaults to curbp */
    if (FLISP_ARG1 == nil)
        return nil;
    if (FLISP_ARG1->type != type_string)
        return newErrorFmt(interp, wrong_type_argument, FLISP_ARG1,
                            "%s - expected %s, got: %s", signature,
                            type_string->type.name->string, FLISP_ARG1->type->type.name->string);
    buffer_t *buffer = find_buffer(FLISP_ARG1->string, false);
    if (buffer == NULL)
        return newError2(interp, invalid_value, FLISP_ARG1,
                        signature, " - buffer does not exist");
    *bufferp = buffer;
    return nil;
}

/* (buffer-filename[ buffer]) */
Object *e_get_buffer_filename(Object *interp, Object **args, Object **env, size_t nArgs)
{
    buffer_t *buffer = curbp;

    if (nArgs) {
        Object *result = get_buffer_arg_one(interp, args, "(buffer-filename[ buffer])", &buffer);
        if (result->type == type_error)
            return result;
    }
    if (buffer->fname == NULL)
        return nil;

    return newString(interp, buffer->fname);
}

/** (buffer-fread stream[ size]) - read size bytes from stream into current buffer at point, return bytes read
 *  If buffer cannot hold size more bytes, -1 is returned.
 *  If size is omitted or nil, read until eof.
 */
Object *e_buffer_fread(Object *interp, Object **args, Object **env, size_t nArgs)
{
    size_t len, size = 0;

    FLISP_ASSERT(FLISP_ARG1, type_stream, "(buffer-fread stream size) - stream");

    if (nArgs >1 && FLISP_ARG2 != nil) {
        FLISP_ASSERT(FLISP_ARG2, type_integer, "(buffer-fread stream size) - size");
        if (FLISP_ARG2->value == 0)
            return newInteger(interp, 0);

        if (FLISP_ARG2->value < 0)
            return newError(interp, invalid_value, FLISP_ARG2, "(buffer-read size stream) - size is negative");
        len = buffer_fread(curbp, FLISP_ARG1->stream.fd, FLISP_ARG2->value);
        if (ferror(FLISP_ARG1->stream.fd))
            return newError2(interp, io_error, FLISP_ARG1, "buffer_fread() failed: %s", strerror(errno));

        if (len == -1)
            return newError(interp, out_of_memory, nil, "buffer_fread() failed, could not grow current buffer");

        return newInteger(interp, len);
    }
    for (;;) {
        len = buffer_fread(curbp, FLISP_ARG1->stream.fd, BUFSIZ);

        if (ferror(FLISP_ARG1->stream.fd))
            return newError2(interp, io_error, FLISP_ARG1, "buffer_fread() failed: %s", strerror(errno));

        if (len == -1)
            return newError(interp, out_of_memory, nil, "buffer_fread() failed, could not grow current buffer");
        size += len;

        end_of_buffer();

        if (feof(FLISP_ARG1->stream.fd))
            return newInteger(interp, size);
    }
}

/** (buffer-fwrite stream size) - write size bytes from current buffer at point to stream, return bytes written */
Object *e_buffer_fwrite(Object *interp, Object **args, Object **env, size_t nArgs)
{
    size_t len;

    FLISP_ASSERT(FLISP_ARG1, type_stream, "(buffer-fwrite stream size) - stream");
    if (nArgs > 1) {
        FLISP_ASSERT(FLISP_ARG2, type_stream, "(buffer-fwrite stream size) - size");
        if (FLISP_ARG2->value == 0)
            return newInteger(interp, 0);
        if (FLISP_ARG2->value < 0)
            return newError(interp, invalid_value, FLISP_ARG2, "(buffer-fwrite stream size) - size is negative");
        len = FLISP_ARG2->value;
    } else {
        len = get_point_max() - get_point();
    }
    len = buffer_fwrite(curbp, FLISP_ARG1->stream.fd, len);
    if (ferror(FLISP_ARG1->stream.fd))
        return newError2(interp, io_error, FLISP_ARG1, "buffer_fwrite() failed: %s", strerror(errno));

    return newInteger(interp, len);
}

/* (buffer-mode[ buffer[ mode]]) => mode - gets or sets mode of buffer.
 * if buffer is not given or nil, use the current buffer
 */
Object *e_buffer_mode(Object *interp, Object **args, Object **env, size_t nArgs)
{
    buffer_t *buffer = curbp;
    if (nArgs) {
        Object *result = get_buffer_arg_one(interp, args, "(buffer-mode[ buffer[ mode]])", &buffer);
        if (result->type == type_error)  return result;
        if (nArgs > 1) {
            FLISP_ASSERT(FLISP_ARG2, type_symbol, "buffer-mode[ buffer[ mode]]) - mode");
            buffer->mode = FLISP_ARG2;
        }
    }
    return buffer->mode;
}

/* Buffer flags */
#define GET_SET_BUFFER_FLAG(FLAG)                                       \
    Object *e_buffer_##FLAG## _p(Object *interp, Object **args, Object **env, size_t nArgs) \
    {                                                                   \
        buffer_t *buffer = curbp;                                       \
        if (nArgs) {                                           \
            Object *result = get_buffer_arg_one(interp, args, "(buffer-" #FLAG "-p[ buffer[ p]])", &buffer); \
            if (result->type == type_error)  return result;             \
            if (nArgs > 1)                                      \
                buffer->FLAG = (FLISP_ARG2 != nil);                  \
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

Object *e_buffer_next(Object *interp, Object **args,Object **env, size_t nArgs)
{
    if (!(nArgs))
        return newString(interp, curbp->name);

    buffer_t *bp = find_buffer(FLISP_ARG1->string, false);

    if (!bp)
        return newError(interp, invalid_value, FLISP_ARG1, "(buffer-next buffer) - buffer does not exist");

    return newString(interp, bp->b_next->name);
}

Object *e_buffer_show(Object *interp, Object **args, Object **env, size_t nArgs)
{
    buffer_t *bp = find_buffer(FLISP_ARG1->string, true);
    if (!bp)
        return newError(interp, out_of_memory, FLISP_ARG1, "(generate-new-buffer name) failed, out of memory");
    switch_to_buffer(bp);
    return FLISP_ARG1;
}

Object *e_delete_buffer(Object *interp, Object **args, Object **env, size_t nArgs)
{
    buffer_t *buffer = find_buffer(FLISP_ARG1->string, false);
    if (buffer == NULL)
        return newError(interp, invalid_value, FLISP_ARG1, "(delete-buffer buffer) - buffer does not exist");
    if (!delete_buffer(buffer))
        return newError(interp, invalid_value, FLISP_ARG1, "(delete-buffer buffer) - refused to delete scratch or current buffer");
    return FLISP_ARG1;
}

/** (get-buffer-create name) */
Object *e_get_buffer_create(Object *interp, Object **args, Object **env, size_t nArgs)
{
    if (find_buffer(FLISP_ARG1->string, true))
        return FLISP_ARG1;
    return newError(interp, out_of_memory, nil, "(get-buffer-create name) failed, out of memory");
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
    switch_to_buffer(list_bp); /* we are leaving the old buffer for a new one */
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
DEFINE_EDITOR_FUNC(list_buffers)

Object *e_set_buffer(Object *interp, Object **args, Object **env, size_t nArgs)
{
    buffer_t *bp = find_buffer(FLISP_ARG1->string, false);

    if (!bp)
        return newError(interp, invalid_value, FLISP_ARG1, "(set-buffer buffer) - buffer does not exist");

    curbp = bp;
    return FLISP_ARG1;
}

Object *e_set_buffer_name(Object *interp, Object **args, Object **env, size_t nArgs)
{
    buffer_t *buffer = find_buffer(FLISP_ARG1->string, false);

    if (buffer != NULL)
        return newError(interp, invalid_value, FLISP_ARG1, "(set-buffer-name name) - name, already exists");

    if (!set_buffer_name(curbp, FLISP_ARG1->string))
        return newError(interp, out_of_memory, FLISP_ARG1, "(set-buffer-name name) - name, failed to allocate string");
    return FLISP_ARG1;
}

/** (set-visited-file-name name) */
Object *e_set_buffer_filename(Object *interp, Object **args, Object **env, size_t nArgs)
{
    if (FLISP_ARG1 == nil) {
        if (curbp->fname != NULL)
            free(curbp->fname);
        curbp->fname = NULL;
        return nil;
    }

    FLISP_ASSERT(FLISP_ARG1, type_string, "(set-visited-file-name name) - name");
    curbp->fname = strdup(FLISP_ARG1->string);
    if (curbp->fname == NULL)
        return newError(interp, out_of_memory, nil,  "(set-visited-file-name name) - name, cannot allocate memory for filename");
    curbp->modified = TRUE;
    return FLISP_ARG1;
}

/* Windows Handling */

DEFINE_EDITOR_FUNC(delete_other_windows)

DEFINE_EDITOR_FUNC(other_window)

/* (pop-to-buffer buffer) */
Object *e_pop_to_buffer(Object *interp, Object **args, Object **env, size_t nArgs)
{
    window_t *wp = popup_window(FLISP_ARG1->string);
    if (wp == NULL)
        return newError(interp, invalid_value, FLISP_ARG1, "(pop-to-buffer buffer) - buffer does not exist");
    /* See other_window() */
    curwp->w_update = true;
    curwp = wp;
    pull_buffer(wp->w_bufp);
    /* Note: bug: first time the cursor does not jump to new window */
    update_display();
    return newString(interp, wp->w_bufp->name);
}

Object *e_split_window(Object *interp, Object **args, Object **env, size_t nArgs) { return (NULL == split_current_window()) ? nil : t; }

DEFINE_EDITOR_FUNC(update_display)

Object *e_refresh(Object *interp, Object ** args, Object **env, size_t nArgs)
{
    refresh();
    return t;
}

/* Message Line */
DEFINE_EDITOR_FUNC(clear_message_line)

Object *e_message(Object *interp, Object **args, Object **env, size_t nArgs)
{
    msg(FLISP_ARG1->string);
    return t;
}

/** (prompt-filename prompt[ default]) */

Object *e_prompt(Object *interp, Object **args, Object **env, size_t nArgs)
{
    char response[81] = "";

    if (nArgs > 1) {
        size_t len = strlen(FLISP_ARG2->string);
        if (len > 80)
            len = 80;
        strncpy(response, FLISP_ARG2->string, len);
        response[len] = '\0';
    }
    if (getinput(FLISP_ARG1->string, response, 80))
        return newStringWithLength(interp, response, strlen(response));
    return nil;
}

Object *e_prompt_filename(Object *interp, Object **args, Object **env, size_t nArgs)
{

    if (nArgs > 1)
        strcpy(response_buf, FLISP_ARG2->string);
    else
        response_buf[0] = '\0';

    char *prompt = strdup(FLISP_ARG1->string);
    if (!getfilename(prompt, (char*) response_buf, PATH_MAX)) {
        free(prompt);
        return nil;
    }
    free(prompt);
    return newString(interp, response_buf);
}


/* Keyboard Handling */

DEFINE_EDITOR_FUNC(describe_bindings)
DEFINE_EDITOR_FUNC(describe_functions)
DEFINE_EDITOR_FUNC(execute_key)

Object *e_getch(Object *interp, Object **args, Object **env, size_t nArgs)
{
    char ch[2];
    ch[0] = (unsigned char)getch();
    ch[1] = '\0';
    return newStringWithLength(interp, ch, 1);
}

Object *e_get_key(Object *interp, Object **args, Object **env, size_t nArgs) { return newString(interp, get_input_key()); }

Object *e_get_key_funcname(Object *interp, Object **args, Object **env, size_t nArgs) { return newString(interp, get_key_funcname()); }

Object *e_get_key_name(Object *interp, Object **args, Object **env, size_t nArgs) { return newString(interp, get_key_name()); }

/* Note: set_key always returns 1, so we don't need to decide here either */
Object *e_set_key(Object *interp, Object **args, Object **env, size_t nArgs) { return (1 == set_key(FLISP_ARG1->string, FLISP_ARG2->string) ? t : nil); }


/* Programming and System Interaction */
void quit(void)
{
    done = 1;
}
DEFINE_EDITOR_FUNC(quit)

/* Note: required? current usage might be replacable by popen() */
Object *e_get_temp_file(Object *interp, Object **args, Object **env, size_t nArgs)
{
    static char temp_file[] = TEMPFILE;

    /* Note: this might be superflouos*/
//    strcpy(temp_file, TEMPFILE);

    if (mkstemp(temp_file) == -1)
        return newError(interp, io_error, nil, "Failed to create temp file");

    return newStringWithLength(interp, temp_file, sizeof(TEMPFILE));
}

Object *e_get_version_string(Object *interp, Object **args, Object **env, size_t nArgs)
{
    return newStringWithLength(interp, m_version, strlen(m_version));
}

void suspend(void)
{
    raise(SIGTSTP);
}
DEFINE_EDITOR_FUNC(suspend)


/* Other utilities */

/* standard insert at the keyboard */
/* Note: used in femto.c main loop and undo.c */
void insert(void)
{
    char_t the_char[2]; /* the inserted char plus a null */
    assert(curbp->b_gap <= curbp->b_egap);

    if (curbp->b_gap == curbp->b_egap && !growgap(curbp, CHUNK))
        return;
    curbp->b_point = movegap(curbp, curbp->b_point);


    /* overwrite if mid line, not EOL or EOF, CR will insert as normal */
    if (curbp->overwrite && *input != '\r' && *(ptr(curbp, curbp->b_point)) != '\n' && curbp->b_point < pos(curbp,curbp->b_ebuf) ) {
        *(ptr(curbp, curbp->b_point)) = *input;
        if (curbp->b_point < pos(curbp, curbp->b_ebuf))
            ++curbp->b_point;
        /* FIXME - overwite mode not handled properly for undo yet */
    } else {
        the_char[0] = *input == '\r' ? '\n' : *input;
        the_char[1] = '\0'; /* null terminate */
        *curbp->b_gap++ = the_char[0];
        curbp->b_point = pos(curbp, curbp->b_egap);
        /* the point is set so that and undo will backspace over the char */
        add_undo(curbp, UNDO_T_INSERT, curbp->b_point, the_char, NULL);
    }
    curbp->modified = TRUE;
}

void insert_string(char *str)
{
    int len = (str == NULL) ? 0 : strlen(str);

    if (curbp->overwrite)
        return;
    if (len <= 0) {
        msg(m_empty);
    } else if (len < curbp->b_egap - curbp->b_gap || growgap(curbp, len)) {
        curbp->b_point = movegap(curbp, curbp->b_point);
        //debug("INS STR: pt=%ld len=%d\n", curbp->b_point, strlen((char *)str));
        add_undo(curbp, UNDO_T_YANK, curbp->b_point, (char_t *)str, NULL);
        memcpy(curbp->b_gap, str, len * sizeof (char_t));
        curbp->b_gap += len;
        curbp->b_point = pos(curbp, curbp->b_egap);
        curbp->modified = TRUE;
    }
}

/*
 * append a string to the end of a buffer
 * used in funcmap.c
 */
void append_string(buffer_t *bp, char *str)
{
    int len = (str == NULL) ? 0 : strlen(str);

    assert(bp != NULL);
    if (len == 0) return;

    /* goto end of buffer */
    bp->b_epage = bp->b_point = pos(bp, bp->b_ebuf);

    if (len < bp->b_egap - bp->b_gap || growgap(bp, len)) {
        bp->b_point = movegap(bp, bp->b_point);
        memcpy(bp->b_gap, str, len * sizeof (char_t));
        bp->b_gap += len;
        bp->b_point = pos(bp, bp->b_egap);
        curbp->modified = TRUE;
        bp->b_epage = bp->b_point = pos(bp, bp->b_ebuf); /* goto end of buffer */

        /* if window is displayed mark all windows for update */
        if (bp->b_cnt > 0) {
            b2w_all_windows(bp);
            mark_all_windows();
        }
    }
}

/* Used in femto.c */
void match_paren_forwards(buffer_t *bp, char open_paren, char close_paren)
{
    int lcount = 0;
    int rcount = 0;
    point_t end = pos(bp, bp->b_ebuf);
    point_t position = bp->b_point;
    char c;

    while (position <= end) {
        c = *ptr(bp, position);
        if (c == open_paren)
            lcount++;
        if (c == close_paren)
            rcount++;
        if (lcount == rcount && lcount > 0) {
            bp->b_paren = position;
            return;
        }
        position++;
    }
    bp->b_paren = NOPAREN;
}

void match_paren_backwards(buffer_t *bp, char open_paren, char close_paren)
{
    int lcount = 0;
    int rcount = 0;
    point_t start = 0;
    point_t position = bp->b_point;
    char c;

    while (position >= start) {
        c = *ptr(bp, position);
        if (c == open_paren)
            lcount++;
        if (c == close_paren)
            rcount++;
        if (lcount == rcount && lcount > 0) {
            bp->b_paren = position;
            return;
        }
        position--;
    }
    bp->b_paren = NOPAREN;
}

void match_parens(void)
{
    assert(curwp != NULL);
    buffer_t *bp = curwp->w_bufp;
    assert(bp != NULL);

    if (buffer_is_empty(bp))
        return;

    // Note: valgrind: Invalid read of size 1
    char p = *ptr(bp, bp->b_point);

    switch(p) {
    case '{':
        match_paren_forwards(bp, '{', '}');
        break;
    case '[':
        match_paren_forwards(bp, '[', ']');
        break;
    case '(':
        match_paren_forwards(bp, '(', ')');
        break;
    case '}':
        match_paren_backwards(bp, '{', '}');
        break;
    case ']':
        match_paren_backwards(bp, '[', ']');
        break;
    case ')':
        match_paren_backwards(bp, '(', ')');
        break;
    default:
        bp->b_paren = NOPAREN;
        break;
    }
}

/* this is called for every user key setup by a call to set_key */
/* Used in key.c and funcmap.c */
void user_func(void)
{
    assert(key_return != NULL);
    if (0 == strcmp(key_return->k_funcname, E_NOT_BOUND)) {
        msg(E_NOT_BOUND);
        return;
    }
    debug("user_func: (%s)\n", key_return->k_funcname);
    eval_string("(%s)", key_return->k_funcname);
}

FLISP_DEFINE_CONSTANT(femto_libs,femto_lib);

Object *femto_flisp_init(Object *interp, Object *extension)
{
    if (extension->extension.version != nil) return extension->extension.version;

    char *library_path;
    Object *e = nil;

    if ((library_path=getenv("FEMTOLIB")) == NULL)
        library_path = CPP_XSTR(E_SCRIPTDIR);
    GC_CHECKPOINT;
    GC_TRACE(gcExt, extension);

    do {
        FLISP_UNLESS_ERR(femto_buffer_register(interp));
        debug("femto buffer module registered\n");
        
        FLISP_UNLESS_ERR(flisp_register_constant(interp, femto_libs, newString(interp, library_path)));


/* Text manipulation: read from, write to buffer text */
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "backspace",             0, 0, (TypeObject*)nil,         e_backspace));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "delete",                0, 0, (TypeObject*)nil,         e_delete));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "erase-buffer",          0, 0, (TypeObject*)nil,         e_zero_buffer));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "get-char",              0, 0, (TypeObject*)nil,         e_get_char));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "insert-string",         1, 1, type_string, e_insert_string));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "kill-region",           0, 0, (TypeObject*)nil,         e_kill_region));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "yank",                  0, 0, (TypeObject*)nil,         e_yank));

/* Selection */
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "copy-region",           0, 0, (TypeObject*)nil,         e_copy_region));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "get-clipboard",         0, 0, (TypeObject*)nil,         e_get_clipboard));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "get-mark",              0, 0, (TypeObject*)nil,         e_get_mark));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "set-clipboard",         1, 1, type_string, e_set_clipboard));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "set-mark",              0, 0, (TypeObject*)nil,         e_set_mark));

/* Cursor Movement and information */
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "backward-char",         0, 0, (TypeObject*)nil,         e_left));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "backward-word",         0, 0, (TypeObject*)nil,         e_backward_word));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "beginning-of-buffer",   0, 0, (TypeObject*)nil,         e_beginning_of_buffer));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "beginning-of-line",     0, 0, (TypeObject*)nil,         e_lnbegin));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "end-of-buffer",         0, 0, (TypeObject*)nil,         e_end_of_buffer));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "end-of-line",           0, 0, (TypeObject*)nil,         e_lnend));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "forward-char",          0, 0, (TypeObject*)nil,         e_right));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "forward-word",          0, 0, (TypeObject*)nil,         e_forward_word));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "get-point",             0, 0, (TypeObject*)nil,         e_get_point));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "get-point-max",         0, 0, (TypeObject*)nil,         e_get_point_max));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "goto-line",             1, 1, type_integer, e_goto_line));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "next-line",             0, 0, (TypeObject*)nil,         e_down));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "previous-line",         0, 0, (TypeObject*)nil,         e_up));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "scroll-up",             0, 0, (TypeObject*)nil,         e_scroll_up));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "scroll-down",           0, 0, (TypeObject*)nil,         e_scroll_down));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "search-forward",        1, 1, type_string, e_search_forward));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "search-backward",       1, 1, type_string, e_search_backward));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "set-point",             1, 1, type_integer, e_set_point));

/* Buffer Management and information */
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "find-buffer-visiting",  1, 1, type_string, e_find_buffer_by_fname));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "buffer-filename",       0, 1, type_string, e_get_buffer_filename));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "buffer-fread",          1, 2, (TypeObject*)nil,         e_buffer_fread));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "buffer-fwrite",         1, 2, (TypeObject*)nil,         e_buffer_fwrite));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "buffer-mode",           0, 2, (TypeObject*)nil,         e_buffer_mode));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "buffer-modified-p",     0, 2, (TypeObject*)nil,         e_buffer_modified_p));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "buffer-overwrite-p",    0, 2, (TypeObject*)nil,         e_buffer_overwrite_p));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "buffer-readonly-p",     0, 2, (TypeObject*)nil,         e_buffer_readonly_p));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "buffer-special-p",      0, 2, (TypeObject*)nil,         e_buffer_special_p));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "buffer-undo-p",         0, 2, (TypeObject*)nil,         e_buffer_undo_p));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "buffer-next",           0, 1, type_string, e_buffer_next));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "buffer-show",           1, 1, type_string, e_buffer_show));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "delete-buffer",         1, 1, type_string, e_delete_buffer));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "get-buffer-create",     1, 1, type_string, e_get_buffer_create));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "list-buffers",          0, 0, (TypeObject*)nil,         e_list_buffers));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "set-buffer",            1, 1, type_string, e_set_buffer));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "set-buffer-name",       1, 1, type_string, e_set_buffer_name));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "set-visited-file-name",  1, 1, (TypeObject*)nil,        e_set_buffer_filename));

/* Window Handling */
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "delete-other-windows",  0, 0, (TypeObject*)nil,         e_delete_other_windows));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "split-window",          0, 0, (TypeObject*)nil,         e_split_window));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "other-window",          0, 0, (TypeObject*)nil,         e_other_window));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "pop-to-buffer",         1, 1, type_string, e_pop_to_buffer));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "update-display",        0, 0, (TypeObject*)nil,         e_update_display));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "refresh",               0, 0, (TypeObject*)nil,         e_refresh));

/* Message Line */
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "clear-message-line",    0, 0, (TypeObject*)nil,         e_clear_message_line));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "message",               1, 1, type_string, e_message));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "prompt",                1, 2, type_string, e_prompt));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "prompt-filename",       1, 2, type_string, e_prompt_filename));

/* Keyboard Handling */
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "describe-bindings",     0, 0, (TypeObject*)nil,         e_describe_bindings));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "describe-functions",    0, 0, (TypeObject*)nil,         e_describe_functions));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "execute-key",           0, 0, (TypeObject*)nil,         e_execute_key));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "getch",                 0, 0, (TypeObject*)nil,         e_getch));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "get-key",               0, 0, (TypeObject*)nil,         e_get_key));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "get-key-funcname",      0, 0, (TypeObject*)nil,         e_get_key_funcname));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "get-key-name",          0, 0, (TypeObject*)nil,         e_get_key_name));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "set-key",               2, 2, type_string, e_set_key));

/* Programming and System Interaction */
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "exit",                  0, 0, (TypeObject*)nil,         e_quit));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "get-temp-file",         0, 0, (TypeObject*)nil,         e_get_temp_file));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "get-version-string",    0, 0, (TypeObject*)nil,         e_get_version_string));
        FLISP_UNLESS_ERR(flisp_register_primitive(interp, "suspend",               0, 0, (TypeObject*)nil,         e_suspend));

        FLISP_UNLESS_ERR((*gcExt)->extension.version = newString(interp, E_VERSION));

    } while (0);
    GC_RELEASE;
    return e;
}

/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
