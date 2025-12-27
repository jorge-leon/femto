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

Object *e_zero_buffer(Interpreter *interp, Object **args, Object **env)
{
    assert(curbp != NULL);
    zero_buffer(curbp);
    return nil;
}

Object *e_get_char(Interpreter *interp, Object **args, Object **env)
{
    static char ch[2] = "\0";
    ch[0] = (char)*(ptr(curbp, curbp->b_point));
    return newStringWithLength(interp, ch, 1);
}

Object *e_insert_string(Interpreter *interp, Object **args, Object **env)
{
    insert_string(FLISP_ARG_ONE->string);
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
Object *e_get_clipboard(Interpreter *interp, Object **args, Object **env)
{
    if (scrap == NULL)
        return lisp_empty_string;
    return newString(interp, (char *)scrap);
}

Object *e_get_mark(Interpreter *interp, Object **args, Object **env) { return newInteger(interp, curbp->b_mark); }

Object *e_set_clipboard(Interpreter *interp, Object **args, Object **env)
{
    if (scrap != NULL)  free(scrap);
    scrap = (char_t *) strdup(FLISP_ARG_ONE->string);
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
Object *e_get_point(Interpreter *interp, Object **args, Object **env) { return newInteger(interp, curbp->b_point); }

/* return point in current buffer */
point_t get_point_max(void) { return pos(curbp, curbp->b_ebuf); }
Object *e_get_point_max(Interpreter *interp, Object **args, Object **env) { return newInteger(interp, pos(curbp, curbp->b_ebuf)); }

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
Object *e_goto_line(Interpreter *interp, Object **args, Object **env)
{
    int line = FLISP_ARG_ONE->integer;

    if (line < 0)
        exceptionWithObject(interp, FLISP_ARG_ONE, invalid_value, "(goto-line line) - line must be positive");
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

void set_point(point_t p)
{
    if (p < 0 || p > pos(curbp, curbp->b_ebuf)) return;
    curbp->b_point = p;
}
Object *e_set_point(Interpreter *interp, Object **args, Object **env)
{
    set_point(FLISP_ARG_ONE->integer);
    return t;
}

/* Buffer Management and information */
Object *e_find_buffer_by_fname(Interpreter *interp, Object **args, Object **env)
{
    if (FLISP_ARG_ONE->string[0] == '\0')
        return nil;

    buffer_t *bp = find_buffer_by_fname(FLISP_ARG_ONE->string);

    return bp == NULL ? nil : newString(interp, bp->name);
}

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

Object *e_delete_buffer(Interpreter *interp, Object **args, Object **env)
{
    buffer_t *buffer = find_buffer(FLISP_ARG_ONE->string, false);
    if (buffer == NULL)
        exceptionWithObject(interp, FLISP_ARG_ONE, invalid_value, "(delete-buffer buffer) - buffer does not exist");
    if (!delete_buffer(buffer))
        exceptionWithObject(interp, FLISP_ARG_ONE, invalid_value, "(delete-buffer buffer) - refused to delete scratch or current buffer");
    return FLISP_ARG_ONE;
}

/** (get-buffer-create name) */
Object *e_get_buffer_create(Interpreter *interp, Object **args, Object **env)
{
    if (find_buffer(FLISP_ARG_ONE->string, true))
        return FLISP_ARG_ONE;
    exceptionWithObject(interp, FLISP_ARG_ONE, out_of_memory, "(get-buffer-create name) failed, out of memory");
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
DEFINE_EDITOR_FUNC(list_buffers)

Object *e_set_buffer(Interpreter *interp, Object **args, Object **env)
{
    buffer_t *bp = find_buffer(FLISP_ARG_ONE->string, false);

    if (!bp)
        exceptionWithObject(interp, FLISP_ARG_ONE, invalid_value, "(set-buffer buffer) - buffer does not exist");

    curbp = bp;
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

/* Windows Handling */

DEFINE_EDITOR_FUNC(delete_other_windows)

Object *e_split_window(Interpreter *interp, Object **args, Object **env) { return (NULL == split_current_window()) ? nil : t; }

DEFINE_EDITOR_FUNC(other_window)

DEFINE_EDITOR_FUNC(update_display)

Object *e_refresh(Interpreter *interp, Object ** args, Object **env)
{
    refresh();
    return t;
}

/* Message Line */
Object *e_message(Interpreter *interp, Object **args, Object **env)
{
    msg(FLISP_ARG_ONE->string);
    return t;
}

/** (prompt-filename prompt[ default]) */

DEFINE_EDITOR_FUNC(clear_message_line)

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


/* Keyboard Handling */

DEFINE_EDITOR_FUNC(describe_bindings)
DEFINE_EDITOR_FUNC(describe_functions)
DEFINE_EDITOR_FUNC(execute_key)

Object *e_getch(Interpreter *interp, Object **args, Object **env)
{
    char ch[2];
    ch[0] = (unsigned char)getch();
    ch[1] = '\0';
    return newStringWithLength(interp, ch, 1);
}

Object *e_get_key(Interpreter *interp, Object **args, Object **env) { return newString(interp, get_input_key()); }

Object *e_get_key_funcname(Interpreter *interp, Object **args, Object **env) { return newString(interp, get_key_funcname()); }

Object *e_get_key_name(Interpreter *interp, Object **args, Object **env) { return newString(interp, get_key_name()); }

Object *e_set_key(Interpreter *interp, Object **args, Object **env) { return (1 == set_key(FLISP_ARG_ONE->string, FLISP_ARG_TWO->string) ? t : nil); }


/* Programming and System Interaction */

/*
 * evaluate a block between mark and point
 */
void eval_block(void)
{
    char *output;

    if (curbp->b_mark == NOMARK || curbp->b_mark >= curbp->b_point) {
        msg("no block defined");
        return;
    }

    copy_region();
    assert(scrap != NULL);
    assert(strlen((char *)scrap) > 0);

    insert_string("\n");

    if ((output = eval_string(false, (char *)scrap)) == NULL)
        return;
    // Note: femto used to insert error messages in the current buffer. Now we don't anymore.
    insert_string(output);
    free_lisp_output();
}
DEFINE_EDITOR_FUNC(eval_block)
void quit(void)
{
    done = 1;
}
DEFINE_EDITOR_FUNC(quit)

/* Note: required? current usage might be replacable by popen() */
Object *e_get_temp_file(Interpreter *interp, Object **args, Object **env)
{
    static char temp_file[] = TEMPFILE;

    /* Note: this might be superflouos*/
//    strcpy(temp_file, TEMPFILE);

    if (mkstemp(temp_file) == -1)
        exception(interp, io_error, "Failed to create temp file");

    return newStringWithLength(interp, temp_file, sizeof(TEMPFILE));
}

Object *e_get_version_string(Interpreter *interp, Object **args, Object **env)
{
    return newStringWithLength(interp, m_version, strlen(m_version));
}

Object *e_log_debug(Interpreter *interp, Object **args, Object **env)
{
    fl_debug(interp, "%s", FLISP_ARG_ONE->string);
    return t;
}

void log_message(char *str)
{
    buffer_t *bp = find_buffer("*messages*", true);
    assert(bp != NULL);
    append_string(bp, str);
}
Object *e_log_message(Interpreter *interp, Object **args, Object **env)
{
    log_message(FLISP_ARG_ONE->string);
    return t;
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

    if (eval_string(true, "(%s)", key_return->k_funcname) == NULL)
        return;
    free_lisp_output();
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
    {"clear-message-line",        0, 0, 0,            e_clear_message_line},
    {"message",                   1, 1, TYPE_STRING,  e_message},
    {"prompt",                    1, 2, TYPE_STRING,  e_prompt},
    {"prompt-filename",           1, 2, TYPE_STRING,  e_prompt_filename},

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
    {"eval-block",                0, 0, 0,            e_eval_block},
    {"exit",                      0, 0, 0,            e_quit},
    {"get-temp-file",             0, 0, 0,            e_get_temp_file},
    {"get-version-string",        0, 0, 0,            e_get_version_string},
    {"log-debug",                 1, 1, TYPE_STRING,  e_log_debug},
    {"log-message",               1, 1, TYPE_STRING,  e_log_message},
    {"suspend",                   0, 0, 0,            e_suspend}
};

Object *femto_libs = &(Object) { .string = "femto_lib" };

void femto_register(Interpreter *interp)
{
    int i;
    char *library_path;
    Object *femto_script_dir;

    femto_buffer_register(interp);
    debug("femto buffer module registered\n");

    for (i = 0; i < sizeof(femto_primitives) / sizeof(femto_primitives[0]); i++)
        lisp_register_primitive(interp, &femto_primitives[i]);
    debug("femto primitives registered\n");

    if ((library_path=getenv("FEMTOLIB")) == NULL)
        library_path = CPP_XSTR(E_SCRIPTDIR);
    femto_script_dir= newString(interp, library_path);
    lisp_register_constant(interp, femto_libs, femto_script_dir);

}

/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
