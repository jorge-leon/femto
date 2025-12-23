/*
 * command.c, femto, Hugh Barney, Public Domain, 2017
 * Derived from: Anthony's Editor January 93, (Public Domain 1991, 1993 by Anthony Howe)
 */

#include <stdlib.h>
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


void beginning_of_buffer(void)
{
    curbp->b_point = 0;
}

void end_of_buffer(void)
{
    curbp->b_point = pos(curbp, curbp->b_ebuf);
    if (curbp->b_epage < pos(curbp, curbp->b_ebuf)) curbp->b_reframe = 1;
}

void quit(void)
{
    done = 1;
}

void suspend(void)
{
    raise(SIGTSTP);
}

void redraw(void)
{
    clear();
    mark_all_windows();
    update_display();
}

void left(void)
{
    int n = prev_utf8_char_size();

    while (0 < curbp->b_point && n-- > 0)
        --curbp->b_point;
}

void right(void)
{
    int n = utf8_size(*ptr(curbp,curbp->b_point));

    while ((curbp->b_point < pos(curbp, curbp->b_ebuf)) && n-- > 0)
        ++curbp->b_point;
}


/*
 * work out number of bytes based on first byte
 *
 * 1 byte utf8 starts 0xxxxxxx  00 - 7F : 000 - 127
 * 2 byte utf8 starts 110xxxxx  C0 - DF : 192 - 223
 * 3 byte utf8 starts 1110xxxx  E0 - EF : 224 - 239
 * 4 byte utf8 starts 11110xxx  F0 - F7 : 240 - 247
 *
 */
int utf8_size(char_t c)
{
    if (c >= 192 && c < 224) return 2;
    if (c >= 224 && c < 240) return 3;
    if (c >= 240 && c < 248) return 4;
    return 1; /* if in doubt it is 1 */
}

int prev_utf8_char_size(void)
{
    int n;
    for (n=2;n<5;n++)
        if (-1 < curbp->b_point - n && (utf8_size(*(ptr(curbp, curbp->b_point - n))) == n))
            return n;
    return 1;
}

void up(void)
{
    curbp->b_point = lncolumn(curbp, upup(curbp, curbp->b_point),curbp->b_col);
}

void down(void)
{
    curbp->b_point = lncolumn(curbp, dndn(curbp, curbp->b_point),curbp->b_col);
}

void lnbegin(void)
{
    curbp->b_point = segstart(curbp, lnstart(curbp,curbp->b_point), curbp->b_point);
}

void lnend(void)
{
        if (curbp->b_point == pos(curbp, curbp->b_ebuf)) return; /* do nothing if EOF */
    curbp->b_point = dndn(curbp, curbp->b_point);
    point_t p = curbp->b_point;
    left();
    curbp->b_point = (*ptr(curbp, curbp->b_point) == '\n') ? curbp->b_point : p;
}

void backward_word(void)
{
    char_t *p;
    while (!isspace(*(p = ptr(curbp, curbp->b_point))) && curbp->b_buf < p)
        --curbp->b_point;
    while (isspace(*(p = ptr(curbp, curbp->b_point))) && curbp->b_buf < p)
        --curbp->b_point;
}

void scroll_up(void)
{
    curbp->b_page = curbp->b_point = upup(curbp, curbp->b_epage);
    while (0 < curbp->b_row--)
        down();
    /* this stops a reframe in display(), and epage is recalculated during display() */
    curbp->b_epage = pos(curbp, curbp->b_ebuf);
}

void scroll_down(void)
{
    int i = curwp->w_rows;
    while (0 < --i) {
        curbp->b_page = upup(curbp, curbp->b_page);
        up();
    }
}

void forward_word(void)
{
    char_t *p;
    while (!isspace(*(p = ptr(curbp, curbp->b_point))) && p < curbp->b_ebuf)
        ++curbp->b_point;
    while (isspace(*(p = ptr(curbp, curbp->b_point))) && p < curbp->b_ebuf)
        ++curbp->b_point;
}

/* standard insert at the keyboard */
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

/*
 * A special insert used as the undo of delete char (C-d or DEL)
 * this is where the char is inserted at the point and the cursor
 * is NOT moved on 1 char.  This MUST be a seperate function so that
 *   INSERT + BACKSPACE are matching undo pairs
 *   INSERT_AT + DELETE are matching undo pairs
 * Note: This function is only ever called by execute_undo to undo a DEL.
 */
void insert_at(void)
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
        curbp->b_point--; /* move point back to where it was before, should always be safe */
        /* the point is set so that and undo will DELETE the char */
        add_undo(curbp, UNDO_T_INSAT, curbp->b_point, the_char, NULL);
    }
    curbp->modified = TRUE;
}

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

void i_gotoline(void)
{
    int line;

    response_buf[0] = '\0';
    if (getinput(m_goto, (char*)response_buf, STRBUF_S)) {
        line = atoi(response_buf);
        goto_line(line);
    }
}

int goto_line(int line)
{
    point_t p;

    p = line_to_point(line);
    if (p != -1) {
        curbp->b_point = p;
        msg(m_line, line);
        return 1;
    } else {
        if (line > 0)
            msg(m_lnot_found, line);
        return 0;
    }
}

void i_set_mark(void)
{
    set_mark();
    msg(str_mark);
}

void set_mark(void)
{
    curbp->b_mark = (curbp->b_mark == curbp->b_point ? NOMARK : curbp->b_point);
}

void unmark(void)
{
    assert(curbp != NULL);
    curbp->b_mark = NOMARK;
}

void toggle_overwrite_mode(void) { curbp->overwrite = !curbp->overwrite; }

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

void copy_region(void) {
    if (i_check_region() == FALSE) return;
    copy_cut(FALSE);
}

void kill_region(void) {
    if (i_check_region() == FALSE) return;
    copy_cut(TRUE);
}

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

/* safe interface to clipboard so we dont pass a NULL pointer to lisp */
char *get_clipboard(void)
{
    static char empty_string[] = "";

    if (scrap == NULL) return empty_string;
    return (char *)scrap;
}

unsigned char *get_scrap(void)
{
    return scrap;
}

/*
 * set the scrap pointer, a setter for external interface code
 * ptr must be a pointer to a malloc'd NULL terminated string
 */
void set_scrap(unsigned char *ptr)
{
    if (scrap != NULL) free(scrap);
    assert(ptr != NULL);
    scrap = ptr;
}

void yank(void)
{
    insert_string((char *)scrap);
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

void log_message(char *str)
{
    buffer_t *bp = find_buffer("*messages*", true);
    assert(bp != NULL);
    append_string(bp, str);
}

void cursor_position(void)
{
    int current, lastln;
    point_t end_p = pos(curbp, curbp->b_ebuf);

    get_line_stats(&current, &lastln);

    if (curbp->b_point == end_p) {
        msg(str_endpos, current, lastln,
            curbp->b_point, ((curbp->b_ebuf - curbp->b_buf) - (curbp->b_egap - curbp->b_gap)));
    } else {
        msg(str_pos, unctrl(*(ptr(curbp, curbp->b_point))), *(ptr(curbp, curbp->b_point)),
            current, lastln,
            curbp->b_point, ((curbp->b_ebuf - curbp->b_buf) - (curbp->b_egap - curbp->b_gap)));
    }
}

char* get_temp_file(void)
{
    int result = 0;
    static char temp_file[] = TEMPFILE;

    strcpy(temp_file, TEMPFILE);
    result = mkstemp(temp_file);

    if (result == -1)
    {
        printf("Failed to create temp file\n");
        exit(1);
    }

    return temp_file;
}

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

void version(void)
{
    msg(m_version);
}

char *get_version_string(void)
{
    return m_version;
}

void resize_terminal(void)
{
    one_window(curwp);
}

/* return char at current point */
char *get_char(void)
{
    static char ch[2] = "\0";
    ch[0] = (char)*(ptr(curbp, curbp->b_point));
    return ch;
}

void set_point(point_t p)
{
    if (p < 0 || p > pos(curbp, curbp->b_ebuf)) return;
    curbp->b_point = p;
}

/* return mark in current buffer */
point_t get_mark(void)
{
    return curbp->b_mark;
}

/* return point in current buffer */
point_t get_point(void)
{
    return curbp->b_point;
}

/* return point in current buffer */
point_t get_point_max(void)
{
    return pos(curbp, curbp->b_ebuf);
}

/*
 * execute a lisp command typed in at the command prompt >
 * send any output to the message line.  This avoids text
 * being sent to the current buffer which means the file
 * contents could get corrupted if you are running commands
 * on the buffers etc.
 *
 * If the output is too big for the message line then send it to
 * a temp buffer called *lisp_output* and popup the window
 *
 */
void repl(void)
{
    buffer_t *bp;
    char *output;

    response_buf[0] = '\0';
    if (!getinput("> ", response_buf, TEMPBUF))
        return;

    if ((output = eval_string(false, response_buf)) == NULL)
        return;

    // Note: Emacs puts errors and output always into the *Messages*
    //       buffer, plus errors are shown in the message line.
    //       Decision about whether to insert the result into the
    //       buffer or show it in the message line is done based on
    //       key pressed/function invoked

    if (strlen(output) < 60) {
        msg(output);
    } else {
        bp = find_buffer("*lisp_output*", true);
        append_string(bp, output);
        (void)popup_window(bp->name);
    }
    free_lisp_output();
}

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

/* this is called for every user key setup by a call to set_key */
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

/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
