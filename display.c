/*
 * display.c, femto, Hugh Barney, Public Domain, 2017
 * Derived from: Anthony's Editor January 93, (Public Domain 1991, 1993 by Anthony Howe)
 */


#define _XOPEN_SOURCE 500
#include <wchar.h>

#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#include <curses.h>

#include "femto.h"
#include "buffer.h"
#include "window.h"
#include "undo.h"
#include "gap.h"
#include "key.h"
#include "display.h"
#include "hilite.h"
#include "command.h"

/* Reverse scan for start of logical line containing offset */
point_t lnstart(BufferObject *bp, register point_t off)
{
    register char_t *p;
    do
        p = ptr(bp, --off);
    while (bp->buffer.buf < p && *p != '\n');
    return (bp->buffer.buf < p ? ++off : 0);
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


/*
 * Forward scan for start of logical line segment containing 'finish'.
 * A segment of a logical line corresponds to a physical screen line.
 */
point_t segstart(BufferObject *bp, point_t start, point_t finish)
{
    char_t *p;
    int c = 0;
    point_t scan = start;

    while (scan < finish) {
        p = ptr(bp, scan);
        if (*p == '\n') {
            c = 0;
            start = scan + 1;
        } else if (COLS <= c) {
            c = 0;
            start = scan;
        }
        scan += utf8_size(*ptr(bp,scan));
        c += *p == '\t' ? 8 - (c & 7) : 1;
    }
    return (c < COLS ? start : finish);
}

/* Forward scan for start of logical line segment following 'finish' */
point_t segnext(BufferObject *bp, point_t start, point_t finish)
{
    point_t scan = segstart(bp, start, finish);
    char_t *p = ptr(bp, scan);
    int c = 0;

    for (;;) {
        if (bp->buffer.ebuf <= p || COLS <= c)
            break;
        scan += utf8_size(*ptr(bp,scan));
        if (*p == '\n')
            break;
        c += *p == '\t' ? 8 - (c & 7) : 1;
        p = ptr(bp, scan);
    }
    return (p < bp->buffer.ebuf ? scan : pos(bp, bp->buffer.ebuf));
}

/* Move up one screen line */
point_t upup(BufferObject *bp, point_t off)
{
    point_t curr = lnstart(bp, off);
    point_t seg = segstart(bp, curr, off);
    if (curr < seg)
        off = segstart(bp, curr, seg-1);
    else
        off = segstart(bp, lnstart(bp,curr-1), curr-1);
    return (off);
}

/* Move down one screen line */
point_t dndn(BufferObject *bp, point_t off)
{
    return (segnext(bp, lnstart(bp,off), off));
}

/* Return the offset of a column on the specified line */
point_t lncolumn(BufferObject *bp, point_t offset, int column)
{
    int c = 0;
    char_t *p;
    while ((p = ptr(bp, offset)) < bp->buffer.ebuf && *p != '\n' && c < column) {
        c += *p == '\t' ? 8 - (c & 7) : 1;
        offset += utf8_size(*ptr(bp,offset));
    }
    return (offset);
}

void display_char(BufferObject *bp, char_t *p)
{
    if ( (ptr(bp, bp->buffer.mark) == p) && (bp->buffer.mark != NOMARK)) {
        addch(*p | A_REVERSE);
        return;
    } else if (bp->buffer.paren != NOPAREN && pos(bp,p) == bp->buffer.paren) {
        attron(COLOR_PAIR(ID_BRACE));
    }
    addch(*p);
}

void dispmsg(void)
{
    move(MSGLINE, 0);
    if (msgflag) {
        addstr(msgline);
        msgflag = FALSE;
    }
    clrtoeol();
}


void display_utf8(BufferObject *bp, int n)
{
    char sbuf[6];
    int i = 0;

    for (i=0; i<n; i++) {
        sbuf[i] = *ptr(bp, bp->buffer.epage + i);
    }
    sbuf[n] = '\0';
    addstr(sbuf);
}

extern Object *interp;
void modeline(window_t *wp)
{
    int i;
    char lch, mch, och, *mode;
    static char modeline[256];

    /* n = utf8_size(*(ptr(wp->w_bufp, wp->w_bufp->b_point))); */
    attron(COLOR_PAIR(ID_MODELINE));
    move(wp->w_top + wp->w_rows, 0);
    lch = (wp == curwp ? '=' : '-');
    mch = (wp->w_bufp->buffer.modified ? '*' : lch);
    och = (wp->w_bufp->buffer.overwrite ? 'O' : lch);
    Object *result = flisp_lookup(interp, wp->w_bufp->buffer.mode);
    mode = (wp->w_bufp->buffer.mode == nil) ? "Text" : ((SimpleObject*)result)->str;
    snprintf(modeline, 256,
             "%c%c%c Femto: %c%c %s (%s) ",
             lch,och,mch,lch,lch, wp->w_bufp->buffer.name->string, mode);
    addstr(modeline);

    for (i = strlen(modeline) + 1; i <= COLS; i++)
        addch(lch);
    attron(COLOR_PAIR(ID_SYMBOL));
}

void display(window_t *wp, int flag)
{
    char_t *p;
    int i, j, k, nch;
    BufferObject *bp = wp->w_bufp;
    int token_type = ID_DEFAULT;
    
    /* find start of screen, handle scroll up off page or top of file  */
    /* point is always within b_page and b_epage */
    if (bp->buffer.point < bp->buffer.page)
        bp->buffer.page = segstart(bp, lnstart(bp,bp->buffer.point), bp->buffer.point);

    /* reframe when scrolled off bottom */
    /* Note: for some reason, the test would not trigger if point was at the end of buffer:
     * if (bp->buffer.reframe == 1 || (bp->buffer.epage <= bp->buffer.point && curbp->buffer.point != pos(curbp, curbp->buffer.ebuf))) {
     * However, this caused inserted lines to disappear. Now, why was the check in place?
     */
    if (bp->buffer.reframe || (bp->buffer.epage <= bp->buffer.point)) {
        bp->buffer.reframe = false;
        /* Find end of screen plus one. */
        bp->buffer.page = dndn(bp, bp->buffer.point);
        /* if we scoll to EOF we show 1 blank line at bottom of screen */
        if (pos(bp, bp->buffer.ebuf) <= bp->buffer.page) {
            bp->buffer.page = pos(bp, bp->buffer.ebuf);
            i = wp->w_rows - 1;
        } else {
            i = wp->w_rows - 0;
        }
        /* Scan backwards the required number of lines. */
        while (0 < i--)
            bp->buffer.page = upup(bp, bp->buffer.page);
    }

    move(wp->w_top, 0); /* start from top of window */
    i = wp->w_top;
    j = 0;
    bp->buffer.epage = bp->buffer.page;
    set_parse_state(bp, bp->buffer.epage); /* are we in a multline comment ? */

    /* paint screen from top of page until we hit maxline */
    while (1) {
        /* reached point - store the cursor position */
        if (bp->buffer.point == bp->buffer.epage) {
            bp->buffer.row = i;
            bp->buffer.col = j;
        }
        p = ptr(bp, bp->buffer.epage);
        nch = 1;
        if (wp->w_top + wp->w_rows <= i || bp->buffer.ebuf <= p) /* maxline */
            break;
        if (*p != '\r') {
            nch = utf8_size(*p);
            if ( nch > 1) {
                wchar_t c;
                /* reset if invalid multi-byte character */
                if (mbtowc(&c, (char*)p, 6) < 0) mbtowc(NULL, NULL, 0); 
                j += wcwidth(c) < 0 ? 1 : wcwidth(c);
                display_utf8(bp, nch);
            } else if (isprint(*p) || *p == '\t' || *p == '\n') {
                j += *p == '\t' ? 8-(j&7) : 1;
                token_type = parse_text(bp, bp->buffer.epage);
                attron(COLOR_PAIR(token_type));
                display_char(bp, p);
            } else {
                const char *ctrl = unctrl(*p);
                j += (int) strlen(ctrl);
                addstr(ctrl);
            }
        }
        if (*p == '\n' || COLS <= j) {
            j -= COLS;
            if (j < 0)
                j = 0;
            ++i;
        }
        bp->buffer.epage = bp->buffer.epage + nch;
    }

    /* replacement for clrtobot() to bottom of window */
    for (k=i; k < wp->w_top + wp->w_rows; k++) {
        move(k, j); /* clear from very last char not start of line */
        clrtoeol();
        j = 0; /* thereafter start of line */
    }

    b2w(wp); /* save buffer stuff on window */
    modeline(wp);
    if (wp == curwp && flag) {
        dispmsg();
        move(bp->buffer.row, bp->buffer.col); /* set cursor */
        refresh();
    }
    wp->w_update = FALSE;
}

void clear_message_line(void)
{
    msgline[0] = '\0';
    msgflag = FALSE;
    move(MSGLINE, 0);
    clrtoeol();
}

/* Note: maybe replace with a slightly modified prompt() or with message()
     This function does not allow editing.
 */
void display_prompt_and_response(char *prompt, char *response)
{
    mvaddstr(MSGLINE, 0, prompt);
    /* if we have a value print it and go to end of it */
    if (response[0] != '\0')
        addstr(response);
    clrtoeol();
}

void update_display(void)
{
    window_t *wp;
    BufferObject *bp;

    bp = curwp->w_bufp;
    bp->buffer.cpoint = bp->buffer.point; /* cpoint only ever set here */

    /* only one window */
    if (wheadp->w_next == NULL) {
        display(curwp, TRUE);
        refresh();
        bp->buffer.psize = bp->buffer.size;
        return;
    }

    display(curwp, FALSE); /* this is key, we must call our win first to get accurate page and epage etc */

    /* never curwp,  but same buffer in different window or update flag set*/
    for (wp=wheadp; wp != NULL; wp = wp->w_next) {
        if (wp != curwp && (wp->w_bufp == bp || wp->w_update)) {
            w2b(wp);
            display(wp, FALSE);
        }
    }

    /* now display our window and buffer */
    w2b(curwp);
    dispmsg();
    move(curwp->w_row, curwp->w_col); /* set cursor for curwp */
    refresh();
    bp->buffer.psize = bp->buffer.size;  /* now safe to save previous size for next time */
}

void w2b(window_t *w)
{
    w->w_bufp->buffer.point = w->w_point;
    w->w_bufp->buffer.page = w->w_page;
    w->w_bufp->buffer.epage = w->w_epage;
    w->w_bufp->buffer.row = w->w_row;
    w->w_bufp->buffer.col = w->w_col;

    /* fixup pointers in other windows of the same buffer, if size of edit text changed */
    if (w->w_bufp->buffer.point > w->w_bufp->buffer.cpoint) {
        w->w_bufp->buffer.point += (w->w_bufp->buffer.size - w->w_bufp->buffer.psize);
        w->w_bufp->buffer.page += (w->w_bufp->buffer.size - w->w_bufp->buffer.psize);
        w->w_bufp->buffer.epage += (w->w_bufp->buffer.size - w->w_bufp->buffer.psize);
    }
}

void b2w(window_t *w)
{
    w->w_point = w->w_bufp->buffer.point;
    w->w_page = w->w_bufp->buffer.page;
    w->w_epage = w->w_bufp->buffer.epage;
    w->w_row = w->w_bufp->buffer.row;
    w->w_col = w->w_bufp->buffer.col;
    w->w_bufp->buffer.size = (w->w_bufp->buffer.ebuf - w->w_bufp->buffer.buf) - (w->w_bufp->buffer.egap - w->w_bufp->buffer.gap);
}

/*
 * save buffer data on all windows that reference this buffer
 * special behaviour for where we want to see updates in real time
 * (for example *messages* buffer)
 */
void b2w_all_windows(BufferObject *bp)
{
    window_t *wp;

    for (wp=wheadp; wp != NULL; wp = wp->w_next) {
        if (wp->w_bufp == bp) {
            b2w(wp);
        }
    }
}

/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
