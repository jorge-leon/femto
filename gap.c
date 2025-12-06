/*
 * gap.c, femto, Hugh Barney, Public Domain, 2017
 * Derived from: Anthony's Editor January 93, (Public Domain 1991, 1993 by Anthony Howe)
 *
 * Buffer content handling.
 */

#include <sys/stat.h>
#include "buffer.h"
#include "gap.h"
#include "header.h"

/** growgap() -  Enlarge gap by at least n chars.
 *
 * @param bp .. buffer.
 * @param n  .. number of characters.
 *
 * @returns TRUE on success. FALSE if buffer has content but cannot grow.
 *
 * Exits with fatal error when no memory can be allocated the first time.
 *
 * The position of the gap is not changed after enlargment.
 *
 */
/* Note: The fatal exit should be left to the caller, though these
 *        mostly are careless and don't care.
 */
bool growgap(buffer_t *bp, point_t n)
{
    char_t *new;
    point_t buflen, newlen, xgap, xegap;

    assert(bp->b_buf <= bp->b_gap);
    assert(bp->b_gap <= bp->b_egap);
    assert(bp->b_egap <= bp->b_ebuf);

    xgap = bp->b_gap - bp->b_buf;
    xegap = bp->b_egap - bp->b_buf;
    buflen = bp->b_ebuf - bp->b_buf;

    /* reduce number of reallocs by growing by a minimum amount */
    n = (n < MIN_GAP_EXPAND ? MIN_GAP_EXPAND : n);
    newlen = buflen + n * sizeof (char_t);

    if (buflen == 0) {
        if (newlen < 0 || MAX_SIZE_T < newlen)
            fatal(f_alloc);
        new = (char_t*) malloc((size_t) newlen);
        if (new == NULL)
            fatal(f_alloc);    /* Cannot edit a file without a buffer. */
    } else {
        if (newlen < 0 || MAX_SIZE_T < newlen) {
            msg(m_alloc);
            return (FALSE);
        }
        new = (char_t*) realloc(bp->b_buf, (size_t) newlen);
        if (new == NULL) {
            msg(m_alloc); /* Report non-fatal error. */
            return (FALSE);
        }
    }

    /* Relocate pointers in new buffer and append the new
     * extension to the end of the gap.
     */
    bp->b_buf = new;
    bp->b_gap = bp->b_buf + xgap;
    bp->b_ebuf = bp->b_buf + buflen;
    bp->b_egap = bp->b_buf + newlen;
    while (xegap < buflen--)
        *--bp->b_egap = *--bp->b_ebuf;
    bp->b_ebuf = bp->b_buf + newlen;

    assert(bp->b_buf < bp->b_ebuf);          /* Buffer must exist. */
    assert(bp->b_buf <= bp->b_gap);
    assert(bp->b_gap < bp->b_egap);          /* Gap must grow only. */
    assert(bp->b_egap <= bp->b_ebuf);
    return (TRUE);
}

point_t movegap(buffer_t *bp, point_t offset)
{
    char_t *p = ptr(bp, offset);
    while (p < bp->b_gap)
        *--bp->b_egap = *--bp->b_gap;
    while (bp->b_egap < p)
        *bp->b_gap++ = *bp->b_egap++;
    assert(bp->b_gap <= bp->b_egap);
    assert(bp->b_buf <= bp->b_gap);
    assert(bp->b_egap <= bp->b_ebuf);
    return (pos(bp, bp->b_egap));
}

/* Given a buffer offset, convert it to a pointer into the buffer */
char_t * ptr(buffer_t *bp, register point_t offset)
{
    if (offset < 0)
        return (bp->b_buf);
#if VALGRIND
    return (bp->b_buf+offset +
            (
                bp->b_buf +
                offset < bp->b_gap ?
                0 : bp->b_egap-bp->b_gap - 1
                )
        );
#else
    return (bp->b_buf+offset + (bp->b_buf + offset < bp->b_gap ? 0 : bp->b_egap-bp->b_gap));
#endif
}

/* Given a pointer into the buffer, convert it to a buffer offset */
point_t pos(buffer_t *bp, register char_t *cp)
{
    assert(bp->b_buf <= cp && cp <= bp->b_ebuf);
    return (cp - bp->b_buf - (cp < bp->b_egap ? 0 : bp->b_egap - bp->b_gap));
}

size_t buffer_fwrite(buffer_t *buffer, size_t size, FILE *stream)
{
    size_t len;

    if (size == 0)
        return 0;

    buffer->b_point = movegap(buffer, buffer->b_point);
    len = buffer->b_ebuf - buffer->b_egap;
    if (size > len)
        size = len;
    return fwrite(buffer->b_egap, sizeof (char), size, stream);
}

void zero_buffer(buffer_t *bp)
{
    /* reset the gap, make it the whole buffer */
    bp->b_gap = bp->b_buf;
    bp->b_egap = bp->b_ebuf;
    bp->b_point = 0; /* goto start of buffer */
    bp->b_mark = NOMARK;
}

/* get the size of the document in the buffer */
point_t document_size(buffer_t *bp)
{
    return (bp->b_ebuf - bp->b_buf) - (bp->b_egap - bp->b_gap);
}

int buffer_is_empty(buffer_t *bp)
{
    if (bp->b_gap == bp->b_buf && bp->b_egap == bp->b_ebuf)
        return 1;
    return 0;
}


/** Read size bytes from stream into buffer starting at point
    
    @returns: number of bytes read or zero if the buffer cannot be grown by size
*/
size_t buffer_fread(buffer_t *buffer, size_t size, FILE *stream)
{
    size_t len;

    if (size == 0)
        return 0;

    if (buffer->b_egap - buffer->b_gap < size * sizeof (char_t) && !growgap(buffer, size))
        return -1;
    buffer->b_point = movegap(buffer, buffer->b_point);
    len = fread(buffer->b_gap, sizeof (char), size, stream);
    if (len == size)
        buffer->b_gap += len;
    return len;
}

/* find the point for start of line ln */
point_t line_to_point(int ln)
{
    point_t end_p = pos(curbp, curbp->b_ebuf);
    point_t p, start;

    for (p=0, start=0; p < end_p; p++) {
        if ( *(ptr(curbp, p)) == '\n') {
            if (--ln == 0)
                return start;
            if (p + 1 < end_p)
                start = p + 1;
        }
    }
    return -1;
}

/* scan buffer and fill in curline and lastline */
void get_line_stats(int *curline, int *lastline)
{
    point_t end_p = pos(curbp, curbp->b_ebuf);
    point_t p;
    int line;

    *curline = -1;

    for (p=0, line=0; p < end_p; p++) {
        line += (*(ptr(curbp,p)) == '\n') ? 1 : 0;
        *lastline = line;

        if (*curline == -1 && p == curbp->b_point) {
            *curline = (*(ptr(curbp,p)) == '\n') ? line : line + 1;
        }
    }

    *lastline = *lastline + 1;

    if (curbp->b_point == end_p)
        *curline = *lastline;
}

/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
