/*
 * gap.c, femto, Hugh Barney, Public Domain, 2017
 * Derived from: Anthony's Editor January 93, (Public Domain 1991, 1993 by Anthony Howe)
 *
 * Buffer content handling.
 */

#include <stdlib.h>
#include <stdbool.h>
#include <sys/stat.h>

#include <assert.h>

#include "femto.h"
#include "buffer.h"
#include "undo.h"
#include "gap.h"
#include "key.h"

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
 *        mostly don't care.
 */
bool growgap(BufferObject *bp, point_t n)
{
    char_t *new;
    point_t buflen, newlen, xgap, xegap;

    assert(bp->buffer.buf <= bp->buffer.gap);
    assert(bp->buffer.gap <= bp->buffer.egap);
    assert(bp->buffer.egap <= bp->buffer.ebuf);

    xgap = bp->buffer.gap - bp->buffer.buf;
    xegap = bp->buffer.egap - bp->buffer.buf;
    buflen = bp->buffer.ebuf - bp->buffer.buf;

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
            return false;
        }
        new = (char_t*) realloc(bp->buffer.buf, (size_t) newlen);
        if (new == NULL) {
            msg(m_alloc); /* Report non-fatal error. */
            return false;
        }
    }

    /* Relocate pointers in new buffer and append the new
     * extension to the end of the gap.
     */
    bp->buffer.buf = new;
    bp->buffer.gap = bp->buffer.buf + xgap;
    bp->buffer.ebuf = bp->buffer.buf + buflen;
    bp->buffer.egap = bp->buffer.buf + newlen;
    while (xegap < buflen--)
        *--bp->buffer.egap = *--bp->buffer.ebuf;
    bp->buffer.ebuf = bp->buffer.buf + newlen;

    assert(bp->buffer.buf < bp->buffer.ebuf);          /* Buffer must exist. */
    assert(bp->buffer.buf <= bp->buffer.gap);
    assert(bp->buffer.gap < bp->buffer.egap);          /* Gap must grow only. */
    assert(bp->buffer.egap <= bp->buffer.ebuf);
    return true;
}

point_t movegap(BufferObject *bp, point_t offset)
{
    char_t *p = ptr(bp, offset);
    while (p < bp->buffer.gap)
        *--bp->buffer.egap = *--bp->buffer.gap;
    while (bp->buffer.egap < p)
        *bp->buffer.gap++ = *bp->buffer.egap++;
    assert(bp->buffer.gap <= bp->buffer.egap);
    assert(bp->buffer.buf <= bp->buffer.gap);
    assert(bp->buffer.egap <= bp->buffer.ebuf);
    return (pos(bp, bp->buffer.egap));
}

/* Given a buffer offset, convert it to a pointer into the buffer */
char_t * ptr(BufferObject *bp, register point_t offset)
{
    if (offset < 0)
        return (bp->buffer.buf);
#if VALGRIND
    return (bp->buffer.buf+offset +
            (
                bp->buffer.buf +
                offset < bp->buffer.gap ?
                0 : bp->buffer.egap-bp->buffer.gap - 1
                )
        );
#else
    return (bp->buffer.buf+offset + (bp->buffer.buf + offset < bp->buffer.gap ? 0 : bp->buffer.egap-bp->buffer.gap));
#endif
}

/* Given a pointer into the buffer, convert it to a buffer offset */
point_t pos(BufferObject *bp, register char_t *cp)
{
    assert(bp->buffer.buf <= cp && cp <= bp->buffer.ebuf);
    return (cp - bp->buffer.buf - (cp < bp->buffer.egap ? 0 : bp->buffer.egap - bp->buffer.gap));
}

/** buffer_fwrite() - write buffer content to stream
 * @param stream
 * @param size
 *
 * Writes size bytes starting from point to stream, but at most
 * the bytes from point to the end of the buffer.
 *
 */
size_t buffer_fwrite(BufferObject *buffer, FILE *stream, size_t size)
{
    size_t len;

    if (size == 0)
        return 0;

    buffer->buffer.point = movegap(buffer, buffer->buffer.point);
    len = buffer->buffer.ebuf - buffer->buffer.egap;
    if (size > len)
        size = len;
    return fwrite(buffer->buffer.egap, sizeof (char), size, stream);
}

void zero_buffer(BufferObject *bp)
{
    /* reset the gap, make it the whole buffer */
    bp->buffer.gap = bp->buffer.buf;
    bp->buffer.egap = bp->buffer.ebuf;
    bp->buffer.point = 0; /* goto start of buffer */
    bp->buffer.mark = NOMARK;
}

/* get the size of the document in the buffer */
point_t document_size(BufferObject *bp)
{
    return (bp->buffer.ebuf - bp->buffer.buf) - (bp->buffer.egap - bp->buffer.gap);
}

bool buffer_is_empty(BufferObject *bp)
{
    return (bp->buffer.gap == bp->buffer.buf && bp->buffer.egap == bp->buffer.ebuf);
}
/** Read size bytes from stream into buffer starting at point

    @returns: number of bytes read or zero if the buffer cannot be grown by size
*/
size_t buffer_fread(BufferObject *buffer, FILE *stream, size_t size)
{
    size_t len;

    if (size == 0)
        return 0;

    if (buffer->buffer.egap - buffer->buffer.gap < size * sizeof (char_t) && !growgap(buffer, size))
        return -1;
    buffer->buffer.point = movegap(buffer, buffer->buffer.point);
    len = fread(buffer->buffer.gap, sizeof (char), size, stream);
    buffer->buffer.gap += len;

    return len;
}

/* find the point for start of line ln */
point_t line_to_point(int ln)
{
    point_t end_p = pos(curbp, curbp->buffer.ebuf);
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
    point_t end_p = pos(curbp, curbp->buffer.ebuf);
    point_t p;
    int line;

    *curline = -1;

    for (p=0, line=0; p < end_p; p++) {
        line += (*(ptr(curbp,p)) == '\n') ? 1 : 0;
        *lastline = line;

        if (*curline == -1 && p == curbp->buffer.point) {
            *curline = (*(ptr(curbp,p)) == '\n') ? line : line + 1;
        }
    }

    *lastline = *lastline + 1;

    if (curbp->buffer.point == end_p)
        *curline = *lastline;
}

/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
