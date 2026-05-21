/* buffer.c, femto, Hugh Barney, Public Domain, 2017
 *
 * Buffer management.
 */

#include <stdlib.h>
#include <string.h>

#include <assert.h>

#include "femto.h"
#include "buffer.h"
#include "window.h"
#include "undo.h"
#include "gap.h"
#include "command.h"

/* Globals */
BufferObject *curbp = (BufferObject*)&nil_obj;                /* current buffer */

FLISP_DEFINE_TYPE(buffer);
FLISP_DEFINE_CONSTANT(mode_c,C);
FLISP_DEFINE_CONSTANT(mode_python,Python);
FLISP_DEFINE_CONSTANT(mode_lisp,Lisp);
FLISP_DEFINE_CONSTANT(mode_dired,Dired);
FLISP_DEFINE_CONSTANT(mode_git,Git);
FLISP_DEFINE_CONSTANT(mode_oxo,OXO);


Object * femto_buffer_register(Object *interp)
{
    Object *e = nil;
    do {
        FLISP_UNLESS_ERR(flisp_register_constant(interp, mode_c, NULL));
        FLISP_UNLESS_ERR(flisp_register_constant(interp, mode_lisp, NULL));
        FLISP_UNLESS_ERR(flisp_register_constant(interp, mode_python, NULL));
        FLISP_UNLESS_ERR(flisp_register_constant(interp, mode_dired, NULL));
        FLISP_UNLESS_ERR(flisp_register_constant(interp, mode_git, NULL));
        FLISP_UNLESS_ERR(flisp_register_constant(interp, mode_oxo, NULL));
    } while (0);
    return e;
}

void buffer_init(BufferObject *bp)
{
    bp->buffer.name = nil;
    bp->buffer.fname = nil;
    bp->buffer.mode = nil;
    bp->buffer.next = bp;

    
    bp->buffer.mark = NOMARK;
    bp->buffer.point = 0;
    bp->buffer.modified = false;
    bp->buffer.overwrite = false;
    bp->buffer.readonly = false;
    bp->buffer.undo = true;
    bp->buffer.special = false;

    bp->buffer.reframe = false;

    bp->buffer.paren = NOPAREN;
    bp->buffer.cpoint = 0;
    bp->buffer.page = 0;
    bp->buffer.epage = 0;
    bp->buffer.size = 0;
    bp->buffer.psize = 0;

    bp->buffer.buf = NULL;
    bp->buffer.ebuf = NULL;
    bp->buffer.gap = NULL;
    bp->buffer.egap = NULL;

    bp->buffer.cnt = 0;
    bp->buffer.utail = NULL;
    bp->buffer.ucnt = -1;
}

#define BUFFER_B_PREV(PREV, BUF) for (PREV = BUF; PREV->buffer.next != BUF; PREV = PREV->buffer.next)

/** new_buffer() - allocate, initialize and register a buffer.
 *
 * @param name .. name of the buffer.
 * @returns buffer object or nil if buffer name is empty or allocation fails.
 *
 * The buffer is put in front of the list.
 *
 * If there is no *scratch* buffer already we create one.
 *
 * It is an error to create a buffer with a name that already exists.
 *
 */
BufferObject *new_buffer(Object *name)
{
    BufferObject *bp, *sb;

    if (name == nil || name->string[0] == '\n')
        return (BufferObject*)nil;

    if ((bp = (BufferObject *) malloc (sizeof (BufferObject))) == NULL)
        return (BufferObject*)nil;

    buffer_init(bp);

    /* a newly created buffer needs to have a gap otherwise it is not ready for insertion */
    if (!growgap(bp, MIN_GAP_EXPAND))
        goto new_buffer_error;

    if (curbp == (BufferObject*)nil) {
        /* assure there is a scratch buffer */
        debug("new_buffer(): curbp is NULL, creating %s buffer\n", str_scratch);
        bp->buffer.name = str_scratch;

        curbp = bp;
        if (strcmp(bp->buffer.name->string, str_scratch->string) == 0) {
            debug("new_buffer(*scratch*): creating %s buffer\n", name->string);
            bp->buffer.next = bp;
            bp->buffer.special = true;
            return bp;
        }
        if ((bp = new_buffer(name)) == (BufferObject*)nil)
            return (BufferObject*)nil;
    }
    debug("new_buffer(): creating %s buffer\n", name->string);
    bp->buffer.name = name;

    BUFFER_B_PREV(sb, curbp);
    sb->buffer.next = bp;
    bp->buffer.next = curbp;
    curbp = bp;
    return bp;

new_buffer_error:
    free(bp);
    debug("new_buffer(): failed to allocate memory\n");
    return (BufferObject*)nil;
}

BufferObject *search_buffer(Object *name)
{
    BufferObject *bp = curbp;
    do
        if (strcmp(name->string, bp->buffer.name->string) == 0)
            return bp;
    while ((bp = bp->buffer.next) != curbp);
    return (BufferObject*)nil;
}
/*
 * Find a buffer, by buffer name. Return the BufferObject or nil. If
 * the buffer is not found and the "cflag" is TRUE, create it.
 */
BufferObject *find_buffer(Object *name, bool cflag)
{
    BufferObject *bp = (BufferObject*)nil;

    debug("find-buffer(%s, %d)\n", name->string, cflag);
    bp = search_buffer(name);

    if (bp == (BufferObject*)nil && cflag)
        bp = new_buffer(name);
    return bp;
}

/*
 * Given a file name, either find the buffer it uses, or create a new
 * empty buffer to put it in.
 */
BufferObject *find_buffer_by_fname(Object *fname)
{
    BufferObject *bp;

    for (bp = curbp; bp != curbp; bp = bp->buffer.next) {
        if (bp->buffer.fname == NULL)
            continue;
        if (strcmp(fname->string, bp->buffer.fname->string) == 0)
            return bp;
    }
    return NULL;
}

/** delete_buffer() - deallocate and unregister a buffer.
 *
 * @param bp  .. buffer
 *
 * @returns TRUE on success, FALSE if we try to delete the *scratch*
 * buffer
 *
 * Assure that the head points to a live buffer and neither the
 * *scratch* nor the current buffer is deleted.
 *
 * Unlink from the list of buffers and free the memory associated with
 * the buffer.
 *
 * Assumes that buffer has been saved if modified
 */
bool delete_buffer(BufferObject *bp)
{
    BufferObject *sb;
    window_t *wp;

    if (bp == curbp || strcmp(bp->buffer.name->string, str_scratch->string) == 0)
        return false;

    BUFFER_B_PREV(sb, bp);

    if (bp == sb) {
        /* lone buffer */
        curbp = (BufferObject*)nil; /* from scratch */
        curbp = new_buffer(str_scratch);
        if (curbp == (BufferObject*)nil)
            return false;
    }
    else if (bp == curbp)
        /* advance curbp before deletion */
        curbp = curbp->buffer.next;
    else
        sb->buffer.next = bp->buffer.next;

    /* disassociate all windows */
    while (bp->buffer.cnt) {
        wp = find_window(bp->buffer.name->string);
        if (wp == NULL)
            return false; /* Note: this would be a programming error and should be at least logged */
        disassociate_b(wp);
        associate_b2w(curbp, wp);
    }

    /* now we can delete */

    free_undos(bp->buffer.utail);
    free(bp->buffer.buf);
    free(bp);

    return true;
}

/* Move buffer to the front of the buffer list */
void pull_buffer(BufferObject *bp)
{
    BufferObject *sb;

    if (bp == curbp)
        return;

    BUFFER_B_PREV(sb, bp);
    sb->buffer.next = bp->buffer.next;
    BUFFER_B_PREV(sb, curbp);
    sb->buffer.next = bp;
    bp->buffer.next = curbp;
    curbp = bp;
}


/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
