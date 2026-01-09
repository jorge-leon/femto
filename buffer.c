/* buffer.c, femto, Hugh Barney, Public Domain, 2017
 *
 * Buffer management.
 */

#include <stdlib.h>
#include <string.h>

#include <assert.h>

#include "femto.h"
#include "undo.h"
#include "buffer.h"
#include "gap.h"
#include "command.h"

/* Globals */
buffer_t *curbp;                /* current buffer */

Object *mode_c = &(Object) { .string = "C" };
Object *mode_python = &(Object) { .string = "Python" };
Object *mode_lisp = &(Object) { .string = "Lisp" };
Object *mode_dired = &(Object) { .string = "Dired" };
Object *mode_git = &(Object) { .string = "Git" };
Object *mode_oxo = &(Object) { .string = "OXO" };

void femto_buffer_register(Interpreter *interp)
{
    flisp_register_constant(interp, mode_c, mode_c);
    flisp_register_constant(interp, mode_lisp, mode_lisp);
    flisp_register_constant(interp, mode_python, mode_python);
    flisp_register_constant(interp, mode_dired, mode_dired);
    flisp_register_constant(interp, mode_git, mode_git);
    flisp_register_constant(interp, mode_oxo, mode_oxo);
}

void buffer_init(buffer_t *bp)
{
    bp->name = NULL;
    bp->b_next = NULL;

    bp->fname = NULL;
    bp->b_mark = NOMARK;
    bp->b_point = 0;
    bp->mode = nil;
    bp->modified = false;
    bp->overwrite = false;
    bp->readonly = false;
    bp->undo = true;
    bp->special = false;

    bp->b_paren = NOPAREN;
    bp->b_cpoint = 0;
    bp->b_page = 0;
    bp->b_epage = 0;
    bp->b_reframe = 0;
    bp->b_size = 0;
    bp->b_psize = 0;

    bp->b_buf = NULL;
    bp->b_ebuf = NULL;
    bp->b_gap = NULL;
    bp->b_egap = NULL;

    bp->b_cnt = 0;
    bp->b_utail = NULL;
    bp->b_ucnt = -1;
}

#define BUFFER_B_PREV(PREV, BUF) for (PREV = BUF; PREV->b_next != BUF; PREV = PREV->b_next)

/** new_buffer() - allocate, initialize and register a buffer.
 *
 * @param name .. name of the buffer.
 * @returns pointer to buffer or NULL if buffer name is empty or allocation fails.
 *
 * The buffer is put in front of the list.
 *
 * If there is no *scratch* buffer already we create one.
 *
 * It is an error to create a buffer with a name that already exists.
 *
 */
buffer_t *new_buffer(char *name)
{
    buffer_t *bp, *sb;

    if (name == NULL || name[0] == '\n')
        return NULL;

    if ((bp = (buffer_t *) malloc (sizeof (buffer_t))) == NULL)
        return NULL;

    buffer_init(bp);

    /* a newly created buffer needs to have a gap otherwise it is not ready for insertion */
    if (!growgap(bp, MIN_GAP_EXPAND))
        goto new_buffer_error;

    if (curbp == NULL) {
        /* assure there is a scratch buffer */
        debug("new_buffer(): curbp is NULL, creating %s buffer\n", str_scratch);
        if (!set_buffer_name(bp, str_scratch))
            goto new_buffer_error;

        curbp = bp;
        if (strcmp(bp->name, str_scratch) == 0) {
            bp->b_next = bp;
            bp->special = true;
            return bp;
        }
        if ((bp = new_buffer(name)) == NULL)
            return NULL;
    }
    debug("new_buffer(): creating %s buffer\n", name);
    if (!set_buffer_name(bp, name))
        goto new_buffer_error;

    BUFFER_B_PREV(sb, curbp);
    sb->b_next = bp;
    bp->b_next = curbp;
    curbp = bp;
    return bp;

new_buffer_error:
    free(bp);
    debug("new_buffer(): failed to allocate memory\n");
    return NULL;
}

buffer_t *search_buffer(char *name)
{
    buffer_t *bp = curbp;
    do
        if (strcmp(name, bp->name) == 0)
            return bp;
    while ((bp = bp->b_next) != curbp);
    return NULL;
}
/*
 * Find a buffer, by buffer name. Return a pointer to the buffer_t
 * structure associated with it. If the buffer is not found and the
 * "cflag" is TRUE, create it.
 */
buffer_t *find_buffer(char *name, bool cflag)
{
    buffer_t *bp = NULL;

    debug("find-buffer(%s, %d)\n", name, cflag);
    bp = search_buffer(name);

    if (bp == NULL && cflag)
        bp = new_buffer(name);
    return bp;
}

/*
 * Given a file name, either find the buffer it uses, or create a new
 * empty buffer to put it in.
 */
buffer_t *find_buffer_by_fname(char *fname)
{
    buffer_t *bp;

    for (bp = curbp; bp != curbp; bp = bp->b_next) {
        if (bp->fname == NULL)
            continue;
        if (strcmp(fname, bp->fname) == 0)
            return bp;
    }
    return NULL;
}

/** set_buffer_name() name or rename buffer
 *
 * @param buffer .. pointer to buffer struct.
 * @param name   .. pointer to string
 *
 * @returns TRUE on success, else FALSE.
 *
 * If the buffer already has a name it is deallocated.
 *
 */

bool set_buffer_name(buffer_t *buffer, char *name)
{
    if (buffer->name != NULL)
        free(buffer->name);

    return (buffer->name = strdup(name)) != NULL;
}

/** delete_buffer() - deallocate and unregister a buffer.
 *
 * @param bp  .. buffer
 *
 * @returns TRUE on success, FALSE if we try to delete the *scratch*
 * buffer
 *
 * Assure that the head points to a live buffer and neither the
 * *scratch* nore the current buffer is deleted.
 *
 * Unlink from the list of buffers and free the memory associated with
 * the buffer.
 *
 * Assumes that buffer has been saved if modified
 */
bool delete_buffer(buffer_t *bp)
{
    buffer_t *sb;

    if (bp == curbp || strcmp(bp->name, str_scratch) == 0)
        return false;

    BUFFER_B_PREV(sb, bp);

    if (bp == sb) {
        /* lone buffer */
        curbp = NULL; /* from scratch */
        curbp = new_buffer(str_scratch);
        if (curbp == NULL)
            return false;
    }
    else if (bp == curbp)
        /* advance curbp before deletion */
        curbp = curbp->b_next;
    else
        sb->b_next = bp->b_next;

    /* now we can delete */
    free_undos(bp->b_utail);
    free(bp->b_buf);
    free(bp->name);
    free(bp->fname);
    free(bp);

    return true;
}

/* Move buffer to the front of the buffer list */
void pull_buffer(buffer_t *bp)
{
    buffer_t *sb;

    if (bp == curbp)
        return;

    BUFFER_B_PREV(sb, bp);
    sb->b_next = bp->b_next;
    BUFFER_B_PREV(sb, curbp);
    sb->b_next = bp;
    bp->b_next = curbp;
    curbp = bp;
}


/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
