/* buffer.c, femto, Hugh Barney, Public Domain, 2017
 *
 * Buffer management.
 */

#include <assert.h>
#include <string.h>
#include "buffer.h"
#include "gap.h"
#include "header.h"

void buffer_init(buffer_t *bp)
{
    bp->b_mark = NOMARK;
    bp->b_point = 0;
    bp->b_paren = NOPAREN;
    bp->b_cpoint = 0;
    bp->b_page = 0;
    bp->b_epage = 0;
    bp->b_reframe = 0;
    bp->b_size = 0;
    bp->b_psize = 0;
    bp->b_flags = 0;
    bp->modified = FALSE;
    bp->b_cnt = 0;
    bp->b_buf = NULL;
    bp->b_ebuf = NULL;
    bp->b_gap = NULL;
    bp->b_egap = NULL;
    bp->b_next = NULL;
    bp->name = NULL;
    bp->b_fname[0] = '\0';
    bp->b_utail = NULL;
    bp->b_ucnt = -1;
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

/*
 * Find a buffer, by buffer name. Return a pointer to the buffer_t
 * structure associated with it. If the buffer is not found and the
 * "cflag" is TRUE, create it.
 */
buffer_t *find_buffer(char *name, int cflag)
{
    buffer_t *bp = NULL;

    debug("find-buffer(%s, %d)\n", name, cflag);
    bp = search_buffer(name);

    if (bp == NULL && cflag)
        bp = new_buffer(name);
    return bp;
}
buffer_t *search_buffer(char *name)
{
    buffer_t *bp;
    for (bp = bheadp; bp != NULL; bp = bp->b_next)
        if (strcmp(name, bp->name) == 0)
            break;
    return (bp);
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

/** new_buffer() - allocate, initialize and register a buffer.
 *
 * @param name .. name of the buffer.
 * @returns pointer to buffer or NULL if buffer name is empty or allocation fails.
 *
 * The buffer is inserted alphabetically (as of strcmp()) into the
 * buffer list.
 *
 * If there is no *scratch* buffer already we create one and set it at
 * the head of the list.
 *
 * If there is already a buffer with the same name, we return that
 * one.
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

    if (name[0] == '*')
        add_mode(bp, B_SPECIAL); /* special buffers start with * in the name */
    else if (global_undo_mode)
        add_mode(bp, B_UNDO);
    /* a newly created buffer needs to have a gap otherwise it is not ready for insertion */
    if (!growgap(bp, MIN_GAP_EXPAND))
        msg(f_alloc);
    /* Note: we probably never get here, because growgap fatal()s anyway */

    if (bheadp == NULL) {
        /* assure there is a scratch buffer */
        debug("new_buffer(): bheadp is NULL, creating %s buffer\n", str_scratch);
        if (!set_buffer_name(bp, str_scratch))
            goto new_buffer_error;

        bheadp = bp;
        if (strcmp(bp->name, str_scratch) == 0)
            return bp;

        if ((bp = new_buffer(name)) == NULL)
            return NULL;
    }
    debug("new_buffer(): creating %s buffer\n", name);
    if (!set_buffer_name(bp, name))
        goto new_buffer_error;

    /* find the place in the list to insert this buffer */
    int cmp = 0;
    for (sb = bheadp; sb->b_next != NULL; sb = sb->b_next) {
        if ((cmp = strcmp(sb->b_next->name, name)) == 0) {
            debug("new_buffer(): buffer name already registered\n");
            free(bp->name);
            free(bp);
            return sb;
        } else if (cmp > 0) {
            debug("new_buffer(): place in list found after %s\n", sb->name);
            bp->b_next = sb->b_next;
            sb->b_next = bp;
            return bp;
        }
    }
    debug("new_buffer(): put in front of the list\n");
    bp->b_next = bheadp;
    bheadp = bp;
    return bp;


new_buffer_error:
    free(bp);
    debug("new_buffer(): failed to allocate memory\n");
    return NULL;
}

/*
 * Given a file name, either find the buffer it uses, or create a new
 * empty buffer to put it in.
 */
buffer_t *find_buffer_by_fname(char *fname)
{
    buffer_t *bp;

    bp = bheadp;
    for (bp = bheadp; bp != NULL; bp = bp->b_next)
        if (strcmp(fname, bp->b_fname) == 0)
            break;

    return bp;
}

void add_mode(buffer_t *bp, buffer_flags_t mode)
{
    /* we dont allow undo mode for special buffers */
    if ( mode == B_UNDO && (bp->b_flags & B_SPECIAL))
        return;

    bp->b_flags |= mode;
}

void delete_mode(buffer_t *bp, buffer_flags_t mode)
{
    bp->b_flags &= ~mode;
}

/** delete_buffer() - deallocate and unregister a buffer.
 *
 * @param bp  .. buffer
 *
 * @returns TRUE on success, FALSE if we try to delete the *scratch*
 * buffer
 *
 * Assure that the head and the
 * current buffer point to a live buffer and the *scratch* buffer is
 * never deleted.
 *
 * Unlink from the list of buffers and free the memory associated with
 * the buffer.
 *
 * Assumes that buffer has been saved if modified
 */
bool delete_buffer(buffer_t *bp)
{
    buffer_t *sb;

    if (strcmp(bp->name, str_scratch) == 0)
        return FALSE;

    /* find place where the bp buffer is next */
    for (sb = bheadp; sb->b_next != NULL; sb = sb->b_next)
        if (sb->b_next == bp)
            break;
    if (sb == bp) {
        /* lone buffer */
        bheadp = NULL; /* from scratch */
        bheadp = new_buffer(str_scratch);
        if (bheadp == NULL)
            return FALSE;
    }
    /* if buffer is the head buffer advance the head */
    else if (bp == bheadp) {
        bheadp = bp->b_next;
    } else {
        sb->b_next = bp->b_next;
    }
    /* If buffer is the current buffer, switch the current buffer */
    /* Note: we might want a different "other-buffer" algorithm here */
    if (bp == curbp)
        switch_to_buffer((bp->b_next == NULL) ? bheadp : bp->b_next);

    /* now we can delete */
    free_undos(bp->b_utail);
    free(bp->b_buf);
    free(bp->name);
    free(bp);

    return TRUE;
}

char* get_buffer_name(buffer_t *bp)
{
    return bp->name;
}

char* get_buffer_filename(buffer_t *bp)
{
    assert(bp->b_fname != NULL);
    return bp->b_fname;
}

char* get_buffer_modeline_name(buffer_t *bp)
{
    /* Note: construct a name string which fits into the
     * modeline. don't use the filename */
    if (bp->b_fname[0] != '\0')
        return bp->b_fname;
    return bp->name;
}

int count_buffers(void)
{
    buffer_t* bp;
    int i;

    for (i=0, bp=bheadp; bp != NULL; bp = bp->b_next)
        i++;

    return i;
}

int modified_buffers(void)
{
    buffer_t* bp;

    for (bp=bheadp; bp != NULL; bp = bp->b_next)
        if (!(bp->b_flags & B_SPECIAL) && bp->modified)
            return TRUE;

    return FALSE;
}

void switch_to_buffer(buffer_t *bp)
{
    disassociate_b(curwp);
    curbp = bp;
    associate_b2w(curbp,curwp);
}

char *get_current_bufname(void)
{
    return get_buffer_name(curbp);
}

char *get_current_filename(void)
{
    assert(curbp != NULL);
    return get_buffer_filename(curbp);
}

void list_buffers(void)
{
    buffer_t *bp;
    buffer_t *list_bp;
    char mod_ch, over_ch;
    char blank[] = " ";
    static char report_line[NAME_MAX + 40];
    char *bn;
    char *fn;

    list_bp = find_buffer(str_buffers, TRUE);

    /* Notes: should'n we use popup-buffer here? */
    disassociate_b(curwp); /* we are leaving the old buffer for a new one */
    curbp = list_bp;
    associate_b2w(curbp, curwp);
    clear_buffer(); /* throw away previous content */

    /*             12 1234567 12345678901234567 */
    insert_string("CO    Size Buffer           File\n");
    insert_string("-- ------- ------           ----\n");

    bp = bheadp;
    while (bp != NULL) {
        if (bp != list_bp) {
            mod_ch  = ((bp->modified) ? '*' : ' ');
            over_ch = ((bp->b_flags & B_OVERWRITE) ? 'O' : ' ');
            bn = (bp->name[0] != '\0' ? bp->name : blank);
            fn = (bp->b_fname[0] != '\0' ? bp->b_fname : blank);
            snprintf(report_line, sizeof(report_line),  "%c%c %7d %-16s %s\n",  mod_ch, over_ch, bp->b_size, bn, fn);
            insert_string(report_line);
        }
        bp = bp->b_next;
    }
}

/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
