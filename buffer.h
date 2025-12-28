#ifndef BUFFER_H
#define BUFFER_H
/*
 * buffer.h, femto
 */


#include <stdint.h>
#include <stdbool.h>
#include <limits.h>

#include "flisp.h"

typedef struct buffer_t
{
    char *name;                 /* buffer name */
    struct buffer_t *b_next;    /* Link to next buffer_t */

    char *fname;                /* filename */
    point_t b_mark;             /* the mark */
    point_t b_point;            /* the point */

    /* Note: This must be a constant object, since it is not garbage
     *       collected.  Currently this means, that we can only use
     *       modes defined in C and registered on interpreter startup
     */
    Object *mode;                /* buffer major mode */
    /* buffer flags */
    bool modified: 1;
    bool overwrite: 1;
    bool readonly: 1;
    bool undo: 1;
    bool special: 1;

    point_t b_paren;            /* matching paren to the point */
    point_t b_cpoint;           /* the original current point, used for mutliple window displaying */
    point_t b_page;             /* start of page */
    point_t b_epage;            /* end of page */
    point_t b_reframe;          /* force a reframe of the display */
    int b_size;                 /* current size of text being edited (not including gap) */
    int b_psize;                /* previous size */

    char_t *b_buf;              /* start of buffer */
    char_t *b_ebuf;             /* end of buffer */
    char_t *b_gap;              /* start of gap */
    char_t *b_egap;             /* end of gap */
    int b_row;                  /* cursor row */
    int b_col;                  /* cursor col */

    int b_cnt;                  /* count of windows referencing this buffer */

    undo_tt *b_utail;           /* recent end of undo list */
    int b_ucnt;                 /* count of how many chars to undo on current undo */
} buffer_t;

extern buffer_t *curbp;         /* current buffer */

/* Major modes */
extern Object *mode_c;
extern Object *mode_lisp;
extern Object *mode_python;
extern Object *mode_dired;
extern Object *mode_git;
extern Object *mode_oxo;

extern void femto_buffer_register(Interpreter *);

extern buffer_t *new_buffer(char *);
extern buffer_t *find_buffer(char *, bool);
extern buffer_t *find_buffer_by_fname(char *);
extern bool set_buffer_name(buffer_t *, char *);
extern bool delete_buffer(buffer_t *);
extern void pull_buffer(buffer_t *);


#endif
/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
