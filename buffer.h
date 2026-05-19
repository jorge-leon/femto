#ifndef BUFFER_H
#define BUFFER_H
/*
 * buffer.h, femto
 */


#include <stdint.h>
#include <stdbool.h>
#include <limits.h>

#include "flisp/lisp.h"

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
    /* buffer public flags */
    bool modified: 1;
    bool overwrite: 1;
    bool readonly: 1;
    bool undo: 1;
    bool special: 1;
    /* internal */
    bool reframe: 1;            /* force a reframe of the display */
    
    point_t b_paren;            /* matching paren to the point */
    point_t b_cpoint;           /* the original current point, used for mutliple window displaying */
    point_t b_page;             /* start of page */
    point_t b_epage;            /* end of page */
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

#if 0
typedef struct bufferExt
{
    Object *name;               /* string: buffer name */
    Object *file;               /* string: filename */
    Object *mode;               /* symbol: buffer major mode */
    Object *flags;              /* Buffer flags */

    Object *row;                /* integer: cursor row */
    Object *col;                /* integer: cursor col */
    Object *windows;            /* integer count of windows referencing this buffer */

    Object *buf_start;          /* integer: start of buffer */
    Object *buf_end;            /* integer: end of buffer */
    Object *gap_start;          /* integer: start of gap */
    Object *gap_end;            /* integer: end of gap */
    
    Object *size;               /* integer: current size of text being edited (not including gap) */
    Object *prev_size;          /* integer: previous size */

    /* Points: are all integers */
    Object *mark;               /* the mark */
    Object *point;              /* the point */

    Object *paren;              /* matching paren to the point */
    Object *point_orig;         /* the original current point, used for mutliple window displaying */
    Object *page_start;         /* start of page */
    Object *page_end;           /* end of page */

    /* Note: should just be a flag */
    Object *reframe;            /* force a reframe of the display */


    undo_tt *b_utail;           /* recent end of undo list */
    int b_ucnt;                 /* count of how many chars to undo on current undo */
} bufferExt;
#else
typedef struct BufferObject BufferObject;
typedef struct BufferExt
{
    Object *name;               /* buffer name */
    Object *fname;              /* filename */
    Object *mode;               /* buffer major mode */    
    BufferObject *next;         /* Link to next */

    point_t mark;               /* the mark */
    point_t point;              /* the point */

    /* buffer public flags */
    bool modified: 1;
    bool overwrite: 1;
    bool readonly: 1;
    bool undo: 1;
    bool special: 1;
    /* internal */
    bool reframe: 1;            /* force a reframe of the display */
    
    point_t paren;            /* matching paren to the point */
    point_t cpoint;           /* the original current point, used for mutliple window displaying */
    point_t page;             /* start of page */
    point_t epage;            /* end of page */
    int size;                 /* current size of text being edited (not including gap) */
    int psize;                /* previous size */

    char_t *buf;              /* start of buffer */
    char_t *ebuf;             /* end of buffer */
    char_t *gap;              /* start of gap */
    char_t *egap;             /* end of gap */
    int row;                  /* cursor row */
    int col;                  /* cursor col */

    int cnt;                  /* count of windows referencing this buffer */

    undo_tt *utail;           /* recent end of undo list */
    int ucnt;                 /* count of how many chars to undo on current undo */
} BufferExt;
#endif
typedef struct BufferObject {
    SimpleObject self;
    BufferExt buffer;  
} BufferObject;


extern BufferObject *curbp;         /* current buffer */

/* Major modes */
extern Object *mode_c;
extern Object *mode_lisp;
extern Object *mode_python;
extern Object *mode_dired;
extern Object *mode_git;
extern Object *mode_oxo;

extern Object *femto_buffer_register(Object *);

extern BufferObject *new_buffer(Object*);
extern BufferObject *find_buffer(Object*, bool);
extern BufferObject *find_buffer_by_fname(Object*);
extern bool set_buffer_name(BufferObject*, Object*);
extern bool delete_buffer(BufferObject*);
extern void pull_buffer(BufferObject*);


#endif
/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
