#ifndef BUFFER_H
#define BUFFER_H
/*
 * buffer.h, femto
 */


#include <stdint.h>
#include <stdbool.h>
#include <limits.h>

#define NBUFN           17      /* size of buffer name 16 chars + null terminator */

typedef enum {
    /* State flags */
    ////B_MODIFIED =  0x00001,
    B_OVERWRITE = 0x00002,         /* overwite mode */
    B_SPECIAL =   0x00004,           /* is a special buffer name of form '*name*' */
    B_READONLY =  0x00008,
    B_UNDO =      0x00010,              /* undo mode */
    /* Syntax highlighting, text modes */
    B_CMODE =     0x10000,             /* c mode overrides TEXT mode */
    B_LISP =      0x40000,              /* lisp mode */
    B_PYTHON =    0x80000,            /* python mode */
} buffer_flags_t;

////typedef enum { C, LISP, PYTHON } buffer_mode_t;

typedef int64_t point_t;
typedef unsigned char char_t;

/*
 * This structure supports the unlimited undo feature
 * Its members must be kept to a minimum as each instance takes up to 32 bytes
 */
typedef struct undo_tt {
    point_t  u_point;
    char_t  *u_string;
    char_t  *u_replace;
    char_t   u_type;
    struct undo_tt *u_prev;
} undo_tt;

typedef struct buffer_t
{
    struct buffer_t *b_next;    /* Link to next buffer_t */
    point_t b_mark;             /* the mark */
    point_t b_point;            /* the point */
    point_t b_paren;            /* matching paren to the point */
    point_t b_cpoint;           /* the original current point, used for mutliple window displaying */
    point_t b_page;             /* start of page */
    point_t b_epage;            /* end of page */
    point_t b_reframe;          /* force a reframe of the display */
    int b_cnt;                  /* count of windows referencing this buffer */
    int b_size;                 /* current size of text being edited (not including gap) */
    int b_psize;                /* previous size */
    char_t *b_buf;              /* start of buffer */
    char_t *b_ebuf;             /* end of buffer */
    char_t *b_gap;              /* start of gap */
    char_t *b_egap;             /* end of gap */
    int b_row;                  /* cursor row */
    int b_col;                  /* cursor col */
    char b_fname[NAME_MAX + 1]; /* filename */
    char *name;                 /* buffer name */
    ////buffer_mode_t mode;        /* buffer major mode */
    buffer_flags_t b_flags;
    /* buffer flags */
    bool modified: 1;
    bool overwrite: 1;
    bool special: 1;
    bool readonly: 1;
    bool undo: 1;
    undo_tt *b_utail;           /* recent end of undo list */
    int b_ucnt;                 /* count of how many chars to undo on current undo */
} buffer_t;



extern buffer_t *search_buffer(char *);
extern buffer_t *new_buffer(char *);
extern buffer_t *find_buffer_by_fname(char *);
extern buffer_t *find_buffer(char *, int);
extern char* get_buffer_filename(buffer_t *);
extern char* get_buffer_modeline_name(buffer_t *);
extern char* get_buffer_name(buffer_t *);
extern bool set_buffer_name(buffer_t *, char *);
extern char *get_current_bufname(void);
extern char *get_current_filename(void);

extern int buffer_is_empty(buffer_t *);
extern int count_buffers(void);
extern bool delete_buffer(buffer_t *);
extern int delete_buffer_byname(char *);
extern int modified_buffers(void);
extern int save_buffer_byname(char *);
extern void switch_to_buffer(buffer_t *);
extern point_t document_size(buffer_t *);
extern void add_mode(buffer_t *, buffer_flags_t);
extern void delete_mode(buffer_t *, buffer_flags_t);
extern void list_buffers(void);
extern void zero_buffer(buffer_t *);


#endif
/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
