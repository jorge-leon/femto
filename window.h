#ifndef WINDOW_H
#define WINDOW_H

typedef struct window_t
{
    struct window_t *w_next;    /* Next window */
    struct buffer_t *w_bufp;    /* Buffer displayed in window */
    struct buffer_t *w_hijack;  /* holds the buffer association for a hijacked window */
    point_t w_point;
    point_t w_mark;
    point_t w_page;
    point_t w_epage;
    char w_top;                 /* origin 0 top row of window */
    char w_rows;                /* no. of rows of text in window */
    int w_row;                  /* cursor row */
    int w_col;                  /* cursor col */
    int w_update;
    char w_name[STRBUF_S];
} window_t;

extern window_t *curwp;
extern window_t *wheadp;

extern int count_windows(void);
extern void associate_b2w(buffer_t *, window_t *);
extern void delete_other_windows(void);
extern void disassociate_b(window_t *);
extern void free_other_windows(window_t *);
extern void hijack_window(window_t *, buffer_t *);
extern void mark_all_windows(void);
extern void one_window(window_t *);
extern void other_window(void);
extern void restore_hijacked_window(window_t *);
extern void split_window(void);
extern window_t *find_window(char *);
extern window_t* new_window(void);
extern window_t *popup_window(char *);
extern window_t *split_current_window(void);


#endif
