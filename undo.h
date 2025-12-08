#ifndef UNDO_H
#define UNDO_H

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


extern char *get_undo_type_name(undo_tt *);
//extern int count_undos(buffer_t *);
extern int get_buf_utf8_size(char_t *, int);
//extern int get_total_undo_size(buffer_t *);
extern int get_undo_again(void);
extern int get_undo_size(undo_tt *);
extern undo_tt *execute_undo(undo_tt *);
extern undo_tt *new_undo(void);
extern void append_undo_string(undo_tt *, char_t *);
extern void discard_undo_history(void);
extern void free_undos(undo_tt *);
extern void list_undo_stats(void);
extern void undo_command(void);
extern void add_undo(buffer_t *, char, point_t, char_t *, char_t *);

#endif
