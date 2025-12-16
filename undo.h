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

/* undo types, in matched pairs */
#define UNDO_T_NONE        0
#define UNDO_T_INSERT      1
#define UNDO_T_BACKSPACE   2
#define UNDO_T_KILL        3
#define UNDO_T_YANK        4
#define UNDO_T_DELETE      5
#define UNDO_T_INSAT       6
#define UNDO_T_REPLACE     7

#define STR_T_INSERT       "INSERT"
#define STR_T_BACKSP       "BACKSP"
#define STR_T_KILL         "KILL  "
#define STR_T_YANK         "YANK  "
#define STR_T_DELETE       "DELETE"
#define STR_T_INSAT        "INSAT "
#define STR_T_REPLACE      "REPLC "
#define STR_T_NONE         "NONE  "


extern undo_tt *execute_undo(undo_tt *);
extern void free_undos(undo_tt *);
extern void undo_command(void);
extern void add_undo(buffer_t *, char, point_t, char_t *, char_t *);

#endif
