#ifndef HILITE_H
#define HILITE_H

/* functions in hilite.c */
extern char_t get_at(buffer_t *, point_t);
extern int is_symbol(char_t);
extern int parse_text(buffer_t *, point_t);
extern void set_parse_state(buffer_t *, point_t);

#endif
