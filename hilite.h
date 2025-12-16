#ifndef HILITE_H
#define HILITE_H

#define ID_DEFAULT         1
#define ID_SYMBOL          2
#define ID_MODELINE        3
#define ID_DIGITS          4
#define ID_LINE_COMMENT    5
#define ID_BLOCK_COMMENT   6
#define ID_DOUBLE_STRING   7
#define ID_SINGLE_STRING   8
#define ID_BRACE           9
// python pain
#define ID_TRIPLE_DOUBLE_QUOTE 10
#define ID_TRIPLE_SINGLE_QUOTE 11
// python assignment to multiline string
#define ID_TRIPLE_DOUBLE_QUOTE_S 12
#define ID_TRIPLE_SINGLE_QUOTE_S 13
#define ID_ASSIGNMENT 14

extern void hilite_init();
extern int parse_text(buffer_t *, point_t);
extern void set_parse_state(buffer_t *, point_t);

#endif
