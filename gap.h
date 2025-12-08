#ifndef GAP_H
#define GAP_H
/*
 * gap.h, femto
 */

#include <stdio.h>

extern char_t * ptr(buffer_t *, register point_t);
extern bool growgap(buffer_t *, point_t);
extern size_t buffer_fread(buffer_t *, size_t, FILE *);
extern size_t buffer_fwrite(buffer_t *, size_t, FILE *);
extern point_t line_to_point(int);
extern point_t movegap(buffer_t *, point_t);
extern point_t pos(buffer_t *, register char_t *);
extern void get_line_stats(int *, int *);
extern int buffer_is_empty(buffer_t *);
extern point_t document_size(buffer_t *);
extern void zero_buffer(buffer_t *);

#endif
