#ifndef GAP_H
#define GAP_H
/*
 * gap.h, femto
 */

#include <stdio.h>

extern char_t * ptr(BufferObject *, register point_t);
extern bool growgap(BufferObject *, point_t);
extern size_t buffer_fread(BufferObject *, FILE *, size_t);
extern size_t buffer_fwrite(BufferObject *, FILE *, size_t);
extern point_t line_to_point(int);
extern point_t movegap(BufferObject *, point_t);
extern point_t pos(BufferObject *, register char_t *);
extern void get_line_stats(int *, int *);
extern bool buffer_is_empty(BufferObject *);
extern point_t document_size(BufferObject *);
extern void zero_buffer(BufferObject *);

#endif
