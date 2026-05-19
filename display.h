#ifndef DISPLAY_H
#define DISPLAY_H

extern point_t dndn(BufferObject *, point_t);
extern point_t lncolumn(BufferObject *, point_t, int);
extern point_t lnstart(BufferObject *, register point_t);
extern point_t segstart(BufferObject *, point_t, point_t);
extern point_t upup(BufferObject *, point_t);
extern void b2w_all_windows(BufferObject *);
extern void b2w(window_t *w);
extern void clear_message_line(void);
extern void display_prompt_and_response(char *, char *);
extern void display(window_t *, int);
extern void dispmsg(void);
extern void update_display(void);
extern void w2b(window_t *);
extern int utf8_size(char_t c);

#endif
