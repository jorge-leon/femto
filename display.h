#ifndef DISPLAY_H
#define DISPLAY_H

extern point_t dndn(buffer_t *, point_t);
extern point_t lncolumn(buffer_t *, point_t, int);
extern point_t lnstart(buffer_t *, register point_t);
extern point_t segstart(buffer_t *, point_t, point_t);
extern point_t upup(buffer_t *, point_t);
extern void b2w_all_windows(buffer_t *);
extern void b2w(window_t *w);
extern void clear_message_line(void);
extern void display_prompt_and_response(char *, char *);
extern void display(window_t *, int);
extern void dispmsg(void);
extern void modeline(window_t *);
extern void update_display(void);
extern void w2b(window_t *);

#endif
