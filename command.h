#ifndef COMMAND_H
#define COMMAND_H

extern char *get_char(void);
extern char *get_clipboard(void);
extern char* get_temp_file(void);
extern char *get_version_string(void);
extern int goto_line(int);
extern int i_check_region(void);
extern int prev_utf8_char_size(void);
extern int utf8_size(char_t c);
extern point_t get_mark(void);
extern point_t get_point(void);
extern point_t get_point_max(void);
extern void append_string(buffer_t *, char *);
extern void backspace(void);
extern void scroll_down(void);
extern void backward_word(void);
extern void beginning_of_buffer(void);
extern void copy_cut(int cut);
extern void copy_region(void);
extern void delete(void);
extern void down(void);
extern void end_of_buffer(void);
extern void eval_block(void);
extern void scroll_up(void);
extern void forward_word(void);
extern void i_gotoline(void);
extern void insert(void);
extern void insert_at(void);
extern void insert_string(char *);
extern void i_set_mark(void);
extern void kill_buffer(void);
extern void kill_region(void);
extern void left(void);
extern void lnbegin(void);
extern void lnend(void);
extern void log_message(char *);
extern void match_parens(void);
extern void suspend(void);
extern void quit(void);
extern void redraw(void);
extern void repl(void);
extern void resize_terminal(void);
extern void right(void);
extern void set_mark(void);
extern void set_point(point_t);
extern void cursor_position(void);
extern void toggle_overwrite_mode(void);
extern void unmark(void);
extern void up(void);
extern void user_func(void);
extern void version(void);
extern void yank(void);

extern void femto_register(Interpreter *);

/* Export void name(void) as Lisp function */
/* Note: the C-Function is used by set_key_internal() bindings */
#define DEFINE_EDITOR_FUNC(name)					\
    extern void name(void);                                             \
    Object *e_##name(Interpreter *interp, Object ** args, Object **env)	\
    {									\
        name();								\
        return t;                                                       \
    }


#endif
