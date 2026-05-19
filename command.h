#ifndef COMMAND_H
#define COMMAND_H

extern void append_string(BufferObject *, char *);
extern void backspace(void);
extern void scroll_down(void);
extern void backward_word(void);
extern void beginning_of_buffer(void);
extern void copy_region(void);
extern void delete(void);
extern void down(void);
extern void end_of_buffer(void);
extern bool goto_line(int line);
extern void eval_block(void);
extern void scroll_up(void);
extern void forward_word(void);
extern void insert(void);
extern void insert_string(char *);
extern void kill_region(void);
extern void left(void);
extern void lnbegin(void);
extern void lnend(void);
extern void match_parens(void);
extern void suspend(void);
extern void quit(void);
extern void right(void);
extern void set_mark(void);
extern void up(void);
extern void user_func(void);
extern void yank(void);

extern Object *femto_flisp_init(Object *, Object *);

/* Export void name(void) as Lisp function */
/* Note: the C-Function is used by set_key_internal() bindings */
#define DEFINE_EDITOR_FUNC(name)					\
    extern void name(void);                                             \
    Object *e_##name(Object *interp, Object ** args, Object **env, size_t nArgs)	\
    {									\
        name();								\
        return t;                                                       \
    }


#endif
