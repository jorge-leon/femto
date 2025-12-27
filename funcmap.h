#ifndef FUNCMAP_H
#define FUNCMAP_H

typedef struct string_list_t
{
    struct string_list_t *next;
    char *string;
} string_list_t;

typedef struct command_t {
    char c_name[MAX_KFUNC + 1];
    void (*c_func)(void);
    struct command_t *c_next;
} command_t;

typedef void (*void_func)(void);

extern command_t *cheadp;

extern command_t *register_command(char *, void_func);
extern void apropos(void);
extern void execute_command(void);
extern void describe_bindings(void);
extern void describe_functions(void);

#endif
