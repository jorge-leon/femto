#ifndef KEY_H
#define KEY_H

typedef struct keymap_t {
    char k_name[MAX_KNAME + 1];       /* name of key eg "c-c c-f" */
    char k_bytes[MAX_KNAME + 1];      /* bytes of key sequence */
    char k_funcname[MAX_KFUNC + 1];   /* name of function, eg (forward-char) */
    void (*k_func)(void);             /* function pointer */
    struct keymap_t *k_next;          /* link to next keymap_t */
} keymap_t;


extern keymap_t *key_return;    /* Command key return */
extern keymap_t *khead;
extern keymap_t *ktail;

//extern keymap_t *new_key(char *, char *);
//extern void make_key(char *, char *);
//extern void create_keys(void);
//extern int set_key_internal(char *, char *, char *, void (*)(void));
extern int set_key(char *, char *);
extern void setup_keys(void);
extern char_t *get_key(keymap_t *, keymap_t **);
extern char *get_input_key(void);

extern char *get_key_funcname(void);
extern char *get_key_name(void);
extern void execute_key(void);
extern int getinput(char *, char *, int, int);

#endif

/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
