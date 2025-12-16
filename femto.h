#define VALGRIND 0
/*
 * femto.h, femto, Hugh Barney, 2023
 * Derived from: Anthony's Editor January 93, (Public Domain 1991, 1993 by Anthony Howe)
 */

#include "lisp.h"

#define E_NAME          "femto"
#define E_VERSION       "2.24"
#define E_LABEL         "Femto:"
#define E_NOT_BOUND     "<not bound>"
#ifndef E_SCRIPTDIR
#define E_SCRIPTDIR    "/usr/local/share/femto"
#endif
#ifndef E_INITFILE
#define E_INITFILE      "/usr/local/share/femto/femto.rc"
#endif
#define E_VERSION_STR    E_NAME " " E_VERSION ", Public Domain, August 2025, by Hugh Barney, Georg Lehner, No warranty."

#define MSGLINE         (LINES-1)
#define NOMARK          -1
#define NOPAREN         -1
#define CHUNK           8096L
#define K_BUFFER_LENGTH 256
#define TEMPBUF         512
#define STRBUF_L        256
#define STRBUF_M        64
#define STRBUF_S        16
#define MAX_KNAME       12
#define MAX_KBYTES      12
#define MAX_KFUNC       30
#define LISP_IN_OUT     2048
#define MIN_GAP_EXPAND  512
#define FWD_SEARCH      1
#define REV_SEARCH      2
#define TEMPFILE        "/tmp/feXXXXXX"

/* undo types, in matched pairs */
#define UNDO_T_NONE        0
#define UNDO_T_INSERT      1
#define UNDO_T_BACKSPACE   2
#define UNDO_T_KILL        3
#define UNDO_T_YANK        4
#define UNDO_T_DELETE      5
#define UNDO_T_INSAT       6
#define UNDO_T_REPLACE     7

#define STR_T_INSERT       "INSERT"
#define STR_T_BACKSP       "BACKSP"
#define STR_T_KILL         "KILL  "
#define STR_T_YANK         "YANK  "
#define STR_T_DELETE       "DELETE"
#define STR_T_INSAT        "INSAT "
#define STR_T_REPLACE      "REPLC "
#define STR_T_NONE         "NONE  "

/* edit field attributes */
#define F_NONE          0
#define F_CLEAR         1
#define ZERO_STRING(X) X[0]='\0';

typedef void (*void_func)(void);

typedef unsigned char char_t;
typedef int64_t point_t;
typedef struct buffer_t buffer_t;
typedef struct window_t window_t;
typedef struct undo_tt undo_tt;

#ifndef NAME_MAX
#define NAME_MAX _POSIX_NAME_MAX
#endif

/*
 * Some compilers define size_t as a unsigned 16 bit number while
 * point_t and off_t might be defined as a signed 32 bit number.
 * malloc(), realloc(), fread(), and fwrite() take size_t parameters,
 * which means there will be some size limits because size_t is too
 * small of a type.
 */
#define MAX_SIZE_T      ((unsigned long) (size_t) ~0)

extern int debug_mode;          /* Enable debugging */
extern int batch_mode;          /* If True GUI is not run */
extern int done;                /* Quit flag. */
extern int msgflag;             /* True if msgline should be displayed. */
extern int global_undo_mode;    /* True if we are undo mode is allowed by default */
extern char_t *scrap;           /* Allocated scrap buffer. */
extern char_t *input;
extern char msgline[];          /* Message line input/output buffer. */
extern char response_buf[];     /* Temporary buffer. */
extern char searchtext[];
extern char replace[];


/* Message strings are defined in data.c */
/* fatal() messages. */
extern char *f_initscr;         /* EXIT_FAILURE ... */
extern char *f_alloc;

/* Messages. */
extern char *m_version;
extern char *m_alloc;
extern char *m_toobig;
extern char *m_empty;
extern char *m_copied;
extern char *m_cut;
extern char *m_line;
extern char *m_lnot_found;
extern char *m_replace;
extern char *m_with;
extern char *m_sprompt;
extern char *m_qreplace;
extern char *m_rephelp;
extern char *m_goto;
extern char *str_mark;
extern char *m_nomark;
extern char *m_noregion;
extern char *str_pos;
extern char *str_endpos;
extern char *str_not_bound;
extern char *str_help_buf;
extern char *str_completions;
extern char *str_apropos;


/* Prompts */
extern char *str_modified_buffers;
extern char *str_yes;
extern char *str_no;
extern char *str_scratch;
extern char *str_output;
extern char *str_buffers;
extern char *str_clip_too_big;


/* functions in complete.c */
extern int getfilename(char *, char *, int);



/* functions in main.c */
extern void debug(char *format, ...);
extern void fatal(char *msg);
extern void msg(char *m, ...);


extern char *eval_string(bool, char *, ...);
extern void free_lisp_output(void);

#define FLISP_INITIAL_MEMORY 5242886UL  /* 256k Lisp object space, no gc on startup */

/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
