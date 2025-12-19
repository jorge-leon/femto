/*
 * data.c, femto, Hugh Barney, Public Domain, 2023
 * Derived from: Anthony's Editor January 93, (Public Domain 1991, 1993 by Anthony Howe)
 */

#include "femto.h"
#include "window.h"
#include "undo.h"
#include "buffer.h"
#include "key.h"

int batch_mode = 0;
int debug_mode = 0;
int done;
int result;
int global_undo_mode = 1;
char_t *scrap;
char_t *input;
int msgflag;
char msgline[TEMPBUF];
char response_buf[NAME_MAX];
char searchtext[STRBUF_M];
char replace[STRBUF_M];

char *f_initscr = "%s: Failed to initialize the screen.\n";
char *f_alloc = "%s: Failed to allocate required memory.\n";
char *m_version = E_VERSION_STR;
char *m_alloc = "No more memory available.";
/* Note: not used, insert-file moved to Lisp, but file size check is
 * not implemented there */
char *m_toobig = "File \"%s\" is too big to load.";
char *m_empty = "Nothing to insert.";
char *m_copied = "%ld bytes copied.";
char *m_cut = "%ld bytes cut.";
char *m_line = "Line %d";
char *m_lnot_found = "Line %d, not found";
char *m_replace = "Query replace: ";
char *m_with = "With: ";
char *m_goto = "Goto line: ";
char *m_sprompt = "Search: ";
char *m_qreplace = "Replace '%s' with '%s' ? ";
char *m_rephelp = "(y)es, (n)o, (!)do the rest, (q)uit";
char *m_nomark = "No mark set";
char *m_noregion = "No region defined, mark and point are the same place";

char *str_modified_buffers = "Modified buffers exist; really exit (y/n) ?";
char *str_yes = " y\b";
char *str_no = " n\b";
char *str_mark = "Mark set";
char *str_pos = "Char = %s 0x%x  Line = %d/%d  Point = %d/%d";
char *str_endpos = "[EOB] Line = %d/%d  Point = %d/%d";
char *str_scratch = "*scratch*";
char *str_output = "*output*";
char *str_buffers = "*buffers*";
char *str_clip_too_big = "Region exceeds lisp clipboard limit of %d bytes";
char *str_not_bound  = "Not bound";
char *str_help_buf = "*help*";
char *str_completions = "*completions*";
char *str_apropos = "apropos: ";

/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
