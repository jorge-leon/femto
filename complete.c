/* complete.c, Atto Emacs, Hugh Barney, Public Domain, 2016 */


#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include <curses.h>

#include "femto.h"
#include "window.h"
#include "undo.h"
#include "buffer.h"
#include "key.h"
#include "display.h"

/** getfilename() -  basic filename completion, based on code in uemacs/PK
 *
 * @param prompt .. Prompt to display in the message line.
 * @param buf    .. Pointer to char buffer where returned filename is left.
 * @param nbuf   .. Size of buf.
 *
 * @returns: false if aborted by user, true otherwise.
 *
 * If buf is not zero it must be a / terminated directoryname to
 * be used as base directory.
 *
 */
bool getfilename(char *prompt, char *buf, int nbuf)
{
    static char temp_file[] = TEMPFILE;
    char sys_command[NAME_MAX];

    int cpos = strlen(buf);    /* current character position in string */
    int wpos, dpos = cpos;
    int key = 0, c, fd;
    bool didtry, iswild = false;

    FILE *fp = NULL;

    for (;;) {
        didtry = (key == 0x09);    /* Was last command tab-completion? */
        display_prompt_and_response(prompt, buf);
        key = getch(); /* get a character from the user */

        switch(key) {
        case 0x07: /* ctrl-g, abort */
        case 0x0a: /* cr, lf */
        case 0x0d:
            if (fp != NULL) fclose(fp);
            return (key != 0x07);

            /* Note: move dpos if we delete past a / */
        case 0x7f: /* del, erase */
        case 0x08: /* backspace */
            if (cpos == 0) continue;
            buf[--cpos] = '\0';
            break;

        case  0x15: /* C-u kill */
            cpos = 0;
            buf[0] = '\0';
            break;

        case 0x09: /* TAB, complete file name */
            /* scan backwards for a wild card and set iswild */
            iswild=false;
            for (wpos = strlen(buf); wpos; wpos--)
                if ((iswild = (buf[wpos] == '*' || buf[wpos] == '?')))
                    break;

            /* first time retrieval */
            if (didtry == false) {
                /* scan backwards for a directory separator */
                for(dpos = strlen(buf); dpos && buf[dpos] != '/'; dpos--);

                if (fp != NULL) fclose(fp);
                strcpy(temp_file, TEMPFILE);
                if (-1 == (fd = mkstemp(temp_file)))
                    fatal("Failed to create temp file in getfilename()\n");
                sys_command[0] = '\0';
                if (dpos) {
                    dpos++;
                    strcpy(sys_command, "cd ");
                    strncat(sys_command, buf, dpos);
                    strcat(sys_command, " ; echo ");
                    strcat(sys_command, buf+dpos);
                } else {
                    strcpy(sys_command, "echo ");
                    strcat(sys_command, buf);
                    dpos = cpos;
                }
                if (!iswild) strcat(sys_command, "*");
                strcat(sys_command, " >");
                strcat(sys_command, temp_file);
                strcat(sys_command, " 2>/dev/null");
                (void) system(sys_command);
                fp = fdopen(fd, "r");
                unlink(temp_file);
            }
            cpos = dpos;

            /* copy next filename into buf */
            /* Note: breaks on filenames with spaces */
            while ((c = getc(fp)) != EOF && c != '\n' && c != ' ')
                if (cpos < nbuf - 1 && c != '*')
                    buf[cpos++] = c;

            buf[cpos] = '\0';
            if (c != ' ') rewind(fp);
            didtry = true;
            break;

        default:
            if (cpos < nbuf - 1) {
                buf[cpos++] = key;
                buf[cpos] = '\0';
            }
            break;
        }
    }
}

/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
