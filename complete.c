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
char sys_command[NAME_MAX];
bool getfilename(char *prompt, char *buf, int nbuf)
{

    int cpos = strlen(buf);    /* current character position in string */
    int wpos, dpos = cpos;
    int key = 0, c;
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
            if (fp != NULL) pclose(fp);
            debug("getfilename(): quitting\n");
            return (key != 0x07);

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
                if (dpos)
                    dpos++;
                else
                    dpos = cpos;

                strcpy(sys_command, "ls -A1 ");
                strcat(sys_command, buf);
                strcat(sys_command, " 2>/dev/null");
                if (fp != NULL) pclose(fp);
                fp = popen(sys_command, "r");
                if (fp == NULL)
                    fatal("getfilename(): failed to get directory listing\n");
            }
            cpos = dpos;

            /* copy next filename into buf */
            while ((c = getc(fp)) != EOF && c != '\n')
                if (cpos < nbuf - 1 && c != '*')
                    buf[cpos++] = c;
            buf[cpos] = '\0';

            didtry = true;
            break;

        default:
            if (cpos < nbuf - 1) {
                buf[cpos++] = key;
                buf[cpos] = '\0';
            }
            break;
        }
        debug("getfilename(): key processed: '%c', %u\n", key, key);
    }
}

/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
