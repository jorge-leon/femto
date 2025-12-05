/* util.c, femto, Hugh Barney, Public Domain, 2017 */

#include <assert.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>
#include "buffer.h"
#include "header.h"


int posix_file(char *fn)
{
    if (fn[0] == '-')
        return (FALSE);

    for (; *fn != '\0'; ++fn) {
        if (!isalnum(*fn) && *fn != '.' && *fn != '_' && *fn != '-' && *fn != '/')
            return (FALSE);
    }
    return (TRUE);
}



/* a safe version of strncpy that ensure null terminate in case of overflows */
void safe_strncpy(char *dest, char *src, int nchars)
{
    strncpy(dest, src, nchars);
    *(dest + nchars - 1) = '\0';  /* force null termination */
}

/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
