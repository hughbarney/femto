/* util.c, femto, Hugh Barney, Public Domain, 2017 */

#include <assert.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>
#include "header.h"

/*
 * Take a file name, and fabricate a buffer name.
 */
void make_buffer_name(char *bname, char *fname)
{
    char *p = fname;

    /* find the end of the filename */
    while (*p != 0)
        ++p;

    /* wind back to the last seperator */
    while (p != fname && p[-1] != '/' && p[-1] != '\\')
        --p;

    safe_strncpy(bname, p, NBUFN);
}

void make_buffer_name_uniq(char *bname)
{
    unsigned char num = 0;
    char basen[NBUFN - 3];
    char bufn[NBUFN];

    if (NULL == find_buffer(bname, FALSE))
        return;

    safe_strncpy(basen, bname, NBUFN - 3);

    while(TRUE) {
        sprintf(bufn, "%s%d", basen, num++);
        
        if (NULL == find_buffer(bufn, FALSE)) {
            strcpy(bname, bufn);
            return;
        }
        assert(num < 100); /* fail after 100 */
    }
}

/* replace control chars with spaces in string s */
void remove_control_chars(char_t *s)
{
    char_t *p = s;

    while (*p != '\0') {
        if (*p < 32)
            *p = ' ';
        p++;
    }
}

/* a safe version of strncpy that ensure null terminate in case of overflows */
void safe_strncpy(char *dest, char *src, int nchars)
{
    strncpy(dest, src, nchars);
    *(dest + nchars - 1) = '\0';  /* force null termination */
}

char* get_file_extension(char *filename) {
    char* dot = strrchr(filename, '.');

    if (!dot || dot == filename) {
        return NULL; // No dot found or dot is the first character
    }
    return dot + 1; // Return pointer to extension (after the dot)
}

/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
