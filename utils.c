/* util.c, femto, Hugh Barney, Public Domain, 2017 */

#include <assert.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>
#include "header.h"


extern int errno;


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
	int num = 0;
	char basen[NBUFN];
	char bufn[NBUFN];

	if (NULL == find_buffer(bname, FALSE))
		return;

	strcpy(basen, bname);
	basen[14] = '\0';
	basen[15] = '\0';

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
