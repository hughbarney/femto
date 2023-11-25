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
        int num, len;
	char bufn[NBUFN];

	if (NULL == find_buffer(bname, FALSE))
		return;

	strncpy(bufn, bname, 15);
	len = strlen(bufn);
	for (num=1; num<10; num++) {
	  sprintf(bufn+len, "%1d", num);
	  if (NULL == find_buffer(bufn, FALSE)) goto uniq;
	}
	strncpy(bufn, bname, 14);
	len = strlen(bufn);
	for (num=10; num<100; num++) {
	  sprintf(bufn+len, "%2d", num);
	  if (NULL == find_buffer(bufn, FALSE)) goto uniq;
	}
	strncpy(bufn, bname, 13);
	len = strlen(bufn);
	for (num=100; num<1000; num++) {
	  sprintf(bufn+len, "%3d", num);
	  if (NULL == find_buffer(bufn, FALSE)) goto uniq;
	}
	assert(false);
 uniq:
	strcpy(bname, bufn);
	return;
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
