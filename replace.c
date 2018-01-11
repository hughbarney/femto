/* replace.c, femto, Hugh Barney, Public Domain, 2017 */

#include <string.h>
#include "header.h"

/*search for a string and replace it with another string */
void query_replace(void)
{
	point_t o_point = curbp->b_point;
	point_t l_point = -1;
	point_t found;
	char question[STRBUF_L];
	int slen, rlen;   /* length of search and replace strings */
	int numsub = 0;   /* number of substitutions */
	int ask = TRUE;
	int c;

	searchtext[0] = '\0';
	replace[0] = '\0';

	if (!getinput(m_replace, (char*)searchtext, STRBUF_M, F_CLEAR))
		return;

	if (!getinput(m_with, (char*)replace, STRBUF_M, F_CLEAR))
		return;

	slen = strlen(searchtext);
	rlen = strlen(replace);

	/* build query replace question string */
	sprintf(question, m_qreplace, searchtext, replace);

	/* scan through the file, from point */
	numsub = 0;
	while(TRUE) {
		found = search_forward(searchtext);

		/* if not found set the point to the last point of replacement, or where we started */
		if (found == -1) {
			curbp->b_point = (l_point == -1 ? o_point : l_point);
			break;
		}

		curbp->b_point = found;
		/* search_forward places point at end of search, move to start of search */
		curbp->b_point -= slen;

		if (ask == TRUE) {
			msg(question);
			clrtoeol();

		qprompt:
			display(curwp, TRUE);
			c = getch();

			switch (c) {
			case 'y': /* yes, substitute */
				break;

			case 'n': /* no, find next */
				curbp->b_point = found; /* set to end of search string */
				continue;

			case '!': /* yes/stop asking, do the lot */
				ask = FALSE;
				break;

			case 0x1B: /* esc */
				flushinp(); /* discard any escape sequence without writing in buffer */
			case 'q': /* controlled exit */
				return;

			default: /* help me */
				msg(m_rephelp);
				goto qprompt;
			}
		}

		l_point = curbp->b_point; /* save last point */
		replace_string(curbp, searchtext, replace, slen, rlen);
		numsub++;
	}

	msg("%d substitutions", numsub);
}

void replace_string(buffer_t *bp, char *s, char *r, int slen, int rlen)
{
	/*
	 * we call this function with the point set at the start of the search string
	 * search places the point at the end of the search
	 * to claculate the value of found we add on the length of the search string
	 */
	point_t found = bp->b_point + slen;

	if (rlen > slen) {
		movegap(bp, found);
		/*check enough space in gap left */
		if (rlen - slen < bp->b_egap - bp->b_gap)
			growgap(bp, rlen - slen);
		/* shrink gap right by r - s */
		bp->b_gap = bp->b_gap + (rlen - slen);
	} else if (slen > rlen) {
		movegap(bp, found);
		/* stretch gap left by s - r, no need to worry about space */
		bp->b_gap = bp->b_gap - (slen - rlen);
	} else {
		/* if rlen = slen, we just overwrite the chars, no need to move gap */
	}

	/* now just overwrite the chars at point in the buffer */
	memcpy(ptr(bp, bp->b_point), r, rlen * sizeof (char_t));
	add_mode(bp, B_MODIFIED);

	add_undo(curbp, UNDO_T_REPLACE, curbp->b_point, (char_t *)s, (char_t *)r);
	curbp->b_point = found - (slen - rlen); /* set point to end of replacement */
}
