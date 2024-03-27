/*
 * gap.c, femto, Hugh Barney, Public Domain, 2017
 * Derived from: Anthony's Editor January 93, (Public Domain 1991, 1993 by Anthony Howe)
 */

#include <sys/stat.h>
#include "header.h"

/* Enlarge gap by n chars, position of gap cannot change */
int growgap(buffer_t *bp, point_t n)
{
    char_t *new;
    point_t buflen, newlen, xgap, xegap;

    assert(bp->b_buf <= bp->b_gap);
    assert(bp->b_gap <= bp->b_egap);
    assert(bp->b_egap <= bp->b_ebuf);

    xgap = bp->b_gap - bp->b_buf;
    xegap = bp->b_egap - bp->b_buf;
    buflen = bp->b_ebuf - bp->b_buf;

    /* reduce number of reallocs by growing by a minimum amount */
    n = (n < MIN_GAP_EXPAND ? MIN_GAP_EXPAND : n);
    newlen = buflen + n * sizeof (char_t);

    if (buflen == 0) {
        if (newlen < 0 || MAX_SIZE_T < newlen)
            fatal(f_alloc);
        new = (char_t*) malloc((size_t) newlen);
        if (new == NULL)
            fatal(f_alloc);    /* Cannot edit a file without a buffer. */
    } else {
        if (newlen < 0 || MAX_SIZE_T < newlen) {
            msg(m_alloc);
            return (FALSE);
        }
        new = (char_t*) realloc(bp->b_buf, (size_t) newlen);
        if (new == NULL) {
            msg(m_alloc); /* Report non-fatal error. */
            return (FALSE);
        }
    }

    /* Relocate pointers in new buffer and append the new
     * extension to the end of the gap.
     */
    bp->b_buf = new;
    bp->b_gap = bp->b_buf + xgap;
    bp->b_ebuf = bp->b_buf + buflen;
    bp->b_egap = bp->b_buf + newlen;
    while (xegap < buflen--)
        *--bp->b_egap = *--bp->b_ebuf;
    bp->b_ebuf = bp->b_buf + newlen;

    assert(bp->b_buf < bp->b_ebuf);          /* Buffer must exist. */
    assert(bp->b_buf <= bp->b_gap);
    assert(bp->b_gap < bp->b_egap);          /* Gap must grow only. */
    assert(bp->b_egap <= bp->b_ebuf);
    return (TRUE);
}

point_t movegap(buffer_t *bp, point_t offset)
{
    char_t *p = ptr(bp, offset);
    while (p < bp->b_gap)
        *--bp->b_egap = *--bp->b_gap;
    while (bp->b_egap < p)
        *bp->b_gap++ = *bp->b_egap++;
    assert(bp->b_gap <= bp->b_egap);
    assert(bp->b_buf <= bp->b_gap);
    assert(bp->b_egap <= bp->b_ebuf);
    return (pos(bp, bp->b_egap));
}

/* Given a buffer offset, convert it to a pointer into the buffer */
char_t * ptr(buffer_t *bp, register point_t offset)
{
    if (offset < 0)
        return (bp->b_buf);
    return (bp->b_buf+offset + (bp->b_buf + offset < bp->b_gap ? 0 : bp->b_egap-bp->b_gap));
}

/* Given a pointer into the buffer, convert it to a buffer offset */
point_t pos(buffer_t *bp, register char_t *cp)
{
    assert(bp->b_buf <= cp && cp <= bp->b_ebuf);
    return (cp - bp->b_buf - (cp < bp->b_egap ? 0 : bp->b_egap - bp->b_gap));
}

int posix_file(char *fn)
{
    if (fn[0] == '_')
        return (FALSE);

    for (; *fn != '\0'; ++fn) {
        if (!isalnum(*fn) && *fn != '.' && *fn != '_' && *fn != '-' && *fn != '/')
            return (FALSE);
    }
    return (TRUE);
}

int save_buffer(buffer_t *bp, char *fn)
{
    FILE *fp;
    point_t length;

    if (!posix_file(fn)) {
        msg(m_badname);
        return (FALSE);
    }
    fp = fopen(fn, "w");
    if (fp == NULL) {
        msg(m_open, fn);
        return (FALSE);
    }
    (void) movegap(bp, (point_t) 0);
    length = (point_t) (bp->b_ebuf - bp->b_egap);
    if (fwrite(bp->b_egap, sizeof (char), (size_t) length, fp) != length) {
        msg(m_write, fn);
        return (FALSE);
    }
    if (fclose(fp) != 0) {
        msg(m_close, fn);
        return (FALSE);
    }
    delete_mode(bp, B_MODIFIED);
    msg(m_saved, fn, pos(bp, bp->b_ebuf));
    return (TRUE);
}

void clear_buffer()
{
    zero_buffer(curbp);
    beginning_of_buffer();
}

int e_load_file(char *fn)
{
    clear_buffer();
    return insert_file(fn, FALSE);
}

/* reads file into buffer at point */
int insert_file(char *fn, int modflag)
{
    FILE *fp;
    size_t len;
    struct stat sb;

    if (stat(fn, &sb) < 0) {
        msg(m_stat, fn);
        return (FALSE);
    }
    if (MAX_SIZE_T < sb.st_size) {
        msg(m_toobig, fn);
        return (FALSE);
    }
    if (curbp->b_egap - curbp->b_gap < sb.st_size * sizeof (char_t) && !growgap(curbp, sb.st_size))
        return (FALSE);
    if ((fp = fopen(fn, "r")) == NULL) {
        msg(m_open, fn);
        return (FALSE);
    }
    curbp->b_point = movegap(curbp, curbp->b_point);
    curbp->b_gap += len = fread(curbp->b_gap, sizeof (char), (size_t) sb.st_size, fp);

    if (fclose(fp) != 0) {
        msg(m_close, fn);
        return (FALSE);
    }
    curbp->b_flags &= (modflag ? B_MODIFIED : ~B_MODIFIED);
    msg(m_loaded, fn, len);
    return (TRUE);
}

/* find the point for start of line ln */
point_t line_to_point(int ln)
{
    point_t end_p = pos(curbp, curbp->b_ebuf);
    point_t p, start;

    for (p=0, start=0; p < end_p; p++) {
        if ( *(ptr(curbp, p)) == '\n') {
            if (--ln == 0)
                return start;
            if (p + 1 < end_p)
                start = p + 1;
        }
    }
    return -1;
}

/* scan buffer and fill in curline and lastline */
void get_line_stats(int *curline, int *lastline)
{
    point_t end_p = pos(curbp, curbp->b_ebuf);
    point_t p;
    int line;

    *curline = -1;

    for (p=0, line=0; p < end_p; p++) {
        line += (*(ptr(curbp,p)) == '\n') ? 1 : 0;
        *lastline = line;

        if (*curline == -1 && p == curbp->b_point) {
            *curline = (*(ptr(curbp,p)) == '\n') ? line : line + 1;
        }
    }

    *lastline = *lastline + 1;

    if (curbp->b_point == end_p)
        *curline = *lastline;
}
