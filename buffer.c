/* buffer.c, femto, Hugh Barney, Public Domain, 2017 */

#include <assert.h>
#include <string.h>
#include "header.h"

void buffer_init(buffer_t *bp)
{
    bp->b_mark = NOMARK;
    bp->b_point = 0;
    bp->b_paren = NOPAREN;
    bp->b_cpoint = 0;
    bp->b_page = 0;
    bp->b_epage = 0;
    bp->b_reframe = 0;
    bp->b_size = 0;
    bp->b_psize = 0;
    bp->b_flags = 0;
    bp->b_cnt = 0;
    bp->b_buf = NULL;
    bp->b_ebuf = NULL;
    bp->b_gap = NULL;
    bp->b_egap = NULL;
    bp->b_next = NULL;
    bp->b_bname[0] = '\0';
    bp->b_fname[0] = '\0';
    bp->b_utail = NULL;
    bp->b_ucnt = -1;
}

void zero_buffer(buffer_t *bp)
{
    /* reset the gap, make it the whole buffer */
    bp->b_gap = bp->b_buf;
    bp->b_egap = bp->b_ebuf;
    bp->b_point = 0; /* goto start of buffer */
    bp->b_mark = NOMARK;
}

/* get the size of the document in the buffer */
point_t document_size(buffer_t *bp)
{
    return (bp->b_ebuf - bp->b_buf) - (bp->b_egap - bp->b_gap);
}

int buffer_is_empty(buffer_t *bp)
{
    if (bp->b_gap == bp->b_buf && bp->b_egap == bp->b_ebuf)
        return 1;
    return 0;
}

/*
 * Find a buffer, by buffer name. Return a pointer to the buffer_t
 * structure associated with it. If the buffer is not found and the
 * "cflag" is TRUE, create it.
 */
buffer_t *find_buffer(char *bname, int cflag)
{
    buffer_t *bp = NULL;
    buffer_t *sb = NULL;

    debug("find-buffer(%s, %d)\n", bname, cflag);
    bp = bheadp;
    while (bp != NULL) {
        if (strcmp(bname, bp->b_bname) == 0) {
            return (bp);
        }
        bp = bp->b_next;
    }

    if (cflag != FALSE) {
        if ((bp = (buffer_t *) malloc (sizeof (buffer_t))) == NULL)
            return (0);

        buffer_init(bp);
        assert(bp != NULL);

        /* find the place in the list to insert this buffer */
        if (bheadp == NULL) {
            bheadp = bp;
        } else if (strcmp(bheadp->b_bname, bname) > 0) {
            /* insert at the begining */
            bp->b_next = bheadp;
            bheadp = bp;
        } else {
            for (sb = bheadp; sb->b_next != NULL; sb = sb->b_next)
                if (strcmp (sb->b_next->b_bname, bname) > 0)
                    break;

            /* and insert it */
            bp->b_next = sb->b_next;
            sb->b_next = bp;
        }

        safe_strncpy(bp->b_bname, bname, NBUFN);
        if (bp->b_bname[0] == '*')
            add_mode(bp, B_SPECIAL); /* special buffers start with * in the name */
        else if (global_undo_mode)
            add_mode(bp, B_UNDO);

        /* a newly created buffer needs to have a gap otherwise it is not ready for insertion */
        if (!growgap(bp, MIN_GAP_EXPAND))
            msg(f_alloc);
    }
    return bp;
}

/*
 * Given a file name, either find the buffer it uses, or create a new
 * empty buffer to put it in.
 */
buffer_t *find_buffer_by_fname(char *fname)
{
    buffer_t *bp;
    char     bname[NBUFN];

    bp = bheadp;
    for (bp = bheadp; bp != NULL; bp = bp->b_next)
        if (strcmp(fname, bp->b_fname) == 0)
            return (bp);

    make_buffer_name(bname, fname);
    make_buffer_name_uniq(bname);
    bp = find_buffer(bname, TRUE);
    return (bp);
}

void add_mode(buffer_t *bp, buffer_flags_t mode)
{
    /* we dont allow undo mode for special buffers */
    if ( mode == B_UNDO && (bp->b_flags & B_SPECIAL))
        return;

    bp->b_flags |= mode;
}

void delete_mode(buffer_t *bp, buffer_flags_t mode)
{
    bp->b_flags &= ~mode;
}

/*
 * Unlink from the list of buffers
 * Free the memory associated with the buffer
 * assumes that buffer has been saved if modified
 */
int delete_buffer(buffer_t *bp)
{
    buffer_t *sb = NULL;

    /* we must have switched to a different buffer first */
    assert(bp != curbp);

    /* if buffer is the head buffer */
    if (bp == bheadp) {
        bheadp = bp->b_next;
    } else {
        /* find place where the bp buffer is next */
        for (sb = bheadp; sb->b_next != bp && sb->b_next != NULL; sb = sb->b_next)
            ;
        assert(sb->b_next == bp || sb->b_next == NULL);
        sb->b_next = bp->b_next;
    }

    /* now we can delete */
    free_undos(bp->b_utail);
    free(bp->b_buf);
    free(bp);
    return TRUE;
}

void next_buffer(void)
{
    assert(curbp != NULL);
    assert(bheadp != NULL);
    disassociate_b(curwp);
    curbp = (curbp->b_next != NULL ? curbp->b_next : bheadp);
    associate_b2w(curbp,curwp);
}

char* get_buffer_name(buffer_t *bp)
{
    assert(bp->b_bname != NULL);
    return bp->b_bname;
}

char* get_buffer_filename(buffer_t *bp)
{
    assert(bp->b_fname != NULL);
    return bp->b_fname;
}

char* get_buffer_file_extension(buffer_t *bp)
{
    assert(bp->b_fname != NULL);
    return get_file_extension(bp->b_fname);
}

char* get_buffer_modeline_name(buffer_t *bp)
{
    if (bp->b_fname[0] != '\0')
        return bp->b_fname;
    return bp->b_bname;
}

int count_buffers(void)
{
    buffer_t* bp;
    int i;

    for (i=0, bp=bheadp; bp != NULL; bp = bp->b_next)
        i++;

    return i;
}

int modified_buffers(void)
{
    buffer_t* bp;

    for (bp=bheadp; bp != NULL; bp = bp->b_next)
        if (!(bp->b_flags & B_SPECIAL) && bp->b_flags & B_MODIFIED)
            return TRUE;

    return FALSE;
}

int delete_buffer_byname(char *bname)
{
    buffer_t *bp = find_buffer(bname, FALSE);
    int bcount = count_buffers();

    if (bp == NULL) return FALSE;

    /* if last buffer, create a scratch buffer */
    if (bcount == 1) {
        bp = find_buffer(str_scratch, TRUE);
    }

    /* switch out of buffer if we are the current buffer */
    if (bp == curbp)
        next_buffer();
    assert(bp != curbp);
    delete_buffer(bp);
    return TRUE;
}


int select_buffer(char *bname)
{
    buffer_t *bp = find_buffer(bname, TRUE);

    assert(bp != NULL);
    assert(curbp != NULL);

    disassociate_b(curwp);
    curbp = bp;
    associate_b2w(curbp,curwp);
    return TRUE;
}

/* a version of save buffer specifically for calling by lisp */
int save_buffer_byname(char *bname)
{
    buffer_t *bp = find_buffer(bname, FALSE);

    if (bp == NULL) return FALSE;
    if (bp->b_fname[0] == '\0') return FALSE;

    save_buffer(bp, bp->b_fname);
    return TRUE;
}

char *get_current_bufname(void)
{
    assert(curbp != NULL);
    return get_buffer_name(curbp);
}

char *get_current_filename(void)
{
    assert(curbp != NULL);
    return get_buffer_filename(curbp);
}

char *get_current_file_extension(void)
{
    assert(curbp != NULL);
    static char no_extension[] = "";

    char *ext = get_buffer_file_extension(curbp);

    if (ext == NULL)
       return no_extension;
  
    return ext;
}

void list_buffers(void)
{
    buffer_t *bp;
    buffer_t *list_bp;
    char mod_ch, over_ch;
    char blank[] = " ";
    static char report_line[NAME_MAX + 40];
    char *bn;
    char *fn;

    list_bp = find_buffer(str_buffers, TRUE);

    disassociate_b(curwp); /* we are leaving the old buffer for a new one */
    curbp = list_bp;
    associate_b2w(curbp, curwp);
    clear_buffer(); /* throw away previous content */

    /*             12 1234567 12345678901234567 */
    insert_string("CO    Size Buffer           File\n");
    insert_string("-- ------- ------           ----\n");

    bp = bheadp;
    while (bp != NULL) {
        if (bp != list_bp) {
            mod_ch  = ((bp->b_flags & B_MODIFIED) ? '*' : ' ');
            over_ch = ((bp->b_flags & B_OVERWRITE) ? 'O' : ' ');
            bn = (bp->b_bname[0] != '\0' ? bp->b_bname : blank);
            fn = (bp->b_fname[0] != '\0' ? bp->b_fname : blank);
            snprintf(report_line, sizeof(report_line),  "%c%c %7d %-16s %s\n",  mod_ch, over_ch, bp->b_size, bn, fn);
            insert_string(report_line);
        }
        bp = bp->b_next;
    }
}

/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
