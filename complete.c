/* complete.c, Atto Emacs, Hugh Barney, Public Domain, 2016 */

#include "header.h"

/* basic filename completion, based on code in uemacs/PK */
int getfilename(char *prompt, char *buf, int nbuf)
{
    static char temp_file[] = TEMPFILE;
    int cpos = 0;    /* current character position in string */
    int k = 0, c, fd, didtry, iswild = 0;

    char sys_command[255];
    FILE *fp = NULL;
    buf[0] ='\0';

    for (;;) {
        didtry = (k == 0x09);    /* Was last command tab-completion? */
        display_prompt_and_response(prompt, buf);
        k = getch(); /* get a character from the user */

        switch(k) {
        case 0x07: /* ctrl-g, abort */
        case 0x0a: /* cr, lf */
        case 0x0d:
            if (fp != NULL) fclose(fp);
            return (k != 0x07 && cpos > 0);

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
            /* scan backwards for a wild card and set */
            iswild=0;
            while (cpos > 0) {
                cpos--;
                if (buf[cpos] == '*' || buf[cpos] == '?')
                    iswild = 1;
            }

            /* first time retrieval */
            if (! didtry) {
                if (fp != NULL) fclose(fp);
                strcpy(temp_file, TEMPFILE);
                if (-1 == (fd = mkstemp(temp_file)))
                    fatal("%s: Failed to create temp file\n");
                strcpy(sys_command, "echo ");
                strcat(sys_command, buf);
                if (!iswild) strcat(sys_command, "*");
                strcat(sys_command, " >");
                strcat(sys_command, temp_file);
                strcat(sys_command, " 2>&1");
                (void) ! system(sys_command); /* stop compiler unused result warning */
                fp = fdopen(fd, "r");
                unlink(temp_file);
            }

            /* copy next filename into buf */
            while ((c = getc(fp)) != EOF && c != '\n' && c != ' ')
                if (cpos < nbuf - 1 && c != '*')
                    buf[cpos++] = c;

            buf[cpos] = '\0';
            if (c != ' ') rewind(fp);
            didtry = 1;
            break;

        default:
            if (cpos < nbuf - 1) {
                buf[cpos++] = k;
                buf[cpos] = '\0';
            }
            break;
        }
    }
}
