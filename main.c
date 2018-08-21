/*
 * main.c, femto, Hugh Barney, Public Domain, 2017
 * Derived from: Anthony's Editor January 93, (Public Domain 1991, 1993 by Anthony Howe)
 */

#include "header.h"

int main(int argc, char **argv)
{
	setup_keys();
	(void)init_lisp();


	setlocale(LC_ALL, "") ; /* required for 3,4 byte UTF8 chars */
	if (initscr() == NULL) fatal(f_initscr);
	raw();
	noecho();
	idlok(stdscr, TRUE);

	start_color();
	init_pair(ID_DEFAULT, COLOR_CYAN, COLOR_BLACK);          /* alpha */
	init_pair(ID_SYMBOL, COLOR_WHITE, COLOR_BLACK);          /* non alpha, non digit */
	init_pair(ID_MODELINE, COLOR_BLACK, COLOR_WHITE);        /* modeline */
	init_pair(ID_DIGITS, COLOR_YELLOW, COLOR_BLACK);         /* digits */
	init_pair(ID_BLOCK_COMMENT, COLOR_GREEN, COLOR_BLACK);   /* block comments */
	init_pair(ID_LINE_COMMENT, COLOR_GREEN, COLOR_BLACK);    /* line comments */
	init_pair(ID_SINGLE_STRING, COLOR_YELLOW, COLOR_BLACK);  /* single quoted strings */
	init_pair(ID_DOUBLE_STRING, COLOR_YELLOW, COLOR_BLACK);  /* double quoted strings */
	init_pair(ID_BRACE, COLOR_BLACK, COLOR_CYAN);            /* brace highlight */

	if (1 < argc) {
		char bname[NBUFN];
		char fname[NAME_MAX + 1];
		/* Save filename irregardless of load() success. */
		safe_strncpy(fname, argv[1], NAME_MAX);
		make_buffer_name(bname, fname);
		curbp = find_buffer(bname, TRUE);
		(void)insert_file(fname, FALSE);
		strcpy(curbp->b_fname, fname);
	} else {
		curbp = find_buffer(str_scratch, TRUE);
		strncpy(curbp->b_bname, str_scratch, STRBUF_S);
	}

	wheadp = curwp = new_window();
	one_window(curwp);
	associate_b2w(curbp, curwp);

	beginning_of_buffer();
	load_config();

	while (!done) {
		update_display();
		input = get_key(khead, &key_return);

		if (key_return != NULL) {
			(key_return->k_func)();
		} else {
			/*
			 * if first char of input is a control char then
			 * key is not bound, except TAB and NEWLINE
			 */
			if (*input > 31 || *input == 0x0A || *input == 0x09)
				insert();
                        else {
				flushinp(); /* discard without writing in buffer */
				msg(str_not_bound);
			}
		}

		/* debug_stats("main loop:"); */
		match_parens();
	}

	if (scrap != NULL) free(scrap);
	move(LINES-1, 0);
	refresh();
	noraw();
	endwin();
	return 0;
}

void fatal(char *msg)
{
	if (curscr != NULL) {
		noraw();
		endwin();
	}
	printf("\n%s %s:\n%s\n", E_NAME, E_VERSION, msg);
	exit(1);
}

void msg(char *m, ...)
{
	va_list args;
	va_start(args, m);
	(void) vsprintf(msgline, m, args);
	va_end(args);
	msgflag = TRUE;
}

void load_config()
{
	char fname[300];
	char *output;
	int fd;

	reset_output_stream();
	(void)snprintf(fname, 300, "%s/%s", getenv("HOME"), E_INITFILE);

	if ((fd = open(fname, O_RDONLY)) == -1)
		return;

	reset_output_stream();
	output = load_file(fd);
	assert(output != NULL);
	close(fd);

	/* all exceptions start with the word error: */
	if (NULL != strstr(output, "error:"))
		fatal(output);
	reset_output_stream();
}

void debug(char *format, ...)
{
	char buffer[256]; /* warning this is limited size, we should use vnsprintf */

	va_list args;
	va_start (args, format);

	static FILE *debug_fp = NULL;

	if (debug_fp == NULL) {
		debug_fp = fopen("debug.out","w");
	}

	vsprintf (buffer, format, args);
	va_end(args);

	fprintf(debug_fp,"%s", buffer);
	fflush(debug_fp);
}

void debug_stats(char *s) {
	debug("%s bsz=%d p=%d m=%d gap=%d egap=%d\n", s, curbp->b_ebuf - curbp->b_buf, curbp->b_point, curbp->b_mark, curbp->b_gap - curbp->b_buf, curbp->b_egap - curbp->b_buf);
}
