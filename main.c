/*
 * main.c, femto, Hugh Barney, Public Domain, 2017
 * Derived from: Anthony's Editor January 93, (Public Domain 1991, 1993 by Anthony Howe)
 */

#include <stdarg.h>
#include <stdio.h>
#include <errno.h>
#include "header.h"

void gui(void); /* The GUI loop used in interactive mode */

#define CPP_XSTR(s) CPP_STR(s)
#define CPP_STR(s) #s

static Interpreter *interp;
char debug_file[] = "debug.out";
FILE *prev, *debug_fp = NULL;
char* output;
size_t len;

/** Lisp eval a file
 *
 * @param: file .. path to file.
 *
 * Tries to open the file indicated by *path* and feeds it to the Lisp
 * interpreter.
 *
 * Output and Errors are logged to the debug file descriptor.
 *
 */
void load_file(char *file)
{
    FILE *fd;
    if (!(fd = fopen(file, "r"))) {
        debug("failed to open file %s: %d", file, errno);
        return;
    }
    interp->input.fd = fd;
    interp->output = debug_fp;
    lisp_eval(interp, NULL);
    if (FLISP_RESULT_CODE(interp) != nil) {
        debug("failed to load file %s:\n", file);
        lisp_write_error(interp, debug_fp);
        if (FLISP_RESULT_CODE(interp) == out_of_memory)
            fatal("OOM, exiting..");
    }
    if (fclose(fd))
        debug("failed to close file %s\n", file);
}

int main(int argc, char **argv)
{
    char *envv, *library_path, *init_file;

    batch_mode = ((envv=getenv("FEMTO_BATCH")) != NULL && strcmp(envv, "0"));
    debug_mode = ((envv=getenv("FEMTO_DEBUG")) != NULL && strcmp(envv, "0"));

    if ((library_path=getenv("FEMTOLIB")) == NULL)
        library_path = CPP_XSTR(E_SCRIPTDIR);

    if ((init_file = getenv("FEMTORC")) == NULL)
        init_file = CPP_XSTR(E_INITFILE);

    if (debug_mode)
        if (!(debug_fp = fopen(debug_file, "w")))
            fatal("could not open debug file");

    debug("start\n");

    /* buffers */
    setlocale(LC_ALL, "") ; /* required for 3,4 byte UTF8 chars */
    curbp = find_buffer(str_scratch, TRUE);
    strncpy(curbp->b_bname, str_scratch, STRBUF_S);
    beginning_of_buffer();
    /* windows */
    wheadp = curwp = new_window();
    associate_b2w(curbp, curwp);

    setup_keys();

    /* Lisp interpreter */
    interp = lisp_new(argv, library_path, NULL, NULL, debug_fp);
    if (interp == NULL)
        fatal("fLisp initialization failed");

    if (strlen(init_file))
        load_file(init_file);

    /* GUI */
    if (!batch_mode) gui();

    debug("main(): shutdown\n");
    // Note: exit frees all memory, do we need this here?
    // Note: we can't do
    //lisp_destroy(interp);
    //here, because we get segfaults in wide character routines.
    if (scrap != NULL) free(scrap);
    return 0;
}

/** Handle errors from Lisp scripts
 *
 * @param interp
 */
void msg_lisp_err(Interpreter *interp)
{
    char *buf;
    size_t len;
    FILE *fd;

    if (NULL == (fd = open_memstream(&buf, &len)))
        fatal("failed to allocate error formatting buffer");
    lisp_write_error(interp, fd);
    msg("%s", buf);
    fclose(fd);
    free(buf);
}

/** eval_string - Invoke fLisp interpreter and return result as string
 *
 * @param do_format  If true, the input string is passed through
 *                   printf style formatting, otherwise it is used directly.
 * @param format     Input string for the interpreter.
 *
 * @returns
 * - NULL  if an error occured formatting the input string or
 *         evaluating it.
 * - A pointer to a string buffer with the fLisp interpreter output.
 *   After use this buffer has to be freed with free_lisp_output().
 */
char *eval_string(bool do_format, char *format, ...)
{
    char buf[INPUT_FMT_BUFSIZ], *input;
    int size;
    va_list args;

    if (do_format) {
        va_start(args, format);
        size = vsnprintf (buf, sizeof(buf), format, args);
        va_end(args);
        if (size > INPUT_FMT_BUFSIZ) {
            msg("input string larger then %d", INPUT_FMT_BUFSIZ);
            return NULL;
        }
        input = buf;
    } else {
        input = format;
    }

    prev = interp->output;  // Note: save for double invocation with user defined functions.
    interp->output = open_memstream(&output, &len);
    lisp_eval(interp, input);
    if (interp->output)
        fflush(interp->output);
    if (FLISP_RESULT_CODE(interp) == nil)
        return output;
    msg_lisp_err(interp);
    if (debug_mode) {
        lisp_write_error(interp, debug_fp);
        debug("=> %s\n", output);
    }
    if (FLISP_RESULT_CODE(interp) == out_of_memory)
        fatal("OOM, exiting..");
    free_lisp_output();
    return NULL;
}
void free_lisp_output(void)
{
    if (!interp->output)
        return;
    fclose(interp->output);
    free(output);
    interp->output = prev;
}

void gui(void)
{
    debug("gui(): init\n");
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

    // python pain
    init_pair(ID_TRIPLE_DOUBLE_QUOTE, COLOR_GREEN, COLOR_BLACK);  /* tripple quoted strings, doc strings */
    init_pair(ID_TRIPLE_SINGLE_QUOTE, COLOR_GREEN, COLOR_BLACK);  /* tripple quoted strings, doc strings */
    init_pair(ID_TRIPLE_DOUBLE_QUOTE_S, COLOR_YELLOW, COLOR_BLACK);  /* tripple quoted strings, string assignment */
    init_pair(ID_TRIPLE_SINGLE_QUOTE_S, COLOR_YELLOW, COLOR_BLACK);  /* tripple quoted strings, string assignment */
    init_pair(ID_ASSIGNMENT, COLOR_WHITE, COLOR_BLACK);          /* = operator in python  */

    /* windows */
    one_window(curwp);

    debug("gui(): loop\n");
    while (!done) {
        update_display();
        input = get_key(khead, &key_return);

        if (key_return != NULL)
            (key_return->k_func)();
        else {
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
    debug("gui(): shutdown\n");
    move(LINES-1, 0);
    refresh();
    noraw();
    endwin();
}

void fatal(char *msg)
{
    if (!batch_mode) {
        if (curscr != NULL) {
            noraw();
            endwin();
        }
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

    if (batch_mode) {
        puts(msgline);
        fflush(stdout);
    }
}

void debug(char *format, ...)
{
    char buffer[256];
    va_list args;

    if (debug_fp == NULL) return;

    va_start (args, format);

    vsnprintf (buffer, sizeof(buffer), format, args);
    va_end(args);
    fprintf(debug_fp,"%s", buffer);
    fflush(debug_fp);
}

void debug_stats(char *s)
{
    debug("%s bsz=%d p=%d m=%d gap=%d egap=%d\n", s, curbp->b_ebuf - curbp->b_buf, curbp->b_point, curbp->b_mark, curbp->b_gap - curbp->b_buf, curbp->b_egap - curbp->b_buf);
}

/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
