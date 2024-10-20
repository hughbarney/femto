/*
 * main.c, femto, Hugh Barney, Public Domain, 2017
 * Derived from: Anthony's Editor January 93, (Public Domain 1991, 1993 by Anthony Howe)
 */

#include <stdarg.h>
#include <stdio.h>
#include "header.h"

void load_config(); /* Load configuration/init file */
void gui(); /* The GUI loop used in interactive mode */

#define CPP_XSTR(s) CPP_STR(s)
#define CPP_STR(s) #s

static Interpreter *interp;
char debug_file[] = "debug.out";
FILE *debug_fp = NULL;

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
    interp = lisp_init(argc, argv, library_path);
    if (interp == NULL)
        fatal("fLisp initialization failed");

    if (debug_mode)
        if (nil == (interp->debug = lisp_stream(interp, debug_fp, debug_file)))
            fatal("could not open debug stream");
    if (strlen(init_file)) {
        // Note: not lisp_eval()'ing it, because we want to have
        //     consistent error handling.
        if (eval_string(true, "(load \"%s\")", init_file) != NULL)
            close_eval_output();
    }

    /* GUI */
    if (!batch_mode) gui();

    debug("main(): shutdown\n");
    // Note: exit frees all memory, do we need this here?
    lisp_destroy(interp);
    if (scrap != NULL) free(scrap);
    return 0;
}

char *eval_string(int do_format, char *format, ...)
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

    if ((lisp_eval_string(interp, input)))
        msg("error: %s", interp->message);
    if (debug_mode) {
        if (interp->result)
            debug("error: %s\n", interp->message);
    }
    debug(interp->output->buf);
    if (interp->result) {
        // Note: close output buf...? if we get segfaults.
        //close_eval_output();
        return NULL;
    }
    return interp->output->buf;
}
void close_eval_output()
{
    assert(interp->output->fd != NULL);
    if (file_fclose(interp, interp->output))
        debug("error: closing output stream");
}

void gui()
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
//    if (!debug_mode) return;

    va_start (args, format);

//    static FILE *debug_fp = NULL;

//    if (debug_fp == NULL)
//        debug_fp = fopen("debug.out","w");

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
