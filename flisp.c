/*
 * flisp.c, Georg Lehner, Public Domain, 2024
 */

#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include "lisp.h"

#define CPP_XSTR(s) CPP_STR(s)
#define CPP_STR(s) #s

Object *stderrStream;
ResultCode result;

// Note: wouldn't need, if we could implement the repl in fLisp
#define INPUT_BUFSIZE 4095
char input[INPUT_BUFSIZE+1]; // Note: termios paste limit or so

#define FLUSH_STDOUT \
    if (file_fflush(interp, interp->output)) \
        fatal("failed to flush output stream")
#define FLUSH_STDERR \
    if (file_fflush(interp, stderrStream)) \
        fatal("failed to flush error stream")


void fatal(char *msg)
{
    fprintf(stderr, "\n%s %s:\n%s\n", FL_NAME, FL_VERSION, msg);
    exit(1);
}

// Note: we'd like to implement the repl() in fLisp itself, for this we'd need:
// - isatty()
// - exception handling in fLisp
// - file output for error messages
int repl(Interpreter *interp)
{
    size_t i;

    writeString(interp->output, FL_NAME " " FL_VERSION "\n");
    while (true) {
        writeString(interp->output, "> ");
        FLUSH_STDOUT;
        FLUSH_STDERR;

        if (!fgets(input, sizeof(input), stdin)) break;
        i=strlen(input);
        if (input[i-1] == '\n')
            input[i-1] = '\0';
        else {
            writeString(stderrStream, "error: more then " CPP_STR(INPUT_BUFSIZ) "read, skipping...\n");
            continue;
        }

        if (lisp_eval_string(interp, input)) {
            writeString(stderrStream, "error: ");
            if (interp->object != nil) {
                writeString(stderrStream, "object '");
                writeObject(stderrStream, interp->object, true);
                writeString(stderrStream, "', ");
            }

            writeString(stderrStream, interp->message);
            writeChar(stderrStream, '\n');
        }
    }
    if (ferror(interp->input->fd))
        fatal("failed to read input stream");

    FLUSH_STDOUT;
    FLUSH_STDERR;

    return interp->result;
}

int main(int argc, char **argv)
{
    char *library_path, *init_file, *debug_file;
    Interpreter *interp;
    jmp_buf exceptionEnv;

    if ((init_file = getenv("FLISPRC")) == NULL)
        init_file = FL_LIBDIR "/" FL_INITFILE;

    if ((library_path=getenv("FLISPLIB")) == NULL)
        library_path = CPP_XSTR(FL_LIBDIR);

    interp = lisp_init(argc, argv, library_path);
    if (interp == NULL)
        fatal("fLisp interpreter initialization failed");

    debug_file=getenv("FLISP_DEBUG");

    if (nil == (interp->output = file_fopen(interp, ">1", "a")))
        fatal("could not open output stream");
    if (nil == (stderrStream = file_fopen(interp, ">2", "a")))
        fatal("could not open error stream");
    if (debug_file != NULL)
        if (nil == (interp->debug = file_fopen(interp, debug_file, "w")))
            fprintf(stderr, "failed to open debug file %s:%d: %s\n", debug_file, interp->result, interp->message);

    if (strlen(init_file)) {
        if (nil == (interp->input = file_fopen(interp, init_file, "r")))
            fprintf(stderr, "failed to open inifile %s:%d: %s\n", init_file, interp->result, interp->message);
        else {
            interp->stackframe = &exceptionEnv;
            // Note: if we could implement the repl in fLisp itself we'd bail out here.
            if (lisp_eval(interp))
                fprintf(stderr, "failed to load inifile %s:%d: %s\n", init_file, interp->result, interp->message);
            interp->stackframe = NULL;
            if (file_fclose(interp, interp->input))
                fprintf(stderr, "failed to close inifile %s:%d %s\n", init_file, interp->result, interp->message);
        }
    }
    if (nil == (interp->input = file_fopen(interp, "<0", "r")))
        fatal("failed to open input stream");

    //Note: could be omitted if we could implement the repl in fLisp itself.
    if (isatty(fileno(interp->input->fd)))
        return repl(interp);

    interp->stackframe = &exceptionEnv;
    result = lisp_eval(interp);
    FLUSH_STDOUT;
    if (result) {
        writeString(stderrStream, "error: ");
        if (interp->object != nil) {
            writeString(stderrStream, "object '");
            writeObject(stderrStream, interp->object, true);
            writeString(stderrStream, "', ");
        }
        writeString(stderrStream, interp->message);
        FLUSH_STDERR;
        return result;
    }
    lisp_destroy(interp);
    return 0;
}

/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
