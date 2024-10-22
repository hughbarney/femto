/*
 * flisp.c, Georg Lehner, Public Domain, 2024
 */

#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include "lisp.h"

#define CPP_XSTR(s) CPP_STR(s)
#define CPP_STR(s) #s

ResultCode result;

#define FLISP_MEMORY_SIZE 131072UL /* 128k */
// Note: wouldn't need, if we could implement the repl in fLisp
#define INPUT_BUFSIZE 4095
char input[INPUT_BUFSIZE+1]; // Note: termios paste limit or so

void fatal(char *msg)
{
    fprintf(stderr, "\n%s %s:\n%s\n", FL_NAME, FL_VERSION, msg);
    exit(1);
}

void printError(Interpreter *interp)
{
    Object *stream;

    if (interp->object != nil) {
        stream = interp->output;
        interp->output = nil;
        writeObject(interp, interp->object, true);
        file_fflush(interp, interp->output);
        fprintf(stderr, "error: '%s', %s\n", interp->output->buf, interp->message);
        interp->output = stream;
    } else
        fprintf(stderr,"error: %s\n", interp->message);
    fflush(stderr);
}


// Note: we'd like to implement the repl() in fLisp itself, for this we'd need:
// - isatty()
// - exception handling in fLisp
// - file output for error messages
int repl(Interpreter *interp)
{
    size_t i;
    ResultCode result;

    puts(FL_NAME " " FL_VERSION);
    puts("exit with Ctrl+D");
    while (true) {
        printf("> ");
        fflush(stdout);

        if (!fgets(input, sizeof(input), stdin)) break;
        i=strlen(input);
        if (input[i-1] == '\n')
            input[i-1] = '\0';
        else {
            fprintf(stderr, "error: more then " CPP_STR(INPUT_BUFSIZ) "read, skipping...\n");
            fflush(stderr);
            continue;
        }

        if (lisp_eval_string(interp, input))
            printError(interp);
    }
    if (ferror(interp->input->fd))
        fatal("failed to read input stream");

    fflush(stdout);
    result = interp->result;
    // Note: close output, error?
    lisp_destroy(interp);
    return result;
}

int main(int argc, char **argv)
{
    char *library_path, *init_file, *debug_file;
    FILE *fd;
    Interpreter *interp;
    Object *iniStream;
    jmp_buf exceptionEnv;

    if ((init_file = getenv("FLISPRC")) == NULL)
        init_file = FL_LIBDIR "/" FL_INITFILE;

    if ((library_path=getenv("FLISPLIB")) == NULL)
        library_path = FL_LIBDIR;

    interp = lisp_new(FLISP_MEMORY_SIZE, argc, argv, library_path);
    if (interp == NULL)
        fatal("fLisp interpreter initialization failed");

    debug_file=getenv("FLISP_DEBUG");

    if (nil == (interp->output = lisp_stream(interp, stdout, ">STDOUT")))
        fatal("could not open output stream");

    if (debug_file != NULL) {
        if (!(fd = fopen(debug_file, "w")))
            fprintf(stderr, "failed to open debug file %s for writing: %d\n", debug_file, errno);
        else
            if (nil == (interp->debug = lisp_stream(interp, fd, debug_file)))
                fprintf(stderr, "could not open debug stream, will continue without\n");
    }

    if (strlen(init_file)) {
        if (!(fd = fopen(init_file, "r")))
            fprintf(stderr, "failed to open inifile %s: %d\n", init_file, errno);
        else {
            if (nil == (iniStream = lisp_stream(interp, fd, init_file)))
                fatal("could not open input file stream for inifile");
            else {
                // load inifile
                interp->stackframe = &exceptionEnv;
                if (lisp_eval(interp, iniStream))
                    fprintf(stderr, "failed to load inifile %s:%d: %s\n", init_file, interp->result, interp->message);
                // Note: if we could implement the repl in fLisp itself we'd bail out here.
                interp->stackframe = NULL;
                if (file_fclose(interp, iniStream))
                    fprintf(stderr, "failed to close inifile %s:%d %s\n", init_file, interp->result, interp->message);
            }
        }
    }
    // Start repl
    if (nil == (interp->input = lisp_stream(interp, stdin, "<STDIN")))
        fatal("failed to open input stream");

    //Note: could be omitted if we could implement the repl in fLisp itself.
    if (isatty(fileno(interp->input->fd)))
        return repl(interp);

    // Just eval the input stream
    interp->stackframe = &exceptionEnv;
    result = lisp_eval(interp, interp->input);
    fflush(stdout);
    if (result) {
        printError(interp);
        return interp->result;
    }
    return result;
}

/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
