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

Interpreter *flisp_interp;

void fatal(char *msg)
{
    fprintf(stderr, "\n%s %s:\n%s\n", FL_NAME, FL_VERSION, msg);
    exit(1);
}

int main(int argc, char **argv)
{
    char *library_path, *init_file, *debug_file;
    Interpreter *interp;
    char input[INPUT_FMT_BUFSIZ];
    ResultCode result = FLISP_OK;

    if ((init_file = getenv("FLISPRC")) == NULL)
        init_file = FL_LIBDIR "/" FL_INITFILE;
    
    if ((library_path=getenv("FLISPLIB")) == NULL)
        library_path = CPP_XSTR(FL_LIBDIR);

    interp = lisp_init(argc, argv, library_path);
    if (interp == NULL)
        fatal("fLisp initialization failed");

    debug_file=getenv("FLISP_DEBUG");
    
    interp->output = file_fopen(interp, ">1", "");
    Object *stderrStream = file_fopen(interp, ">2", "");

    if (debug_file != NULL) { 
        interp->debug = file_fopen(interp, debug_file, "w");
    }
    
    if (isatty(0)) {
        printf(FL_NAME " " FL_VERSION "\n");
        fflush(stdout);
    }
    if (strlen(init_file)) {
        // Note: we do not have input streams yet
        result = lisp_eval(interp, "(load \"%s\"", init_file);
        if (result)
            fprintf(stderr, "failed to load inifile %s: %s", init_file, interp->message);
    }

    while(true) {
        if (isatty(0))
            printf("\n> ");
        fflush(stdout);
        // Note: we do not have input streams yet
        if (fgets(input, INPUT_FMT_BUFSIZ, stdin) == NULL) break;

        if (input[strlen(input)-1] == '\n')
            input[strlen(input)-1] = '\0';
            
        result = lisp_eval(interp, input);
        if (result) {
            writeString(stderrStream, "error: ");
            if (interp->object != nil) {
                writeString(stderrStream, "object '");
                writeObject(stderrStream, interp->object, true);
                writeString(stderrStream, "', ");
            }
            writeString(stderrStream, interp->message);
            fflush(stderrStream->fd);
        }
    }
    puts("");
    return 0;
}

/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
