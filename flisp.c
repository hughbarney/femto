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
    char *library_path, *init_file;
    Interpreter *interp;
    char input[INPUT_FMT_BUFSIZ];
    ResultCode result = RESULT_OK;

    if ((init_file = getenv("FLISPRC")) == NULL)
        init_file = CPP_XSTR(E_INITFILE);
    
    if ((library_path=getenv("FLISPLIB")) == NULL)
        library_path = CPP_XSTR(E_SCRIPTDIR);

    interp = lisp_init(argc, argv, library_path);
    if (interp == NULL)
        fatal("fLisp initialization failed");

    interp->output = file_fopen(interp, ">1", "");
    interp->message = file_fopen(interp, ">2", "");
    interp->debug = file_fopen(interp, ">2", "");

    if (strlen(init_file)) {
        // Note: we do not have input streams yet
        result = lisp_eval(interp, "(load \"%s\"", init_file);
        if (result)
            return result;
    }
    
    while(1) {
        if (isatty(0))
            write(0, "\n> ", 3);
        fflush(stdout);
        // Note: we do not have input streams yet
        if (fgets(input, INPUT_FMT_BUFSIZ, stdin) == NULL) break;
        
        result = lisp_eval(interp, input);
    }
    puts("");
    return result;
}

/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
