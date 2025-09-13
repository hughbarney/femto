/*
 * flisp.c, Georg Lehner, Public Domain, 2024
 */

#include <stdlib.h>
#include "lisp.h"

void fatal(char *msg)
{
    fputs("\n" FL_NAME " " FL_VERSION ": ", stderr);
    fputs(msg, stderr);
    fputc('\n', stderr);
    exit(1);
}

int main(int argc, char **argv)
{
    char *library_path, *rcfile, *debug_file;
    FILE *debug_fd = NULL, *input_fd = stdin;
    Interpreter *interp;

    if ((rcfile = getenv("FLISPRC")) == NULL)
        rcfile = FL_LIBDIR "/" FL_RCFILE;

    if (*rcfile != '\0')
        if (!(input_fd = fopen(rcfile, "r")))
            fatal("failed to open rcfile");

    if ((debug_file=getenv("FLISP_DEBUG")) != NULL)
        if ((debug_fd = fopen(debug_file, "w")) == NULL)
            fatal("failed to open debug file");

    if ((library_path=getenv("FLISPLIB")) == NULL)
        library_path = FL_LIBDIR;

    interp = lisp_new(argv, library_path, input_fd, stdout, debug_fd);
    if (interp == NULL)
        fatal("fLisp interpreter initialization failed");

    lisp_eval(interp, NULL);
    if (FLISP_RESULT_CODE(interp) != nil) {
        lisp_write_error(interp, stderr);
        return 1;
    }
    return 0;
}

/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
