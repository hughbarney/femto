#include <errno.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "lisp.h"
#include "file.h"

Object *permission_denied =  (Object *) (&(Symbol) { NULL, .string = "permission-denied" });
Object *not_found = &(Object) { NULL, .string = "not-found" };
Object *file_exists = &(Object) { NULL, .string = "file-exists" };
Object *read_only = &(Object) { NULL, .string = "read-only" };

Constant flisp_file_constants[] = {
    { &permission_denied, &permission_denied },
    { &not_found, &not_found },
    { &file_exists, &file_exists },
    { &read_only, &read_only },
    { NULL, NULL }
};

/** (fflush[ stream]) - flush stream, output or all streams
 *
 * @param stream  Stream to flush. If t all streams are flushed, if
 *                not given the interpreter output is flushed.
 *
 * @returns t
 * @throws io-error
 */
Object *primitiveFflush(Interpreter *interp, Object** args, Object **env)
{
    FILE *fd = interp->output;

    if (FLISP_HAS_ARGS)
        if (FLISP_ARG_ONE == t)
            fd = NULL;
        else {
            CHECK_TYPE(FLISP_ARG_ONE, type_stream,  "(fflush[ stream]) - stream");
            if (FLISP_ARG_ONE->fd == NULL)
                exception(interp, invalid_value, "(fflush[ stream]) - stream already closed");
            fd = FLISP_ARG_ONE->fd;
        }
    else if (fd == NULL)
        exception(interp, invalid_value, "(fflush[ stream]) - output stream not set");

    if (fflush(fd) == EOF)
        exception(interp, io_error, "(fflush[ stream]) - fflush() failed: %s", strerror(errno));

    return t;
}
/** (fseek stream offset[ relativep]) - seek position in stream or input
 *
 * @param stream     stream object, if nil interpreter input stream.
 * @param offset     offset from start if positive, from end if
 *                   negative.
 * @param relativep  if given and not nil seek from current position.
 */
Object *primitiveFseek(Interpreter *interp, Object** args, Object **env)
{
    int result, whence = SEEK_SET;
    FILE *fd = interp->input.fd;
    off_t pos;

    if (FLISP_ARG_ONE == nil) {
        if (fd == NULL)
            exception(interp, invalid_value, "(fseek stream offset[ relativep]) - input stream not set");
    } else {
        CHECK_TYPE(FLISP_ARG_ONE, type_stream,  "(fseek stream offset) - stream");
        if (FLISP_ARG_ONE->fd == NULL)
            exception(interp, invalid_value, "(fseek stream) - stream already closed");
        fd = FLISP_ARG_ONE->fd;
    }
    CHECK_TYPE(FLISP_ARG_TWO, type_integer, "(fseek stream offset) - offset");

    if (FLISP_HAS_ARG_THREE && FLISP_ARG_THREE != nil)
        whence = SEEK_CUR;
    else if (FLISP_ARG_TWO->integer < 0)
        whence = SEEK_END;
    result = fseeko(fd, FLISP_ARG_TWO->integer, whence);
    if (result == -1)
        exception(interp, io_error, "(fseek stream offset) - fseeko() failed: %s", strerror(errno));

    if ((pos = ftello(fd)) == -1)
        exception(interp, io_error, "(fseek stream offset) - ftello() failed: %s", strerror(errno));

    return newInteger(interp, pos);
}
/** (ftell[ stream]) - return current position in stream or input
 *
 * @param  stream  stream. If not given the input stream is used.
 *
 * @returns current position in stream.
 *
 * @throws
 * - invalid-value  if stream is already closed.
 * - io-error       if ftello fails.
 */
Object *primitiveFtell(Interpreter *interp, Object** args, Object **env)
{
    FILE *fd = interp->input.fd;
    off_t pos;

    if (FLISP_HAS_ARGS) {
        if (FLISP_ARG_ONE->fd == NULL)
            exception(interp, invalid_value, "(ftell[ stream]) - stream already closed");
        fd = FLISP_ARG_ONE->fd;
    } else if (fd == NULL)
        exception(interp, invalid_value, "(ftell[ stream]) - input stream not set");
    if ((pos = ftello(fd)) == -1)
        exception(interp, io_error, "(ftell[ stream]) - ftello() failed: %s", strerror(errno));

    return newInteger(interp, pos);
}
/** (feof[ stream]) - return end-of-file status of stream or input
 *
 * @param stream  stream. If not given the input stream is used.
 *
 * @returns  nil or end-of-file
 */
Object *primitiveFeof(Interpreter *interp, Object** args, Object **env)
{
    FILE *fd = interp->input.fd;

    if (FLISP_HAS_ARGS) {
        if (FLISP_ARG_ONE->fd == NULL)
            exception(interp, invalid_value, "(feof[ stream]) - stream already closed");
        fd = FLISP_ARG_ONE->fd;
    } else if (fd == NULL)
        exception(interp, invalid_value, "(feof[ stream]) - input stream not set");

    return (feof(fd)) ? end_of_file : nil;
}
/** (fgetc[ stream]) - read one character from stream or input
 *
 * @param stream  stream to read input from, if not given read from
 *                interpreter input stream.
 */
Object *primitiveFgetc(Interpreter *interp, Object** args, Object **env)
{
    char s[] = "\0\0";
    int c;
    FILE *fd = interp->input.fd;

    if (FLISP_HAS_ARGS) {
        CHECK_TYPE(FLISP_ARG_ONE, type_stream, "(fgetc[ stream] - stream)");
        if (FLISP_ARG_ONE->fd == NULL)
            exception(interp, invalid_value, "(fgetc[ stream]) - stream already closed");
        fd = FLISP_ARG_ONE->fd;
    } else if (fd == NULL)
        exception(interp, invalid_value, "(fgetc[ stream]) - input stream not set");

    c = streamGetc(interp, fd);
    if (c == EOF)
        return end_of_file;
    s[0] = (char)c;
    return newString(interp, s);
}
/** (fungetc i[ stream]) - ungetc integer i as char to stream or input
 *
 * @param i       integer converted to unsigned char
 * @param stream  stream, if not given the interpreter input stream
 *
 * Caution: ungetc'ing the interpreter input stream will likely cause
 *          undesired results like memory exhaustion.
 *
 * @returns i
 * @throws:
 * - invalid-value  If stream is closed or interpreter input stream is
 *                  not set.
 * - io-error       When ungetc() fails.
 */
/* Note: not yet sure if (fungetc i) is a) a good idea, b) any way
 *   secure.
 */
Object *primitiveFungetc(Interpreter *interp, Object** args, Object **env)
{
    int c;
    FILE *fd = interp->input.fd;

    CHECK_TYPE(FLISP_ARG_ONE, type_integer, "(fungetc char[ stream] - char)");
    if (FLISP_HAS_ARG_TWO) {
        CHECK_TYPE(FLISP_ARG_TWO, type_stream, "(fungetc char[ stream] - stream)");
        if (FLISP_ARG_TWO->fd == NULL)
            exception(interp, invalid_value, "(fungetc char [ stream]) - stream already closed");
        fd = FLISP_ARG_TWO->fd;
    } else if (fd == NULL)
        exception(interp, invalid_value, "(fungetc char [ stream]) - input stream not set");

    c = ungetc((int)(FLISP_ARG_ONE->integer), fd);
    if (c == EOF)
        exception(interp, io_error, "(fungetc char [ stream]) - ungetc() failed");

    return newInteger(interp, FLISP_ARG_ONE->integer);
}
/** (fgets[ stream]) - read a line or up to INPUT_FMT_BUFSIZ from stream or input
 *
 * @param stream  stream to read from. If not give use the input stream.
 *
 * @returns The string read from stream or end-of-file if no input is
 *          available. If a line is read it includes the trailing \n.
 *
 * @throws
 * - invalid-value   If stream is already closed.
 * - out-of-memory   If the input buffer cannot be allocated.
 * - io-error        If fgets() failed.
 */
Object *primitiveFgets(Interpreter *interp, Object** args, Object **env)
{
    Object *string = nil;
    char *input;
    FILE *fd = interp->input.fd;

    if (FLISP_HAS_ARGS) {
        CHECK_TYPE(FLISP_ARG_ONE, type_stream, "(fgets[ stream] - stream)");
        if (FLISP_ARG_ONE->fd == NULL)
            exception(interp, invalid_value, "(fgets[ stream]) - stream already closed");
        fd = FLISP_ARG_ONE->fd;
    }
    input = malloc(INPUT_FMT_BUFSIZ);
    if(input == NULL)
        exception(interp, out_of_memory, "fgets() failed, %s", strerror(errno));

    *input = '\0';

    if(fgets(input, INPUT_FMT_BUFSIZ, fd) != NULL) {
        string = newString(interp, input);
        free(input);
        return string;
    }
    free(input);
    if (!feof(fd))
        exceptionWithObject(interp, FLISP_ARG_ONE, io_error, "fgets() failed: %s", strerror(errno));
    return end_of_file;
}
/** (fstat path[ linkp]) - get  information about file
 *
 * @param path   String containing the path to the file to query.
 * @param linkp  If given and not null do not follow the symbolic link
 *               if path is one, return the link information instead.
 *
 * @returns A property list with size, mode uid and gid as integer, type as character:
 * - b  block device
 * - c  character device
 * - d  directory
 * - p  fifo
 * - f  regular file
 * - l  symbolic link
 * - s  socket
 * - -  unkown file type
 *
 * @trhows
 * - permission-denied
 * - not-found
 * - invalid-value      if path is to long.
 * - io-error
 */
Object *primitiveFstat(Interpreter *interp, Object** args, Object **env)
{
    struct stat info;
    int result;
    Object *object;
    char *type;

    CHECK_TYPE(FLISP_ARG_ONE, type_string,  "(fstat path[ linkp]) - stream");

    if (FLISP_HAS_ARG_TWO && FLISP_ARG_TWO != nil)
        result = lstat(FLISP_ARG_ONE->string, &info);
    else
        result = stat(FLISP_ARG_ONE->string, &info);

    if (result == -1) {
        switch(errno) {
        case EACCES:
            exceptionWithObject(interp, FLISP_ARG_ONE, permission_denied, "(fstat path[ linkp]): %s", strerror(errno));
            break;
        case ENOENT:
        case ENOTDIR:
            exceptionWithObject(interp, FLISP_ARG_ONE, not_found, "(fstat path[ linkp]): %s", strerror(errno));
            break;
        case ENAMETOOLONG:
            exceptionWithObject(interp, FLISP_ARG_ONE, invalid_value, "(fstat path[ linkp]): %s", strerror(errno));
        }
        exceptionWithObject(interp, FLISP_ARG_ONE, io_error, "(fstat path[ linkp]): l/stat() failed: %s", strerror(errno));
    }

    /* (size _size_ type _type_ mode _mode_ uid _uid_ gid _gid_ ) */
    GC_CHECKPOINT;
    if      (S_ISBLK(info.st_mode)) type = "b";
    else if (S_ISCHR(info.st_mode)) type = "c";
    else if (S_ISDIR(info.st_mode)) type = "d";
    else if (S_ISFIFO(info.st_mode)) type = "p";
    else if (S_ISREG(info.st_mode)) type = "f";
    else if (S_ISLNK(info.st_mode)) type = "l";
    else if (S_ISSOCK(info.st_mode)) type = "s";
#if 0
    /* This works with muslc, but not gnu libc */
    else if (S_TYPEISMQ(info)) type = "Q";
    else if (S_TYPEISSEM(info)) type = "S";
    else if (S_TYPEISSHM(info)) type = "M";
    else if (S_TYPEISTMO(info)) type = "T";
#endif
    else type = "-";

    object = newString(interp, type);
    GC_TRACE(gcObject, object);
    object = newCons(interp, gcObject, &nil);
    GC_TRACE(gcResult, object);

    *gcObject = newSymbol(interp, "type");
    *gcResult = newCons(interp, gcObject, gcResult);

    *gcObject = newInteger(interp, info.st_gid);
    *gcResult = newCons(interp, gcObject, gcResult);

    *gcObject = newSymbol(interp, "gid");
    *gcResult = newCons(interp, gcObject, gcResult);

    *gcObject = newInteger(interp, info.st_uid);
    *gcResult = newCons(interp, gcObject, gcResult);

    *gcObject = newSymbol(interp, "uid");
    *gcResult = newCons(interp, gcObject, gcResult);

    *gcObject = newInteger(interp, info.st_mode & 07777);
    *gcResult = newCons(interp, gcObject, gcResult);

    *gcObject = newSymbol(interp, "mode");
    *gcResult = newCons(interp, gcObject, gcResult);

    *gcObject = newInteger(interp, info.st_size);
    *gcResult = newCons(interp, gcObject, gcResult);

    *gcObject = newSymbol(interp, "size");
    *gcResult = newCons(interp, gcObject, gcResult);

    GC_RETURN(*gcResult);
}
/** (fttyp[ fd]) - check if input or stream has a tty
 *
 * @param fd  stream
 *
 * @returns t if fd is associated with a tty.
 *
 */
Object *primitiveFttyP(Interpreter *interp, Object** args, Object **env)
{
    FILE* fd = interp->input.fd;
    if (FLISP_HAS_ARGS)
        fd = FLISP_ARG_ONE->fd;
    return (isatty(fileno(fd))) ? t : nil;
}
/** (fmkdir path[ mode]) - create directory
 *
 * @param path   String,  directory to create.
 * @param mode   Integer, mode for creating the directory, 0775 if not given.
 *
 * @returns t on success
 *
 * @throws
 * - invalid-value  If path is too long, a component of path is not an
 *                  existing directory or path is the empty string.
 * - permission-denied  If search or write permission is denied.
 * - file-exists    If the directory already exists.
 * - io-error
 *
 */
Object *primitiveMkdir(Interpreter *interp, Object** args, Object **env)
{
    mode_t mode = S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH;
    CHECK_TYPE(FLISP_ARG_ONE, type_string,  "(fmkdir path[ mode) - path");
    if (FLISP_HAS_ARG_TWO) {
        CHECK_TYPE(FLISP_ARG_TWO, type_integer,  "(fmkdir path[ mode) - mode");
        mode = FLISP_ARG_TWO->integer;
    }
    if (mkdir(FLISP_ARG_ONE->string, mode) == -1) {
        switch(errno) {
        case EACCES:
        case EROFS:
            exceptionWithObject(interp, FLISP_ARG_ONE, permission_denied,
                                "(fmkdir path[ mode]): %s", strerror(errno));
        case EEXIST:
            exceptionWithObject(interp, FLISP_ARG_ONE, file_exists,
                                "(fmkdir path[ mode]): %s", strerror(errno));
        case ENAMETOOLONG:
        case ENOENT:
        case ENOTDIR:
            exceptionWithObject(interp, FLISP_ARG_ONE, invalid_value,
                                "(fmkdir path[ mode]): %s", strerror(errno));
        }
        exceptionWithObject(interp, FLISP_ARG_ONE, io_error,
                            "(fmkdir path[ mode]): %s", strerror(errno));
    }
    return t;
}
/** (popen line[ mode]) - run command line and read from/write to it
 *
 * @param line  String containing a command line to be run by the
 *              system shell.
 * @param mode  "r" for reading from the standard output of the
 *              command. "w" for writing to the standard input of the
 *              command. If not given defaults to "r".
 *
 * @returns A stream object to read from/write to.
 *
 * @trows
 * - invalid-value  if mode is not "r" or "w".
 * - io-error
 *
 * Note: the stream must be closed with (pclose), it is an error to
 * use (fclose) on (popen) streams.
 */
Object *primitivePopen(Interpreter *interp, Object** args, Object **env)
{
    FILE *fd;
    char *mode = "r";

    if(FLISP_HAS_ARG_TWO) {
        if (strcmp(FLISP_ARG_TWO->string, "r") && strcmp(FLISP_ARG_TWO->string, "w"))
            exception(interp, invalid_value,
                      "(popen path[ mode]) - mode must be \"r\" or \"w\", got: %s",
                      FLISP_ARG_TWO->string);
        mode = FLISP_ARG_TWO->string;
    }

    fd = popen(FLISP_ARG_ONE->string, mode);
    if (fd == NULL)
        exception(interp, io_error, "(popen path[ mode]) - popen() failed: %s", strerror(errno));

    return newStreamObject(interp, fd, FLISP_ARG_ONE->string);
}
/** (pclose stream) - close a stream opened with popen
 *
 * @param stream  Stream to close. Must be a stream opened with
 *                (popen).
 *
 * @returns The exit status of the command.
 *
 * @throws io-error if pclose() failed.
 */
Object *primitivePclose(Interpreter *interp, Object** args, Object **env)
{
    int result = pclose(FLISP_ARG_ONE->fd);

    if (result == -1)
        exceptionWithObject(interp, FLISP_ARG_ONE, io_error, "pclose() failed: %s", strerror(errno));

    return newInteger(interp, result);
}

/* OS interface */

/** (system s) ⇒ i: run a command line in the system shell
 *
 * @param s .. Command line
 *
 * @returns The exit code of the shell.
 */
Object *fl_system(Interpreter *interp, Object **args, Object **env)
{
    return newInteger(interp, system(FLISP_ARG_ONE->string));
}

/** (getenv name) ⇒ value: get value of environment variable 
 *
 * @param name .. Name of environment variable
 *
 * @returns *value* of environment variable *name* as string or `nil`
 *          if *name* does not exit.
 */
Object *fl_getenv(Interpreter *interp, Object **args, Object **env)
{
    char *e = getenv(FLISP_ARG_ONE->string);
    if (e == NULL) return nil;
    return newStringWithLength(interp, e, strlen(e));
}


Primitive flisp_file_primitives[] = {
    {"fflush",    0, 1, 0,           primitiveFflush},
    {"fseek",     2, 3, 0,           primitiveFseek},
    {"ftell",     0, 1, TYPE_STREAM, primitiveFtell},
    {"feof",      0, 1, TYPE_STREAM, primitiveFeof},
    {"fgetc",     0, 1, 0,           primitiveFgetc},
    {"fungetc",   1, 2, 0,           primitiveFungetc},
    {"fgets",     0, 1, 0,           primitiveFgets},
    {"fstat",     1, 2, 0,           primitiveFstat},
    {"fttyp",     0, 1, TYPE_STREAM, primitiveFttyP},
    {"fmkdir",    1, 2, 0,           primitiveMkdir},
    {"popen",     1, 2, TYPE_STRING, primitivePopen},
    {"pclose",    1, 1, TYPE_STREAM, primitivePclose},
    {"system",    1, 1, TYPE_STRING, fl_system},
    {"getenv",    1, 1, TYPE_STRING, fl_getenv},
};


/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
