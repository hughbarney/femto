#ifndef FILE_C
#define FILE_C

/** file_fflush - flush output stream
 *
 * @param interp  fLisp interpreter
 * @param stream  open output stream
 *
 * returns: 0 on success, erno otherwise
 *
 * Public C Interface
 */
int file_fflush(Interpreter *interp, Object *stream)
{
    return (fflush(stream->fd) == EOF) ? errno : 0;
}
Object *primitiveFflush(Interpreter *interp, Object** args, Object **env)
{
    if (FLISP_ARG_ONE->fd == NULL)
        exception(interp, invalid_value, "(fflush stream) - stream already closed");
    return newInteger(interp, file_fflush(interp, FLISP_ARG_ONE));
}

off_t file_ftell(Interpreter *interp, Object *stream)
{
    return ftello(stream->fd);
}
Object *primitiveFtell(Interpreter *interp, Object** args, Object **env)
{
    if (FLISP_ARG_ONE->fd == NULL)
        exception(interp, invalid_value, "(ftell stream) - stream already closed");
    return newInteger(interp, file_ftell(interp, FLISP_ARG_ONE));
}

Object *primitiveFgetc(Interpreter *interp, Object** args, Object **env)
{
    char s[] = "\0\0";

    int c = getc(FLISP_ARG_ONE->fd);
    if (c == EOF)
        return nil;
    s[0] = (char)c;
    return newString(interp, s);
}
#endif


/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
