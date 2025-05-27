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
    ONE_STREAM_ARG(fflush);
    if (stream->fd == NULL)
        exception(interp, FLISP_INVALID_VALUE, "(fflush stream) - stream already closed");
    return newNumber(interp, file_fflush(interp, stream));
}

off_t file_ftell(Interpreter *interp, Object *stream)
{
    return ftello(stream->fd);
}
Object *primitiveFtell(Interpreter *interp, Object** args, Object **env)
{
    ONE_STREAM_ARG(ftell);
    if (stream->fd == NULL)
        exception(interp, FLISP_INVALID_VALUE, "(ftell stream) - stream already closed");
    return newNumber(interp, file_ftell(interp, stream));
}

Object *primitiveFgetc(Interpreter *interp, Object** args, Object **env)
{
    char s[] = "\0\0";
    ONE_STREAM_ARG(getc);

    int c = getc(stream->fd);
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
