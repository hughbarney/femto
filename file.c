#ifndef FILE_C
#define FILE_C

Object *primitiveFopen(Object ** args, GC_PARAM)
{
    TWO_STRING_ARGS(fopen);
    return file_fopen(interp, first->string, second->string);
}
Object *primitiveFclose(Object** args, GC_PARAM)
{
    ONE_STREAM_ARG(fclose);
    if (fd->fd == NULL)
        exception(interp, FLISP_INVALID_VALUE, "(fflush fd) - stream fd already closed");
    return newNumber(file_fclose(interp, fd), GC_ROOTS);
}
Object *primitiveFflush(Object** args, GC_PARAM)
{
    ONE_STREAM_ARG(fflush);
    if (fd->fd == NULL)
        exception(interp, FLISP_INVALID_VALUE, "(fflush fd) - stream fd already closed");
    return newNumber(file_fflush(interp, fd), GC_ROOTS);
}

off_t file_ftell(Interpreter *interp, Object *stream)
{
    return ftello(stream->fd);
}
Object *primitiveFtell(Object** args, GC_PARAM)
{
    ONE_STREAM_ARG(ftell);
    if (fd->fd == NULL)
        exception(interp, FLISP_INVALID_VALUE, "(ftell fd) - stream fd already closed");
    return newNumber(file_ftell(interp, fd), GC_ROOTS);
}

Object *primitiveFgetc(Object** args, GC_PARAM)
{
    char s[] = "\0\0";
    ONE_STREAM_ARG(getc);
    
    int c = getc(fd->fd);
    if (c == EOF)
	return nil;
    s[0] = (char)c;
    return newString(s, GC_ROOTS);
}
#endif


/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
