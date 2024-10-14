#ifndef FILE_C
#define FILE_C

Object *primitiveFgetc(Object** args, GC_PARAM)
{
    Object *first = (*args)->car;
    char s[] = "\0\0";

    if (first->type != TYPE_STREAM)
	exceptionWithObject(first, "not a stream");

    int c = getc(first->fd);
    if (c == EOF)
	return nil;
    s[0] = (char)c;
    return newString(s, GC_ROOTS);
}
#endif
