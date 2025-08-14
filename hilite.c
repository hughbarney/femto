/* hlite.c, generic syntax hilighting, Atto Emacs, Hugh Barney, Public Domain, 2016 */

#include "header.h"

int state = ID_DEFAULT;
int next_state = ID_DEFAULT;
int skip_count = 0;

char_t get_at(buffer_t *bp, point_t pt)
{
    // Note: ptr get's it wrong here: when at the end
    //   Valgrind complains, the pointer is behind 0 bytes
    //   However somewhere else it is needed exactly this way
    //   otherwise we get an extra zero byte at the end of the buffer.
    return (*ptr(bp, pt));
}

static char_t symbols[] = "{}[]()!'£$%^&*-+=:;@~#<>,.?/\\|";

int is_symbol(char_t c)
{
    register char_t *p = symbols;

    for (p = symbols; *p != '\0'; p++)
        if (*p == c) return 1;
    return 0;
}

void set_parse_state(buffer_t *bp, point_t pt)
{
    register point_t po;

    state = ID_DEFAULT;
    next_state = ID_DEFAULT;
    skip_count = 0;

    for (po =0; po < pt; po++)
        parse_text(bp, po);
}

int parse_text(buffer_t *bp, point_t pt)
{
    if (skip_count-- > 0)
        return state;

    char_t c_now = get_at(bp, pt);
    char_t c_next = get_at(bp, pt + 1);
    char_t c_next_next = get_at(bp, pt + 2);

    state = next_state;
    int cmode = (bp->b_flags & B_CMODE ? 1 : 0);
    int lisp_mode = (bp->b_flags & B_LISP ? 1 : 0);
    int python_mode = (bp->b_flags & B_PYTHON ? 1 : 0);

    // detect assignment operator for python
    if (python_mode == 1 && state == ID_DEFAULT && c_now == '=' && c_next != '=')
	return (next_state = state = ID_ASSIGNMENT);

    // if in assignment state and now is whitespace , stay in assigment mode
    if (python_mode == 1 && state == ID_ASSIGNMENT  && (c_now == ' ' || c_now == '\t' || c_now == '\n'))
	return (next_state = state = ID_ASSIGNMENT);

    // double quoted string
    if (python_mode == 1 && state == ID_ASSIGNMENT  && c_now == '"' && c_next != '"')
        return (next_state = ID_DOUBLE_STRING);

    // single quoted string
    if (python_mode == 1 && state == ID_ASSIGNMENT  && c_now == '\'' && c_next != '\'')
        return (next_state = ID_SINGLE_STRING);

    // if in assignment state and now is not triple single/double quote
    if (python_mode == 1 && state == ID_ASSIGNMENT  
            && c_now != '\'' &&  c_next != '\'' && c_next_next !='\''
            && c_now != '\"' &&  c_next != '\"' && c_next_next !='\"')
	return (next_state = state = ID_DEFAULT);

    // C start of block comment 
    if (cmode == 1 && state == ID_DEFAULT && c_now == '/' && c_next == '*') {
        skip_count = 1;
        return (next_state = state = ID_BLOCK_COMMENT);
    }

    // C end of block comment
    if (cmode == 1 && state == ID_BLOCK_COMMENT && c_now == '*' && c_next == '/') {
        skip_count = 1;
        next_state = ID_DEFAULT;
        return ID_BLOCK_COMMENT;
    }

    // C line comment
    if (cmode == 1 && state == ID_DEFAULT && c_now == '/' && c_next == '/') {
        skip_count = 1;
        return (next_state = state = ID_LINE_COMMENT);
    }

    // Lisp line comment
    if (lisp_mode == 1 && state == ID_DEFAULT && c_now == ';') {
        skip_count = 1;
        return (next_state = state = ID_LINE_COMMENT);
    }

    // Python line comment
    if (python_mode == 1 && state == ID_DEFAULT && c_now == '#') {
        skip_count = 1;
        return (next_state = state = ID_LINE_COMMENT);
    }

    // Python, Lisp and C line comment end
    if ((python_mode == 1 || cmode == 1 || lisp_mode == 1) && state == ID_LINE_COMMENT && c_now == '\n')
        return (next_state = ID_DEFAULT);


    // the dreaded triple double quotes """ in python, we must handle these before single quote handling
    // DOCSTRING variant
     if (python_mode == 1 && state == ID_DEFAULT && c_now == '"' && c_next == '"' && c_next_next == '"') {
        skip_count = 2;
        return (next_state = state = ID_TRIPLE_DOUBLE_QUOTE);
     }

     if (python_mode == 1 && state == ID_TRIPLE_DOUBLE_QUOTE  && c_now == '"' && c_next == '"' && c_next_next == '"') {
        skip_count = 2;
        next_state = ID_DEFAULT;
        return ID_TRIPLE_DOUBLE_QUOTE;
     }

    // the dreaded triple double quotes """ in python, we must handle these before single quote handling
    // String Assignment variant
     if (python_mode == 1 && state == ID_ASSIGNMENT && c_now == '"' && c_next == '"' && c_next_next == '"') {
        skip_count = 2;
        return (next_state = state = ID_TRIPLE_DOUBLE_QUOTE_S);
     }

     if (python_mode == 1 && state == ID_TRIPLE_DOUBLE_QUOTE_S && c_now == '"' && c_next == '"' && c_next_next == '"') {
        skip_count = 2;
        next_state = ID_DEFAULT;
        return ID_TRIPLE_DOUBLE_QUOTE_S;
     }

    // the dreaded triple single quotes ''' in python, we must handle these before single quote handling
    // docstring variant
     if (python_mode == 1 && state == ID_DEFAULT && c_now == '\'' && c_next == '\'' && c_next_next == '\'') {
        skip_count = 2;
        return (next_state = state = ID_TRIPLE_SINGLE_QUOTE);
     }

     if (python_mode == 1 && state == ID_TRIPLE_SINGLE_QUOTE  && c_now == '\'' && c_next == '\'' && c_next_next == '\'') {
        skip_count = 2;
        next_state = ID_DEFAULT;
        return ID_TRIPLE_SINGLE_QUOTE;
     }

    // the dreaded triple single quotes ''' in python, we must handle these before single quote handling
    // assignment variant
     if (python_mode == 1 && state == ID_ASSIGNMENT && c_now == '\'' && c_next == '\'' && c_next_next == '\'') {
        skip_count = 2;
        return (next_state = state = ID_TRIPLE_SINGLE_QUOTE_S);
     }

     if (python_mode == 1 && state == ID_TRIPLE_SINGLE_QUOTE_S  && c_now == '\'' && c_next == '\'' && c_next_next == '\'') {
        skip_count = 2;
        next_state = ID_DEFAULT;
        return ID_TRIPLE_SINGLE_QUOTE_S;
     }


    // double quoted string
    if ((python_mode == 1 || cmode == 1) && state == ID_DEFAULT && c_now == '"')
        return (next_state = ID_DOUBLE_STRING);

    // escape inside double quoted string
    if ((python_mode == 1 || cmode == 1) && state == ID_DOUBLE_STRING && c_now == '\\') {
        skip_count = 1;
        return (next_state = ID_DOUBLE_STRING);
    }

    // end of double quoted string
    if ((python_mode == 1 || cmode == 1) && state == ID_DOUBLE_STRING && c_now == '"') {
        next_state = ID_DEFAULT;
        return ID_DOUBLE_STRING;
    }

    // single quote matching, dont want in lisp code
    if ((python_mode == 1 || cmode == 1) && state == ID_DEFAULT && c_now == '\'')
        return (next_state = ID_SINGLE_STRING);

    // escape inside single quote matching
    if ((python_mode == 1 || cmode == 1) && state == ID_SINGLE_STRING && c_now == '\\') {
        skip_count = 1;
        return (next_state = ID_SINGLE_STRING);
    }
    
    // end of single quote matching 
    if ((python_mode == 1 || cmode == 1) && state == ID_SINGLE_STRING && c_now == '\'') {
        next_state = ID_DEFAULT;
        return ID_SINGLE_STRING;
    }

    // general alphabet text, not attached to any mode    
    if (state != ID_DEFAULT)
        return (next_state = state);

    // digits, not activated by any mode    
    if (state == ID_DEFAULT && c_now >= '0' && c_now <= '9') {
        next_state = ID_DEFAULT;
        return (state = ID_DIGITS);
    }

    // symbols not attivated by any mode
    if (state == ID_DEFAULT && 1 == is_symbol(c_now)) {
        next_state = ID_DEFAULT;
        return (state = ID_SYMBOL);
    }

    return (next_state = state);
}

/*
 * Local Variables:
 * c-file-style: "k&r"
 * c-basic-offset: 4
 * indent-tabs-mode: nil
 * End:
 */
