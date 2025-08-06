/************************* Editor Extensions **************************************/


#define F_NONE          0
#define F_CLEAR         1

#define DEFINE_EDITOR_FUNC(name)					\
  extern void name();							\
    Object *e_##name(Interpreter *interp, Object ** args, Object **env)	\
    {									\
      name();								\
      return t;								\
    }

DEFINE_EDITOR_FUNC(beginning_of_buffer)
DEFINE_EDITOR_FUNC(end_of_buffer)
DEFINE_EDITOR_FUNC(left)
DEFINE_EDITOR_FUNC(right)
DEFINE_EDITOR_FUNC(forward_word)
DEFINE_EDITOR_FUNC(backward_word)
DEFINE_EDITOR_FUNC(up)
DEFINE_EDITOR_FUNC(down)
DEFINE_EDITOR_FUNC(lnbegin)
DEFINE_EDITOR_FUNC(lnend)
DEFINE_EDITOR_FUNC(yank)
DEFINE_EDITOR_FUNC(update_display)
DEFINE_EDITOR_FUNC(clear_message_line)
DEFINE_EDITOR_FUNC(copy_region)
DEFINE_EDITOR_FUNC(set_mark)
DEFINE_EDITOR_FUNC(kill_region)
DEFINE_EDITOR_FUNC(delete)
DEFINE_EDITOR_FUNC(backspace)
DEFINE_EDITOR_FUNC(forward_page)
DEFINE_EDITOR_FUNC(backward_page)
DEFINE_EDITOR_FUNC(suspend)
DEFINE_EDITOR_FUNC(quit)
DEFINE_EDITOR_FUNC(eval_block)
DEFINE_EDITOR_FUNC(delete_other_windows)
DEFINE_EDITOR_FUNC(list_buffers)
DEFINE_EDITOR_FUNC(describe_bindings)
DEFINE_EDITOR_FUNC(describe_functions)
DEFINE_EDITOR_FUNC(split_window)
DEFINE_EDITOR_FUNC(other_window)
DEFINE_EDITOR_FUNC(execute_key)

extern int set_key(char *, char *);
extern int getinput(char *, char *, int, int);
extern char *get_char(void);
extern char *get_input_key(void);
extern char *get_key_name(void);
extern char *get_key_funcname(void);
extern char *get_clipboard(void);
extern char *get_current_bufname(void);
extern char *get_current_filename(void);
extern void set_scrap(unsigned char *);
extern void execute_key(void);
extern int select_buffer(char *);
extern int delete_buffer_byname(char *);
extern int add_mode_current_buffer(char *);
extern int delete_mode_current_buffer(char *);
extern int save_buffer_byname(char *);
extern int count_buffers(void);
extern void display_prompt_and_response(char *, char *);
extern void msg(char *,...);
extern void clear_message_line(void);
extern void log_message(char *);
extern void insert_string(char *);
extern void move_to_search_result(point_t);
extern point_t search_forward(char *);
extern point_t search_backwards(char *);
extern void readfile(char *);
extern void shell_command(char *);
extern int goto_line(int);
extern int add_mode_global(char *);
extern char *get_version_string(void);
extern char *get_temp_file(void);

Object *e_get_char(Interpreter *interp, Object **args, Object **env) { return newStringWithLength(interp, get_char(), 1); }
Object *e_get_key(Interpreter *interp, Object **args, Object **env) { return newString(interp, get_input_key()); }
Object *e_get_key_name(Interpreter *interp, Object **args, Object **env) { return newString(interp, get_key_name()); }
Object *e_get_key_funcname(Interpreter *interp, Object **args, Object **env) { return newString(interp, get_key_funcname()); }
Object *e_get_clipboard(Interpreter *interp, Object **args, Object **env) { return newString(interp, get_clipboard()); }
Object *e_get_buffer_count(Interpreter *interp, Object **args, Object **env) { return newNumber(interp, count_buffers()); }

Object *e_refresh(Interpreter *interp, Object ** args, Object **env)
{
    refresh();
    return t;
}

Object *e_set_key(Interpreter *interp, Object **args, Object **env)
{
    TWO_STRING_ARGS(set-key);
    return (1 == set_key(first->string, second->string) ? t : nil);
}

Object *e_add_mode_global(Interpreter *interp, Object **args, Object **env)
{
    ONE_STRING_ARG(add-mode-global);
    return (1 == add_mode_global(arg->string) ? t : nil);
}

Object *e_add_mode(Interpreter *interp, Object **args, Object **env)
{
    ONE_STRING_ARG(add-mode);
    return (1 == add_mode_current_buffer(arg->string) ? t : nil);
}

Object *e_delete_mode(Interpreter *interp, Object **args, Object **env)
{
    ONE_STRING_ARG(delete-mode);
    return (1 == delete_mode_current_buffer(arg->string) ? t : nil);
}

Object *e_set_clipboard(Interpreter *interp, Object **args, Object **env)
{
    ONE_STRING_ARG(set-clipboard);

    /* gets freed by next call to set_clipboard */
    char *sub = strdup(arg->string);
    set_scrap((unsigned char *)sub);
    return t;
}

Object *e_get_temp_file(Interpreter *interp, Object **args, Object **env)
{
    char *fn = get_temp_file();
    return newStringWithLength(interp, fn, strlen(fn));
}

Object *e_insert_file(Interpreter *interp, Object **args, Object **env) {

    // Note: want to give an optional modify flag, but then it segfaults
    int mflag;

    ONE_STRING_ARG(insert-file);

//    mflag = (arg->cdr != nil && arg->cdr->car != nil);
    mflag = FALSE;

    return ((insert_file(arg->string, mflag) == TRUE) ? t : nil);
}

Object *e_getfilename(Interpreter *interp, Object **args, Object **env) {

    ONE_STRING_ARG(prompt-filename);

    if (FALSE == getfilename(arg->string, (char*) response_buf, NAME_MAX))
	return nil;

    return newString(interp, response_buf);
}

Object *e_show_prompt(Interpreter *interp, Object **args, Object **env)
{
    TWO_STRING_ARGS(show-prompt);
    display_prompt_and_response(first->string, second->string);
    return t;
}

Object *e_prompt(Interpreter *interp, Object **args, Object **env)
{
    TWO_STRING_ARGS(prompt);

    char response[81];
    strncpy(response, second->string, 80);
    response[80] = '\0';

    (void) ! getinput(first->string, response, 80, F_NONE);
    return newStringWithLength(interp, response, strlen(response));
}

Object *e_get_version_string(Interpreter *interp, Object **args, Object **env)
{
    char *ver = get_version_string();
    return newStringWithLength(interp, ver, strlen(ver));
}

Object *e_goto_line(Interpreter *interp, Object **args, Object **env)
{
    Object *arg = (*args)->car;

    ONE_NUMBER_ARG(goto-line);

    int result = goto_line(arg->number);
    return (result == 1 ? t : nil);
}

Object *e_select_buffer(Interpreter *interp, Object **args, Object **env)
{
    ONE_STRING_ARG(select-buffer);
    // Note: select buffer always returns TRUE
    //  so it seems to be superfluous to test for the return value
    int result = select_buffer(arg->string);
    return (result ? t : nil);
}

Object *e_rename_buffer(Interpreter *interp, Object **args, Object **env)
{
    ONE_STRING_ARG(rename-buffer);
    rename_current_buffer(arg->string);
    char *bname = get_current_bufname();
    return newStringWithLength(interp, bname, strlen(bname));
}

Object *e_save_buffer(Interpreter *interp, Object **args, Object **env)
{
    ONE_STRING_ARG(save-buffer);
    int result = save_buffer_byname(arg->string);
    return (result ? t : nil);
}

Object *e_kill_buffer(Interpreter *interp, Object **args, Object **env)
{
    ONE_STRING_ARG(kill-buffer);
    int result = delete_buffer_byname(arg->string);
    return (result ? t : nil);
}

Object *e_zero_buffer(Interpreter *interp, Object **args, Object **env)
{
    assert(curbp != NULL);
    zero_buffer(curbp);
    return nil;
}

Object *e_find_file(Interpreter *interp, Object **args, Object **env)
{
    ONE_STRING_ARG(find-file);
    readfile(arg->string);
    return t;
}

Object *e_search_forward(Interpreter *interp, Object **args, Object **env)
{
    ONE_STRING_ARG(search-forward);
    point_t founded = search_forward(arg->string);
    move_to_search_result(founded);
    return (founded == -1 ? nil : t);
}

Object *e_search_backward(Interpreter *interp, Object **args, Object **env)
{
    ONE_STRING_ARG(search-backward);
    point_t founded = search_backwards(arg->string);
    move_to_search_result(founded);
    return (founded == -1 ? nil : t);
}

Object *e_getch(Interpreter *interp, Object **args, Object **env)
{
    char ch[2];
    ch[0] = (unsigned char)getch();
    ch[1] = '\0';
    return newStringWithLength(interp, ch, 1);
}

Object *e_get_buffer_name(Interpreter *interp, Object **args, Object **env)
{
    char buf[40];
    strcpy(buf, get_current_bufname());
    return newStringWithLength(interp, buf, strlen(buf));
}

Object *e_get_buffer_filename(Interpreter *interp, Object **args, Object **env)
{
    char buf[128];
    strcpy(buf, get_current_filename());
    return newStringWithLength(interp, buf, strlen(buf));
}

Object *e_get_buffer_file_extension(Interpreter *interp, Object **args, Object **env)
{
    char buf[20];
    strcpy(buf, get_current_file_extension());
    return newStringWithLength(interp, buf, strlen(buf));
}

Object *e_message(Interpreter *interp, Object **args, Object **env)
{
    ONE_STRING_ARG(message);
    msg(arg->string);
    return t;
}

Object *e_log_message(Interpreter *interp, Object **args, Object **env)
{
    ONE_STRING_ARG(log-message);
    log_message(arg->string);
    return t;
}

Object *e_log_debug(Interpreter *interp, Object **args, Object **env)
{
    ONE_STRING_ARG(log-debug);
    fl_debug(interp, "%s", arg->string);
    return t;
}

Object *e_insert_string(Interpreter *interp, Object **args, Object **env)
{
    ONE_STRING_ARG(insert-string);
    insert_string(arg->string);
    return t;
}

extern void set_point(point_t);
extern point_t get_mark(void);
extern point_t get_point(void);
extern point_t get_point_max(void);

Object *e_set_point(Interpreter *interp, Object **args, Object **env)
{
    ONE_NUMBER_ARG(set-point)
    set_point(num->number);
    return t;
}

Object *e_get_mark(Interpreter *interp, Object **args, Object **env)
{
    return newNumber(interp, get_mark());
}

Object *e_get_point(Interpreter *interp, Object **args, Object **env)
{
    return newNumber(interp, get_point());
}

Object *e_get_point_max(Interpreter *interp, Object **args, Object **env)
{
    return newNumber(interp, get_point_max());
}
