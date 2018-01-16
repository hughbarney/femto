/*
 * key.c, femto, Hugh Barney, Public Domain, 2017
 * Derived from: Anthony's Editor January 93, (Public Domain 1991, 1993 by Anthony Howe)
 */

#include "header.h"

keymap_t *new_key(char *name, char *bytes)
{
	keymap_t *kp = (keymap_t *)malloc(sizeof(keymap_t));
	assert(kp != NULL);

 	strncpy(kp->k_name, name, MAX_KNAME);
	strncpy(kp->k_bytes, bytes, MAX_KBYTES);
	kp->k_name[MAX_KNAME] ='\0';
	kp->k_bytes[MAX_KBYTES] ='\0';
	kp->k_func = user_func;
	strcpy(kp->k_funcname, E_NOT_BOUND);
	kp->k_next = NULL;
	return kp;
}

/* note, no check if name already exists */
void make_key(char *name, char *bytes)
{
	keymap_t *kp = new_key(name, bytes);
	ktail->k_next = kp;
	ktail = kp;
}

void create_keys()
{
	char ch;
	char ctrx_map1[] = "c-x c-|";
	char ctrx_map2[] = "c-x x";
	char ctrc_map1[] = "c-c c-|";
	char ctrc_map2[] = "c-c x";
	char ctrl_map[] = "c-|";
	char esc_map[] = "esc-|";
	char ctrx_bytes[] = "\x18\x01";
	char ctrc_bytes[] = "\x03\x01";
	char ctrl_bytes[] = "\x01";
	char esc_bytes[] = "\x1B\x61";

	assert(khead == NULL);
	khead = ktail = new_key("c-space", "\x00");

	/* control-a to z */
	for (ch = 1; ch <= 26; ch++) {
		if (ch == 3 || ch == 9 || ch == 10 || ch == 24) continue;  /* skip c-c, tab, linefeed, ctrl-x */
		ctrl_map[2] = ch + 96;   /* ASCII a is 97 */
		ctrl_bytes[0] = ch;
		make_key(ctrl_map, ctrl_bytes);
	}

	/* esc-a to z */
	for (ch = 1; ch <= 26; ch++) {
		esc_map[4] = ch + 96;
		esc_bytes[1] = ch + 96;
		make_key(esc_map, esc_bytes);
	}

	/* control-x control-a to z */
	for (ch = 1; ch <= 26; ch++) {
		ctrx_map1[6] = ch + 96;
		ctrx_bytes[1] = ch;
		make_key(ctrx_map1, ctrx_bytes);
	}

	/* control-x a to z */
	for (ch = 1; ch <= 26; ch++) {
		ctrx_map2[4] = ch + 96;
		ctrx_bytes[1] = ch + 96;
		make_key(ctrx_map2, ctrx_bytes);
	}	

	/* control-c control-a to z */
	for (ch = 1; ch <= 26; ch++) {
		ctrc_map1[6] = ch + 96;
		ctrc_bytes[1] = ch;
		make_key(ctrc_map1, ctrc_bytes);
	}

	/* control-c a to z */
	for (ch = 1; ch <= 26; ch++) {
		ctrc_map2[4] = ch + 96;
		ctrc_bytes[1] = ch + 96;
		make_key(ctrc_map2, ctrc_bytes);
	}
}

int set_key_internal(char *name, char *funcname, char *bytes, void (*func)(void))
{
	keymap_t *kp;
	
	/* check if we have an existing key, if so update it */
	for (kp = khead; kp->k_next != NULL; kp = kp->k_next) {
		if (0 == strcmp(kp->k_name, name)) {
			strncpy(kp->k_funcname, funcname, MAX_KFUNC);
			kp->k_funcname[MAX_KFUNC] ='\0';
			if (func != NULL)  /* dont set if its a user_func */
				kp->k_func = func;
			else
				kp->k_func = user_func;
			if (strcmp("user_func", funcname) != 0)
				(void)register_command(funcname, func);
			return 1;
		}
	}

	/* not found, create it and add onto the tail */
	kp = new_key(name, bytes);
	strncpy(kp->k_funcname, funcname, MAX_KFUNC);
	kp->k_funcname[MAX_KFUNC] ='\0';
	kp->k_func = func;
	ktail->k_next = kp;
	ktail = kp;
	if (strcmp("user_func", funcname) != 0)
		(void)register_command(funcname, func);
	return 1;
}

int set_key(char *name, char *funcname)
{
	return set_key_internal(name, funcname, "", NULL);
}

void setup_keys()
{
	create_keys();
        set_key_internal("c-a",     "beginning-of-line"     , "\x01", lnbegin);
	set_key_internal("c-b",     "backward-char"         , "\x02", left);
	set_key_internal("c-d",     "delete"                , "\x04", delete);
	set_key_internal("c-e",     "end-of-line"           , "\x05", lnend);
	set_key_internal("c-f",     "forward-char"          , "\x06", right);
	set_key_internal("c-n",     "next-line"             , "\x0E", down);
	set_key_internal("c-p",     "previous-line"         , "\x10", up);
	set_key_internal("c-h",     "backspace"             , "\x08", backspace);
	set_key_internal("c-k",     "kill-to-eol"           , "\x0B", user_func);
	set_key_internal("c-l",     "refresh"               , "\x0C", redraw);
	set_key_internal("c-n",     "next-line"             , "\x0E", down);
	set_key_internal("c-p",     "previous-line"         , "\x10", up);
	set_key_internal("c-r",     "search-backward"       , "\x12", search);
	set_key_internal("c-s",     "search-forward"        , "\x13", search);
	set_key_internal("c-u",     "undo"                  , "\x15", undo_command);
	set_key_internal("c-v",     "forward-page"          , "\x16", forward_page);
	set_key_internal("c-w",     "kill-region"           , "\x17", kill_region);
	set_key_internal("c-y",     "yank"                  , "\x19", yank);

        set_key_internal("esc-a",   "apropos"               , "\x1B\x61", apropos);
	set_key_internal("esc-b",   "backward-word"         , "\x1B\x62", backward_word);
	set_key_internal("esc-c",   "copy-region"           , "\x1B\x63", copy_region);
	set_key_internal("esc-d",   "kill-to-eol"           , "\x1B\x64", user_func);
	set_key_internal("esc-f",   "forward-word"          , "\x1B\x66", forward_word);
	set_key_internal("esc-g",   "goto-line"             , "\x1B\x67", i_gotoline);
	set_key_internal("esc-i",   "yank"                  , "\x1B\x69", yank);
	set_key_internal("esc-k",   "kill-region"           , "\x1B\x6B", kill_region);
	set_key_internal("esc-l",   "describe-bindings"     , "\x1B\x6C", describe_bindings);
	set_key_internal("esc-m",   "set-mark"              , "\x1B\x6D", i_set_mark);
	set_key_internal("esc-n",   "next-buffer"           , "\x1B\x6E", next_buffer);
	set_key_internal("esc-o",   "delete-other-windows"  , "\x1B\x6F", delete_other_windows);
	set_key_internal("esc-r",   "query-replace"         , "\x1B\x72", query_replace);
	set_key_internal("esc-v",   "page-up"               , "\x1B\x76", backward_page);
	set_key_internal("esc-w",   "copy-region"           , "\x1B\x77", copy_region);
	set_key_internal("esc-x",   "execute-command"       , "\x1B\x78", execute_command);

	set_key_internal("esc-up",    "beginning-of-buffer" , "\x1B\x1B\x5B\x41", beginning_of_buffer);	
	set_key_internal("esc-down",  "end-of-buffer"       , "\x1B\x1B\x5B\x42", end_of_buffer);
	set_key_internal("esc-right", "user-func"           , "\x1B\x1B\x5B\x43", user_func);
	set_key_internal("esc-left",  "user-func"           , "\x1B\x1B\x5B\x44", user_func);
	set_key_internal("esc-end",   "end-of-buffer"       , "\x1B\x1B\x4F\x46", end_of_buffer);
	set_key_internal("esc-home",  "beginning-of-buffer" , "\x1B\x1B\x4F\x48", beginning_of_buffer);
	set_key_internal("esc-@",     "set-mark"            , "\x1B\x40", i_set_mark);
	set_key_internal("esc-<",     "beginning-of-buffer" , "\x1B\x3C", beginning_of_buffer);
	set_key_internal("esc->",     "end-of-buffer"       , "\x1B\x3E", end_of_buffer);
	set_key_internal("esc-]",     "eval-block"          , "\x1B\x5D", eval_block);
	set_key_internal("esc-;",     "exec-lisp-command"   , "\x1B\x3B", repl);
	set_key_internal("esc-.",     "user-func"           , "\x1B\x2E", user_func);

	set_key_internal("up ",       "previous-line",        "\x1B\x5B\x41", up);
	set_key_internal("down",      "next-line",            "\x1B\x5B\x42", down);
	set_key_internal("left",      "backward-char",        "\x1B\x5B\x44", left);
	set_key_internal("right",     "forward-char",         "\x1B\x5B\x43", right);
	set_key_internal("home",      "beginning-of-line",    "\x1B\x4F\x48", lnbegin);
	set_key_internal("end",       "end-of-line",          "\x1B\x4F\x46", lnend);
	set_key_internal("del",       "delete",               "\x1B\x5B\x33\x7E", delete);
	set_key_internal("ins",       "toggle-overwrite-mode" , "\x1B\x5B\x32\x7E", toggle_overwrite_mode);
	set_key_internal("pgup",      "page-up",              "\x1B\x5B\x35\x7E", backward_page);
	set_key_internal("pgdn",      "page-down",            "\x1B\x5B\x36\x7E", forward_page);
	set_key_internal("backspace", "backspace",            "\x7f", backspace);

	set_key_internal("c-x c-c",   "exit"                  , "\x18\x03", quit_ask);
	set_key_internal("c-x c-f",   "find-file"             , "\x18\x06", i_readfile);  
	set_key_internal("c-x c-n",   "next-buffer"           , "\x18\x0E", next_buffer);
	set_key_internal("c-x c-s",   "save-buffer"           , "\x18\x13", savebuffer);  
	set_key_internal("c-x c-w",   "write-file"            , "\x18\x17", writefile);
	set_key_internal("c-x 1",     "delete-other-windows"  , "\x18\x31", delete_other_windows);
	set_key_internal("c-x 2",     "split-window"          , "\x18\x32", split_window);
	set_key_internal("c-x =",     "cursor-position"       , "\x18\x3D", cursor_position);
	set_key_internal("c-x ?",     "user-func"             , "\x18\x3F", user_func);
	set_key_internal("c-x b",     "list-buffers"          , "\x18\x62", list_buffers);
	set_key_internal("c-x i",     "insert-file"           , "\x18\x69", insertfile);
	set_key_internal("c-x k",     "kill-buffer"           , "\x18\x6B", kill_buffer);
	set_key_internal("c-x n",     "next-buffer"           , "\x18\x6E", next_buffer);
	set_key_internal("c-x o",     "other-window"          , "\x18\x6F", other_window);
	set_key_internal("c-x @",     "shell-command"         , "\x18\x40", i_shell_command);
	set_key_internal("c-x (",     "user-func"             , "\x18\x28", user_func);
	set_key_internal("c-x )",     "user-func"             , "\x18\x29", user_func);
	set_key_internal("c-x `",     "user-func"             , "\x18\x60", user_func);
	set_key_internal("c-space",   "set-mark"              , "\x00", i_set_mark);
	set_key_internal("c-]",       "user-func"             , "\x1D", user_func);
	set_key_internal("resize",     "resize"               , "\x9A", resize_terminal);

	register_command("describe-functions", describe_functions);
	register_command("show-version", version);
}


char_t *get_key(keymap_t *keys, keymap_t **key_return)
{
	keymap_t *k;
	int submatch;
	static char_t buffer[K_BUFFER_LENGTH];
	static char_t *record = buffer;

	*key_return = NULL;

	/* if recorded bytes remain, return next recorded byte. */
	if (*record != '\0') {
		*key_return = NULL;
		return record++;
	}
	/* reset record buffer. */
	record = buffer;

	do {
		assert(K_BUFFER_LENGTH > record - buffer);
		/* read and record one byte. */
		*record++ = (unsigned)getch();
		*record = '\0';

		/* if recorded bytes match any multi-byte sequence... */
		for (k = keys, submatch = 0; k != NULL; k = k->k_next) {
			char_t *p, *q;

			if (k->k_func == NULL) continue;
			assert(k->k_bytes != NULL);

			for (p = buffer, q = (char_t *)k->k_bytes; *p == *q; ++p, ++q) {
			        /* an exact match */
				if (*q == '\0' && *p == '\0') {
	    				record = buffer;
					*record = '\0';
					*key_return = k;
					return record; /* empty string */
				}
			}
			/* record bytes match part of a command sequence */
			if (*p == '\0' && *q != '\0') {
				submatch = 1;
			}
		}
	} while (submatch);
	/* nothing matched, return recorded bytes. */
	record = buffer;
	return (record++);
}

/* wrapper to simplify call and dependancies in the interface code */
char *get_input_key()
{
	return (char *)get_key(khead, &key_return);
}

/* the name of the bound function of this key */
char *get_key_funcname()
{
	return (key_return != NULL ? key_return->k_funcname : "");
}

/* the name of the last key */
char *get_key_name()
{
	return (key_return != NULL ? key_return->k_name : "");
}

/* execute the function of the last bound key */
void execute_key()
{
	if (key_return != NULL)
		(key_return->k_func)();
}

int getinput(char *prompt, char *buf, int nbuf, int flag)
{
	int cpos = 0;
	int c;
	int start_col = strlen(prompt);

	mvaddstr(MSGLINE, 0, prompt);
	clrtoeol();

	if (flag == F_CLEAR) buf[0] = '\0';

	/* if we have a default value print it and go to end of it */
	if (buf[0] != '\0') {
		addstr(buf);
		cpos = strlen(buf);
	}

	for (;;) {
		refresh();
		c = getch();
		/* ignore control keys other than backspace, cr, lf */
		if (c < 32 && c != 0x07 && c != 0x08 && c != 0x0a && c != 0x0d)
			continue;

		switch(c) {
		case 0x0a: /* cr, lf */
		case 0x0d:
			buf[cpos] = '\0';
			return (cpos > 0 ? TRUE : FALSE);

		case 0x07: /* ctrl-g */
			buf[0] = '\0';
			return FALSE;

		case 0x7f: /* del, erase */
		case 0x08: /* backspace */
			if (cpos == 0)
				continue;

			move(MSGLINE, start_col + cpos - 1);
			addch(' ');
			move(MSGLINE, start_col + cpos - 1);
			buf[--cpos] = '\0';
			break;

		default:
			if (cpos < nbuf -1) {
				addch(c);
				buf[cpos++] = c;
				buf[cpos] ='\0';
			}
			break;
		}
	}
}
