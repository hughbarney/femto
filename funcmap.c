/* funcmap.c, femto, Hugh Barney, Public Domain, 2017 */

#include "header.h"

void free_string_list(string_list_t *list)
{
	string_list_t *next;

	while (list) {
		next = list->next;
		free(list->string);
		free(list);
		list = next;
	}
}

int count_string_list(string_list_t *list)
{
	int count = 0;
	string_list_t *sl;

	if (list == NULL) return 0;

	for (sl = list; sl != NULL; sl = sl-> next)
		count++;
	return count;
}

command_t *register_command(char *cmd_name, void (*func)(void))
{
	command_t *cp = cheadp;
	command_t *cmdp;

	while (cp != NULL) {
		if (strcmp(cmd_name, cp->c_name) == 0) {
			//debug("existing cmd: %s\n", cmd_name);
			cp->c_func = func; /* update it */
			return cp;
		}
		cp = cp->c_next;
	}

	if ((cmdp = (command_t *) malloc (sizeof (command_t))) == NULL)
		return NULL;

	assert(cmdp != NULL);
	strcpy(cmdp->c_name, cmd_name);
	cmdp->c_func = func;
	cmdp->c_next = NULL;
	assert(func != NULL);

	/* find the place in the list to insert this command */
	if (cheadp == NULL) {
		cheadp = cmdp;
	} else if (strcmp(cheadp->c_name, cmd_name) > 0) {
		/* insert at the head of the list */
		cmdp->c_next = cheadp;
		cheadp = cmdp;
	} else {
		for (cp = cheadp; cp->c_next != NULL; cp = cp->c_next)
			if (strcmp (cp->c_next->c_name, cmd_name) > 0)
				break;
		/* and insert it */
		cmdp->c_next = cp->c_next;
		cp->c_next = cmdp;
	}
	//debug("register_cmd: %s\n", cmdp->c_name);
	return cmdp;
}

/*
 * match possible function names
 */
string_list_t *match_functions(const char *fname)
{
	command_t       *fn;
	string_list_t   *head, *sl;
	int		 len;
	int              count = 0;

	len = strlen(fname);
	head = NULL;

	for (fn = cheadp; fn->c_next != NULL; fn = fn->c_next) {
		if (memcmp(fname, fn->c_name, len) == 0) {
			if ((sl = malloc(sizeof(*sl))) == NULL) {
				free_string_list(head);
				return (NULL);
			}

			sl->string = strdup(fn->c_name);
			sl->next = head;
			head = sl;
			count++;
		}
	}
	return head;
}

/* translate from function name to function pointer */
void_func name_to_function(const char *fname)
{
	command_t *fn;

	for (fn = cheadp; fn->c_next !=NULL; fn = fn->c_next)
		if (strcmp(fn->c_name, fname) == 0)
			return fn->c_func;
	return NULL;
}


int match_string_position(string_list_t *list, int pos)
{
	string_list_t *sl;
	char ch;

	if (list == NULL)
		return FALSE;

	ch = list->string[pos];

	for (sl = list; sl != NULL; sl = sl->next) {
		if (sl->string[pos] != ch)
			return FALSE;
	}
	return TRUE;
}

int shortest_string_len(string_list_t *list)
{
	string_list_t *sl;
	int shortest = 100;
	int i = 0;

	if (list == NULL) return 0;

	for (sl = list; sl != NULL; sl = sl->next) {
		i = strlen(sl->string);
		if (i <= shortest)
			shortest = i;
	}
	return shortest;
}

char *shortest_common_string(string_list_t *list)
{
	static char str[60];
	static char empty_string[] = "";
	int pos;
	int len = shortest_string_len(list);

	if (len == 0) return empty_string;

	for (pos = 0; pos < len; pos++)
		if (match_string_position(list, pos) == FALSE)
			break;

	pos++;

	/* return pos chars from first string */
	safe_strncpy(str, list->string, pos);
	return str;
}

/* show commands that match a sub-string */
void apropos()
{
	buffer_t *bp;
	command_t *fn;
	keymap_t *ky;
	char bindlist[40];
	char apropos[STRBUF_M];

	if (0 == getinput(str_apropos, response_buf, STRBUF_M, F_CLEAR))
		return;

	bp = find_buffer(str_help_buf, TRUE);
	assert(bp != NULL);
	zero_buffer(bp);

	for (fn = cheadp; fn->c_next != NULL; fn = fn->c_next) {
		if (strstr(fn->c_name, response_buf) == NULL)
			continue;

		bindlist[0] = '\0';

		for (ky = khead; ky != NULL; ky = ky->k_next) {
			if (strcmp(fn->c_name, ky->k_funcname) == 0) {
				if (bindlist[0] != '\0')
					strcat(bindlist, ", ");

				strcat(bindlist, ky->k_name);

				/* place some limits on the number of keys bindings we will show per command */
				if (strlen(bindlist) > 30)
					break;
			}
		}

		sprintf(apropos, "%-30s %s\n", fn->c_name, bindlist);
		append_string(bp, apropos);
	}

	(void)popup_window(str_help_buf);
}

/* show all the key bindings in a buffer */
void describe_bindings()
{
	buffer_t *bp;
	keymap_t *ky;
	char binding[80];

	bp = find_buffer(str_help_buf, TRUE);
	assert(bp != NULL);
	zero_buffer(bp);
	
	for (ky = khead; ky != NULL; ky = ky->k_next) {
		sprintf(binding, "%-16s %s\n", ky->k_name, ky->k_funcname);
		append_string(bp, binding);
	}

	(void)popup_window(bp->b_bname);
}

/* show all registered functions in a buffer */
void describe_functions()
{
	buffer_t *bp;
	command_t *cp;
	char funcname[80];

	bp = find_buffer(str_help_buf, TRUE);
	assert(bp != NULL);
	zero_buffer(bp);

	for (cp = cheadp; cp != NULL; cp = cp->c_next) {
		sprintf(funcname, "%s\n", cp->c_name);
		append_string(bp, funcname);
	}

	(void)popup_window(bp->b_bname);
}

/* Esc-x execute command prompt */
void execute_command()
{
	buffer_t *bp = NULL;
	window_t *wp = NULL;
	string_list_t *cmd_list, *sl;
	void_func funct = NULL;

	int process_input = 1;
	int tab_count = 0;
	int column = 0;
	int cpos = 0;
	int ch;
	char command_name[STRBUF_M + 1];
	char prompt[] = "Execute command: ";
	char *shortest_match;

	command_name[0] = '\0';
	bp = find_buffer(str_completions, TRUE);
	assert(bp != NULL);

	display_prompt_and_response(prompt, command_name);
	cpos = strlen(command_name);

	while (process_input) {
		ch = getch();
		/* ignore control keys other than C-g, TAB, backspace, del, CR, ESC */
		if (ch < 32 && ch != 7 && ch != 9 && ch != 8 && ch != 13 && ch != 10 && ch != 27)
			continue;

		switch(ch) {
		case 27: /* esc */
			tab_count = 0;
			flushinp(); /* discard any escape sequence without writing in buffer */
			break;

		case 7: /* ctrl-g */
			command_name[0] = '\0';
			process_input = 0;
			break;

		case 13: /* CR, LF, only allow if there is 1 matched command waiting */
		case 10:
			cmd_list = match_functions(command_name);
			if (1 != count_string_list(cmd_list)) {
				free_string_list(cmd_list);
				continue;
			}

			free_string_list(cmd_list);
			process_input = 0;
			break;

		case 9: /* TAB */
			tab_count++;
			cmd_list = match_functions(command_name);
			shortest_match = shortest_common_string(cmd_list);
			if (strlen(shortest_match) == 0) {
				free_string_list(cmd_list);
				break;
			}

			strcpy(command_name, shortest_match);
			cpos = strlen(command_name);

			if (tab_count > 1 || wp != NULL) {
				zero_buffer(bp);
				wp = popup_window(bp->b_bname); /* does nothing if already exists */
				column = 0;

				/* show matched commands, start wrapping from 30 chars from end of screen */
				for (sl = cmd_list; sl != NULL; sl = sl->next) {
					append_string(bp, sl->string);
					append_string(bp, "  ");
					column += (2 + strlen(sl->string));

					if (column >= COLS - 30) {
						append_string(bp, "\n");
						column = 0;
					}
				}
				display(wp, TRUE);
			}
			free_string_list(cmd_list);
			display_prompt_and_response(prompt, command_name);
			refresh();
			break;

		case 0x7f: /* del, erase */
		case 0x08: /* backspace */
			tab_count = 0;
			if (cpos == 0)
				continue;
			command_name[--cpos] = '\0';
			display_prompt_and_response(prompt, command_name);
			break;

		default:
			tab_count = 0;
			if (cpos < STRBUF_M - 1) {
				command_name[cpos++] = ch;
				command_name[cpos] = '\0';
				display_prompt_and_response(prompt, command_name);
			}
			break;
		}
	}

	/* restore to previous window states */
	if (wp != NULL && wp->w_hijack != NULL)
		restore_hijacked_window(wp);
	else if (wp != NULL && count_windows() == 2)
		delete_other_windows();

	/* attempt to execute matched command */
	if (strlen(command_name) > 0) {
		funct = name_to_function(command_name);

		if (funct == NULL || funct == user_func) {
			char funcname[80];
			reset_output_stream();
			sprintf(funcname, "(%s)", command_name);
			char *output = call_lisp(funcname);

			/* show errors on message line */
			/* can probably make this a common function */                
			if (NULL != strstr(output, "error:")) {
				char buf[81];
				strncpy(buf, output, 80);
				buf[80] ='\0';
				msg(buf);
			}	
			reset_output_stream();
		} else {
			(funct)();
		}
	}

	mark_all_windows(); /* a lot has gone on, mark every window for update */
}
