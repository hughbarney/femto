/*
 * header.h, femto, Hugh Barney, 2017
 * Derived from: Anthony's Editor January 93, (Public Domain 1991, 1993 by Anthony Howe)
 */
#define _XOPEN_SOURCE 500
#include <locale.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include <curses.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <ctype.h>
#include <limits.h>
#include <string.h>
#include <unistd.h>
#include <wchar.h>
int mkstemp(char *);

#define E_NAME          "femto"
#define E_VERSION       "2.10"
#define E_LABEL         "Femto:"
#define E_NOT_BOUND	"<not bound>"
#define E_INITFILE      "femto.rc"
#define E_VERSION_STR    E_NAME " " E_VERSION ", Public Domain, January 2018, by Hugh Barney,  No warranty."

#define MSGLINE         (LINES-1)
#define NOMARK          -1
#define NOPAREN         -1
#define CHUNK           8096L
#define K_BUFFER_LENGTH 256
#define TEMPBUF         512
#define STRBUF_L        256
#define STRBUF_M        64
#define STRBUF_S        16
#define NBUFN           17              /* size of buffer name 16 chars + null terminator */
#define MAX_KNAME       12
#define MAX_KBYTES      12
#define MAX_KFUNC       30
#define LISP_IN_OUT     2048
#define MIN_GAP_EXPAND  512
#define FWD_SEARCH      1
#define REV_SEARCH      2
#define TEMPFILE        "/tmp/feXXXXXX"

#define ID_DEFAULT         1
#define ID_SYMBOL          2
#define ID_MODELINE        3
#define ID_DIGITS          4
#define ID_LINE_COMMENT    5
#define ID_BLOCK_COMMENT   6
#define ID_DOUBLE_STRING   7
#define ID_SINGLE_STRING   8
#define ID_BRACE           9

/* undo types, in matched pairs */
#define UNDO_T_NONE        0
#define UNDO_T_INSERT      1
#define UNDO_T_BACKSPACE   2
#define UNDO_T_KILL        3
#define UNDO_T_YANK        4
#define UNDO_T_DELETE      5
#define UNDO_T_INSAT       6
#define UNDO_T_REPLACE     7

#define STR_T_INSERT       "INSERT"
#define STR_T_BACKSP       "BACKSP"
#define STR_T_KILL         "KILL  "
#define STR_T_YANK         "YANK  "
#define STR_T_DELETE       "DELETE"
#define STR_T_INSAT        "INSAT "
#define STR_T_REPLACE      "REPLC "
#define STR_T_NONE         "NONE  "

/* edit field attributes */
#define F_NONE          0
#define F_CLEAR         1
#define ZERO_STRING(X) X[0]='\0';

typedef unsigned char char_t;
typedef long point_t;
typedef void (*void_func)(void);

typedef enum {
	B_MODIFIED = 0x01,
	B_OVERWRITE = 0x02,		/* overwite mode */
	B_SPECIAL = 0x04,		/* is a special buffer name of form '*name*' */
	B_UNDO = 0x08,                  /* undo mode */
} buffer_flags_t;

typedef struct string_list_t
{
	struct string_list_t *next;
	char *string;
} string_list_t;

typedef struct keymap_t {
	char k_name[MAX_KNAME + 1];       /* name of key eg "c-c c-f" */
	char k_bytes[MAX_KNAME + 1];      /* bytes of key sequence */
	char k_funcname[MAX_KFUNC + 1];   /* name of function, eg (forward-char) */
	void (*k_func)(void);             /* function pointer */
	struct keymap_t *k_next;          /* link to next keymap_t */
} keymap_t;

typedef struct command_t {
        char c_name[MAX_KFUNC + 1];
	void (*c_func)(void);
	struct command_t *c_next;
} command_t;

/*
 * This structure supports the unlimited undo feature
 * Its members must be kept to a minimum as each instance takes up to 32 bytes
 */
typedef struct undo_tt {
	point_t  u_point;
	char_t  *u_string;
	char_t  *u_replace;
	char_t   u_type;
	struct undo_tt *u_prev;
} undo_tt;

typedef struct buffer_t
{
	struct buffer_t *b_next;  /* Link to next buffer_t */
	point_t b_mark;	     	  /* the mark */
	point_t b_point;          /* the point */
	point_t b_paren;          /* matching paren to the point */
	point_t b_cpoint;         /* the original current point, used for mutliple window displaying */
	point_t b_page;           /* start of page */
	point_t b_epage;          /* end of page */
	int b_cnt;                /* count of windows referencing this buffer */
	int b_size;               /* current size of text being edited (not including gap) */
	int b_psize;              /* previous size */
	char_t *b_buf;            /* start of buffer */
	char_t *b_ebuf;           /* end of buffer */
	char_t *b_gap;            /* start of gap */
	char_t *b_egap;           /* end of gap */
	int b_row;                /* cursor row */
	int b_col;                /* cursor col */
	char b_fname[NAME_MAX + 1]; /* filename */
	char b_bname[NBUFN];      /* buffer name */
	buffer_flags_t b_flags;   /* buffer flags */
	undo_tt *b_utail;         /* recent end of undo list */
	int b_ucnt;               /* count of how many chars to undo on current undo */
} buffer_t;

typedef struct window_t
{
	struct window_t *w_next;   /* Next window */
	struct buffer_t *w_bufp;   /* Buffer displayed in window */
	struct buffer_t *w_hijack; /* holds the buffer association for a hijacked window */
	point_t w_point;
	point_t w_mark;
	point_t w_page;
	point_t w_epage;
	char w_top;	    /* origin 0 top row of window */
	char w_rows;        /* no. of rows of text in window */
	int w_row;          /* cursor row */
	int w_col;          /* cursor col */
	int w_update;
	char w_name[STRBUF_S];
} window_t;

extern buffer_t *curbp;			/* current buffer */
extern buffer_t *bheadp;		/* head of list of buffers */
extern window_t *curwp;
extern window_t *wheadp;
extern command_t *cheadp;

/*
 * Some compilers define size_t as a unsigned 16 bit number while
 * point_t and off_t might be defined as a signed 32 bit number.  
 * malloc(), realloc(), fread(), and fwrite() take size_t parameters,
 * which means there will be some size limits because size_t is too
 * small of a type.
 */
#define MAX_SIZE_T      ((unsigned long) (size_t) ~0)

extern int done;                /* Quit flag. */
extern int msgflag;             /* True if msgline should be displayed. */
extern int global_undo_mode;    /* True if we are undo mode is allowed by default */
extern point_t nscrap;          /* Length of scrap buffer. */
extern char_t *scrap;           /* Allocated scrap buffer. */
extern char_t *input;
extern char msgline[];          /* Message line input/output buffer. */
extern char response_buf[];     /* Temporary buffer. */
extern char searchtext[];
extern char replace[];

extern keymap_t *key_return;    /* Command key return */
extern keymap_t *khead;
extern keymap_t *ktail;

/* fatal() messages. */
extern char *f_ok;              /* EXIT_OK */
extern char *f_error;           /* EXIT_ERROR */
extern char *f_usage;           /* EXIT_USAGE */
extern char *f_initscr;         /* EXIT_FAILURE ... */
extern char *f_alloc;

/* Messages. */
extern char *m_ok;
extern char *m_version;
extern char *m_alloc;
extern char *m_toobig;
extern char *m_empty;
extern char *m_stat;
extern char *m_open;
extern char *m_close;
extern char *m_read;
extern char *m_write;
extern char *m_copied;
extern char *m_cut;
extern char *m_badname;
extern char *m_saved;
extern char *m_loaded;
extern char *m_newfile;
extern char *m_line;
extern char *m_lnot_found;
extern char *m_replace;
extern char *m_with;
extern char *m_sprompt;
extern char *m_qreplace;
extern char *m_rephelp;
extern char *m_goto;
extern char *str_mark;
extern char *m_nomark;
extern char *m_noregion;
extern char *str_pos;
extern char *str_endpos;
extern char *str_not_bound;
extern char *str_help_buf;
extern char *str_completions;
extern char *str_apropos;
extern char *str_exec_command;


/* Prompts */
extern char *str_notsaved;
extern char *str_modified_buffers;
extern char *str_read;
extern char *str_insert_file;
extern char *str_write;
extern char *str_yes;
extern char *str_no;
extern char *str_scratch;
extern char *str_output;
extern char *str_shell_cmd;
extern char *str_buffers;
extern char *str_clip_too_big;


/* functions in buffer.c */
extern buffer_t *find_buffer_by_fname(char *);
extern buffer_t *find_buffer(char *, int);
extern char* get_buffer_filename(buffer_t *);
extern char* get_buffer_modeline_name(buffer_t *);
extern char* get_buffer_name(buffer_t *);
extern char *get_current_bufname();
extern int buffer_is_empty(buffer_t *);
extern int count_buffers();
extern int delete_buffer(buffer_t *);
extern int delete_buffer_byname(char *);
extern int modified_buffers();
extern int save_buffer_byname(char *);
extern int select_buffer(char *);
extern point_t document_size(buffer_t *);
extern void add_mode(buffer_t *, buffer_flags_t);
extern void buffer_init(buffer_t *);
extern void delete_mode(buffer_t *, buffer_flags_t);
extern void list_buffers();
extern void next_buffer();
extern void zero_buffer(buffer_t *);

/* functions in command.c */
extern char *get_char();
extern char *get_clipboard();
extern char* get_temp_file();
extern char *get_version_string();
extern char *rename_current_buffer(char *);
extern int add_mode_global(char *);
extern int goto_line(int);
extern int i_check_region();
extern int prev_utf8_char_size();
extern int utf8_size(char_t c);
extern int yesno(int flag);
extern point_t get_mark();
extern point_t get_point();
extern point_t get_point_max();
extern unsigned char *get_scrap();
extern void append_string(buffer_t *, char *);
extern void backspace();
extern void backward_page();
extern void backward_word();
extern void beginning_of_buffer();
extern void copy_cut(int cut);
extern void copy_region();
extern void delete();
extern void down();
extern void end_of_buffer();
extern void eval_block();
extern void forward_page();
extern void forward_word();
extern void i_gotoline();
extern void insert();
extern void insert_at();
extern void insertfile();
extern void insert_string(char *);
extern void i_readfile();
extern void i_set_mark();
extern void i_shell_command();
extern void kill_buffer();
extern void kill_region();
extern void left();
extern void lnbegin();
extern void lnend();
extern void log_debug(char *);
extern void log_debug_message(char *format, ...);
extern void log_message(char *);
extern void match_paren_backwards(buffer_t *, char, char);
extern void match_paren_forwards(buffer_t *, char, char);
extern void match_parens();
extern void quit();
extern void quit_ask();
extern void readfile(char *);
extern void redraw();
extern void repl();
extern void resize_terminal();
extern void right();
extern void savebuffer();
extern void set_mark();
extern void set_point(point_t);
extern void set_scrap(unsigned char *);
extern void shell_command(char *);
extern void cursor_position();
extern void toggle_overwrite_mode();
extern void unmark();
extern void up();
extern void user_func();
extern void version();
extern void writefile();
extern void yank();

/* functions in complete.c */
extern int getfilename(char *, char *, int);

/* functions in display.c */
extern point_t dndn(buffer_t *, point_t);
extern point_t lncolumn(buffer_t *, point_t, int);
extern point_t lnstart(buffer_t *, register point_t);
extern point_t segnext(buffer_t *, point_t, point_t);
extern point_t segstart(buffer_t *, point_t, point_t);
extern point_t upup(buffer_t *, point_t);
extern void b2w_all_windows(buffer_t *);
extern void b2w(window_t *w);
extern void clear_message_line();
extern void display_char(buffer_t *, char_t *);
extern void display_prompt_and_response(char *, char *);
extern void display_utf8(buffer_t *, char_t, int);
extern void display(window_t *, int);
extern void dispmsg();
extern void modeline(window_t *);
extern void update_display();
extern void w2b(window_t *);

/* functions in funcmap.c */
extern char *shortest_common_string(string_list_t *);
extern command_t *register_command(char *, void (*)(void));
extern int count_string_list(string_list_t *);
extern int match_string_position(string_list_t *, int);
extern int shortest_string_len(string_list_t *);
extern string_list_t *match_functions(const char *);
extern void apropos();
extern void execute_command();
extern void free_string_list(string_list_t *);
extern void_func name_to_function(const char *);
extern void describe_bindings();
extern void describe_functions();

/* functions in gap.c */
extern char_t * ptr(buffer_t *, register point_t);
extern int e_load_file(char *);
extern int growgap(buffer_t *, point_t);
extern int insert_file(char *, int);
extern int posix_file(char *);
extern int save_buffer(buffer_t *, char *);
extern point_t line_to_point(int);
extern point_t movegap(buffer_t *, point_t);
extern point_t pos(buffer_t *, register char_t *);
extern void clear_buffer();
extern void get_line_stats(int *, int *);

/* functions in hilite.c */
extern char_t get_at(buffer_t *, point_t);
extern int is_symbol(char_t);
extern int parse_text(buffer_t *, point_t);
extern void set_parse_state(buffer_t *, point_t);

/* functions in key.c */
extern char *get_input_key();
extern char *get_key_funcname();
extern char *get_key_name();
extern char_t *get_key(keymap_t *, keymap_t **);
extern int getinput(char *, char *, int, int);
extern int set_key(char *, char *);
extern int set_key_internal(char *, char *, char *, void (*)(void));
extern keymap_t *new_key(char *, char *);
extern void create_keys();
extern void execute_key();
extern void make_key(char *, char *);
extern void setup_keys();

/* functions in lisp.c */
extern char *call_lisp(char *);
extern char *load_file(int);
extern int init_lisp();
extern void reset_output_stream(void);

/* functions in main.c */
extern int main(int argc, char **);
extern void debug(char *format, ...);
extern void debug_stats(char *s);
extern void fatal(char *msg);
extern void load_config();
extern void msg(char *m, ...);

/* functions in replace.c */
extern void query_replace(void);
extern void replace_string(buffer_t *, char *, char *, int, int);

/* functions in search.c */
extern point_t search_backwards(char *);
extern point_t search_forward(char *);
extern void display_search_result(point_t found, int, char *, char *);
extern void move_to_search_result(point_t);
extern void search();

/* functions in undo.c */
extern char *get_undo_type_name(undo_tt *);
extern int count_undos(buffer_t *);
extern int get_buf_utf8_size(char_t *, int);
extern int get_total_undo_size(buffer_t *);
extern int get_undo_again();
extern int get_undo_size(undo_tt *);
extern undo_tt *execute_undo(undo_tt *);
extern undo_tt *new_undo();
extern void add_undo(buffer_t *, char, point_t, char_t *, char_t *);
extern void append_undo_string(undo_tt *, char_t *);
extern void debug_undo(char *, undo_tt *, buffer_t *);
extern void discard_buffer_undo_history(buffer_t *);
extern void discard_undo_history();
extern void dump_undos(buffer_t *);
extern void free_undos(undo_tt *);
extern void list_undos();
extern void list_undo_stats();
extern void undo_command();

/* functions in utils.c */
extern void make_buffer_name(char *, char *);
extern void make_buffer_name_uniq(char *);
extern void remove_control_chars(char_t *);
extern void safe_strncpy(char *, char *, int);

/* functions in window.c */
extern int count_windows();
extern void associate_b2w(buffer_t *, window_t *);
extern void delete_other_windows();
extern void disassociate_b(window_t *);
extern void free_other_windows(window_t *);
extern void hijack_window(window_t *, buffer_t *);
extern void mark_all_windows();
extern void one_window(window_t *);
extern void other_window();
extern void restore_hijacked_window(window_t *);
extern void split_window();
extern window_t *find_window(char *);
extern window_t* new_window();
extern window_t *popup_window(char *);
extern window_t *split_current_window();
