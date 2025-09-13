# fLisp Femto Editor Extension

[fLisp Manual](flisp.html) [(Markdown)](flisp.md)

### Overview

The [editor extension](#primitives) introduces several types of objects:

- <span class="dfn">Buffers</span> hold text
- <span class="dfn">Windows</span> display buffer contents to the user
- <span class="dfn">Keyboard Input</span> allows the user to interact
  with buffers and windows
- The <span class="dfn">Message Line</span> gives feedback to the user
- Several other function for operating system or user interaction

Several [Lisp libraries](#libraries) make use of the extensions
primitives to provide advanced functionality.

### Table of Contents

1.  [Overview](#overview)
2.  Table of Contents
3.  [Editor Extension](#primitives)
    1.  [Buffers](#buffers)
        1.  [Text manipulation](#text)
        2.  [Selection](#selection)
        3.  [Cursor Movement](#cursor)
        4.  [Buffer management](#buffer_management)
    2.  [User Interaction](#ui)
        1.  [Window Handling"](#windows)
        2.  [Message Line](#message_line)
        3.  [Keyboard Handling](#keyboard)
        4.  [Programming and System Interaction](#programming_system)
4.  [Lisp Libraries](#libraries)
    1.  [`femto`](#femto_lib)
    2.  [`bufmenu`](#bufmenu) Buffer Selection Menu
    3.  [`defmacro`](#defmacro) Editor Macros
    4.  [`dired`](#dired) Directory Navigation
    5.  [`info`](#info) Builtin Help
    6.  [`git`](#git) Git Repo Helper
    7.  [`grep`](#grep) File Content Search
    8.  [`oxo`](#oxo) Tic-Tac-Toe Game

### Editor Extension

#### Buffers

This section describes the buffer related functions added by Femto to
fLisp. The description is separated in function related to buffer
management and text manipulation. Text manipulation always operates on
the <span class="dfn">current buffer</span>. Buffer management creates,
deletes buffers, or selects one of the existing buffers as the current
buffer.

Buffers store text and allow to manipulate it. A buffer has the
following properties:

*name*  
Buffers are identified by their name. If a buffer name is enclosed in
`*`asterisks`*` the buffer receives special treatment.

*text*  
zero or more characters.

*point*  
The position in the text where text manipulation takes place. The first
position in the text is 0. Note: in Emacs the first position is 1.

*mark*  
An optional second position in the text. If the *mark* is set, the text
between *point* and *mark* is called the
<span class="dfn">selection</span> or <span class="dfn">region</span>.

*filename*  
If set the buffer is associated with the respective file.

*flags*  
Different flags determine the behavior of the buffer. Editor specific
flags: `special`, `modified`.

Mode flags determine the syntax highlighter mode: `cmode` and `lispmode`
are available. If none is set `text` mode is used for syntax
hightlighting.

In the following, any mention to one of them refers to the respective
current buffers property.

##### Text manipulation

`(insert-string «string»)`  
Inserts *string* before *point*. <u>S: insert</u>.

`(insert-file-contents-literally «string» `\[*flag*\]`)`  
Inserts the file *string* after *point*. If *flag* is not nil the buffer
is marked as not modified. <u>B</u>

Note: Currently the flag is forced to nil. The function should return
`(«filename» «count»)` but it returns a flag indicating if the operation
succeeded.

`(erase-buffer)`  
Erases all text in the current buffer. <u>C</u>

`(delete)`  
Deletes the character after *point*. <u>S: delete-char</u>

`(backspace)`  
Deletes the character to the left of *point*. <u>S:
delete-backward-char</u>

`(get-char)`  
Returns the character at *point*. <u>S: get-byte</u>

`(copy-region)`  
Copies *region* to the *clipboard*. <u>S: copy-region-as-kill</u>

`(kill-region)`  
Deletes the text in the *region* and copies it to the *clipboard*.
<u>D</u>

`(yank)`  
Pastes the *clipboard* before *point*. <u>C</u>

##### Selection

`(set-mark)`  
Sets *mark* to *point*. <u>D</u>

`(get-mark)`  
Returns the position of *mark*, -1 if *mark* is unset. <u>S: mark</u>

`(get-point)`  
Returns the position of *point*. <u>S: point</u>

`(get-point-max)`  
Returns the maximum accessible value of point in the current buffer.
<u>S: point-max</u>

`(set-clipboard «variable»)`  
`Sets «clipboard» to the contents of «variable».` <u>S:
gui-set-selection</u>

`(get-clipboard)`  
Returns the *clipboard* contents. <u>S: gui-get-selection</u>

##### Cursor Movement

`(set-point «number»)`  
Sets the point to in the current buffer to the position *number*. <u>S:
goto-char</u>

`(goto-line «number»)`  
Sets the point in the current buffer to the first character on line
*number*. <u>S: goto-line</u>, not an Elisp function.

`(search-forward «string»)`  
Searches for *string* in the current buffer, starting from point
forward. If string is found, sets the point after the first occurrence
of *string* and returns `t`, otherwise leaves point alone and returns
`nil`. <u>D</u>

`(search-backward «string»)`  
Searches for *string* in the current buffer, starting from point
backwards. If string is found, sets the point before the first
occurrence of *string* and returns `t`, otherwise leaves point alone and
returns `nil`. <u>D</u>

`(beginning-of-buffer)`  
Sets the point in the current buffer to the first buffer position,
leaving mark in its current position. <u>C</u>

`(end-of-buffer)`  
Sets the point in the current buffer to the last buffer position,
leaving mark in its current position. <u>C</u>

`(beginning-of-line)`  
Sets point before the first character of the current line, leaving mark
in its current position. <u>S: move-beginning-of-line</u>

`(end-of-line)`  
Sets point after the last character of the current line, i.e. before the
end-of-line character sequence, leaving mark in its current position.
<u>S: move-end-of-line</u>

`(forward-word)`  
Moves the point in the current buffer forward before the first char of
the next word. If there is no word left the point is set to the end of
the buffer. If the point is already at the start or within a word, the
current word is skipped. <u>D</u>: **Note**: Elisp moves to the *end* of
the the next word.

`(backward-word)`  
Moves the point in the current buffer backward after the last char of
the previous word. If there is no word left the point is set to the
beginning of the buffer. If the point is already at the end or within a
word, the current word is skipped. <u>D</u>: **Note**: Elisp moves to
the *beginning* of the previous word.

`(forward-char)`  
Moves the point in the current buffer one character forward, but not
past the end of the buffer. <u>C</u>

`(backward-char)`  
Moves the point in the current buffer one character backward, but not
before the end of the buffer. <u>C</u>

`(forward-page)`  
Moves the point of the current buffer to the beginning of the last
visible line of the associated screen and scrolls the screen up to show
it as the first line. <u>S: scroll-up</u>

`(backward-page)`  
Moves the point of the current buffer to the beginning of the first
visible line of the associated screen and scrolls the screen down to
show it as the last line. <u>S: scroll-down</u>

`(next-line)`  
Moves the point in the current buffer to the same character position in
the next line, or to the end of the next line if there are not enough
characters. In the last line of the buffer moves the point to the end of
the buffer. <u>C</u>

`(previous-line)`  
Moves the point in the current buffer to the same character position in
the previous line, or to the end of the previous line if there are not
enough characters. In the first line of the buffer the point is not
moved. <u>C</u>

##### Buffer management

`(list-buffers)`  
Lists all the buffers in a buffer called `*buffers*`.

`(get-buffer-count)`  
Returns the number of buffers, includes all special buffers and
`*buffers*`.

`(select-buffer «string»)`  
Makes the buffer named *string* the current buffer. Note: <u>C</u> to
`set-buffer` in Elisp.

`(rename-buffer «string»)`  
Rename the current buffer to *string*. <u>C</u>

`(kill-buffer «string»)`  
Kill the buffer names *string*. Unsaved changes are discarded. <u>C</u>

`(get-buffer-name)`  
Return the name of the current buffer. Note: <u>C</u> to `buffer-name`
in Elisp.

`(add-mode-global «string»)`  
Sets global mode *string* for all buffers. Currently the only global
mode is <span class="kbd">undo</span>.

`(add-mode «string»)`  
Set a flag for the current buffer.

`(delete-mode «string»)`  
Reset a flag for the current buffer.

`(find-file «string»)`  
Loads file with path *string* into a new buffer. After loading
`(read-hook «string»)` is called. <u>C</u>

`(save-buffer «string»)`  
Saves the buffer named *string* to disk. <u>C</u>

#### User Interaction

This section lists function related to window and message line
manipulation, keyboard input and system interaction.

##### Window Handling

`(delete-other-windows)`  
Make current window the only window. <u>C</u>

`(split-window)`  
Splits the current window. Creates a new window for the current buffer.
<u>C</u>

`(other-window)`  
Moves the cursor to the next window down on the screen. Makes the buffer
in that window the current buffer. <u>D</u>

Note: Elisp `other-window` has a required parameter *count*, which
specifies the number of windows to move down or up.

`(update-display)`  
Updates all modified windows.

`(refresh)`  
Updates all windows by marking them modified and calling
`update-display`.

##### Message Line

`(message «string»)`  
Displays *string* in the message line. <u>D</u>

`(clear-message-line)`  
Displays the empty string in the message line.

`(prompt «prompt» «default»)`  
Displays *prompt* in the command line and sets *default* as initial
value for the user response. The user can edit the response. When
hitting return, the final response is returned.

`(show-prompt «prompt» «default»)`  
Displays *prompt* and *default* in the command line, but does not allow
editing. Returns `t`.

`(prompt-filename «prompt»)`  
Displays *prompt* in the command line and allows to enter or search for
a file name. Returns the relative path to the selected file name or the
response typed by the user.

##### Keyboard Handling

`(set-key «key-name» «lisp-func»)`  
Binds key key-name to the lisp function *lisp-func*.

`(get-key-name)`  
Returns the name of the currently pressed key, eg: `c-k` for control-k.

`(get-key-funcname)`  
Return the name of the function bound to the currently pressed key.

`(execute-key)`  
Executes the function of the last bound key. <span class="mark">Tbd.
bound or pressed?</span>

`(describe-bindings)`  
Creates a listing of all current key bindings, in a buffer named
`*help*` and displays it in a new window. <u>C</u>

`(describe-functions)`  
Creates a listing of all functions bound to keys in a buffer named
`*help*` and displays it in a new window.

`(getch)`  
Waits for a key to be pressed and returns the key as string. See also
`get-key-name`, `get-key-funcname` and `execute-key`.

##### Programming and System Interaction

`(exit)`  
Exit Femto without saving modified buffers.

`(eval-block)`  
Evaluates the *region* in the current buffer, inserts the result at
*point* and returns it. If *mark* in the current buffer is before
*point* `eval-block` evaluates this *region* and inserts the result at
*point*. If *point* is before *mark* `eval-block` does nothing but
returning `t`.

`(log-message «string»)`  
Logs *string* to the `*messages*` buffer.

`(log-debug «string»)`  
Logs string to the file `debug.out`.

`(get-version-string)`  
Returns the complete version string of Femto, including the copyright.

### Lisp Libraries

<span class="mark">Tbd.: document the libraries.</span>

#### `femto`

*Femto* editor specific functions.

This library implements helper function required by the Femto editor. It
is written only in *fLisp* primitives and plus the `flisp` Library.

#### `bufmenu` Buffer Selection Menu

#### `defmacro` Editor Macros

#### `dired` Directory Navigation

#### `info` Builtin Help

#### `git` Git Repo Helper

#### `grep` File Content Search

#### `oxo` Tic-Tac-Toe Game

[^](#toc)
