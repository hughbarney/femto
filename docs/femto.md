# The Femto Editor

> A designer knows he has achieved perfection not when there is nothing
> left to add, but when there is nothing left to take away. -- Antoine
> de Saint-Exupery

*Femto* is an Emacs like text editor for the console with a minimal
memory footprint which can be customized with a Lisp extension language.
This manual describes how to use and customize *Femto*. It refers to
*Femto* version 2.22.

*Femto* is hosted on its [Github
repository](https://github.com/hughbarney/femto), it is released to the
public domain by [Hugh Barney](mailto:hughbarney@gmail.com) in 2017.

The extension language of *Femto* is documented in the [fLisp
manual](flisp.html) ([Markdown](flisp.md)).

## Femto Key Bindings

        C-A   begining-of-line
        C-B   backward-character
        C-D   delete-char
        C-E   end-of-line
        C-F   forward Character
        C-G   Abort (at prompts)
        C-H   backspace
        C-I   handle-tab
        C-J   newline
        C-K   kill-to-eol
        C-L   refresh display
        C-M   Carrage Return
        C-N   next line
        C-P   previous line
        C-R   search-backwards
        C-S   search-forwards
        C-T   transpose-chars
        C-U   Undo
        C-V   Page Down
        C-W   Kill Region (Cut)
        C-X   CTRL-X command prefix
        C-Y   Yank (Paste)
        C-Z   suspend

        M-<   Start of file
        M->   End of file
        M-v   Page Up
        M-f   Forward Word
        M-b   Backwards Word
        M-g   goto-line
        M-r   Search and Replace
        M-w   copy-region

        C-<spacebar> Set mark at current position.

        ^X^B  List Buffers
        ^X^C  Exit. Any unsaved files will require confirmation.
        ^X^F  Find file; read into a new buffer created from filename.
        ^X^S  Save current buffer to disk, using the filename associated with the buffer
        ^X^W  Write current buffer to disk. Type in a new filename at the prompt
        ^X@   shell-command (prompted for a command which is sent to the shell
        ^Xi   Insert file at point
        ^X=   Show Character at position
        ^X^N  next-buffer
        ^Xn   next-buffer
        ^Xk   kill-buffer
        ^X1   delete-other-windows
        ^X2   split-window
        ^Xo   other-window

        Home  Beginning-of-line
        End   End-of-line
        Del   Delete character under cursor
        Ins   Toggle Overwrite Mode
        Left  Move left
        Right Move point right
        Up    Move to the previous line
        Down  Move to the next line
        Backspace delete caharacter on the left
        Ctrl+Up      beginning of file
        Ctrl+Down    end of file
        Ctrk+Left    Page Down
        Ctrl+Right   Page Up

### Copying and moving

        C-<spacebar> Set mark at current position
        ^W   Delete region
        ^Y   Yank back kill buffer at cursor
        M-w  Copy Region

A region is defined as the area between this mark and the current cursor
position. The kill buffer is the text which has been most recently
deleted or copied.

Generally, the procedure for copying or moving text is:

1.  Mark out region using M- at the beginning and move the cursor to the
    end.
2.  Delete it (with ^W) or copy it (with M-W) into the kill buffer.
3.  Move the cursor to the desired location and yank it back (with ^Y).

### Searching

        C-S or C-R enters the search prompt, where you type the search string
        BACKSPACE - will reduce the search string, any other character will extend it
        C-S at the search prompt will search forward, will wrap at end of the buffer
        C-R at the search prompt will search backwards, will wrap at start of the buffer
        ESC will escape from the search prompt and return to the point of the match
        C-G abort the search and return to point before the search started

## Lisp Interaction

There are two ways to interact with Tiny-Lisp within Femto.

- You can use C-\] to find the last s-expression above the cursor and
  send it to be evaluated.
- You can mark a region and send the whole region to be evaluated with
  ESC-\].

### Lisp Interaction - finding and evaluating the last s-expression

This works in almost the same way as GNU Emacs in the scratch buffer.

### Lisp Interaction - mark and evaluating a region

Type a lisp function into the editor, for example:

        1: --------------
        2: (defun factorial (n)
        3:   (cond ((= n 0) 1)
        4:     (t (* n (factorial (- n 1))))))
        5:--------------

Place the cursor at the beginning of line 1 and set a mark (hit
control-spacebar).

Now move the cursot to line 5 and evaluate the block of code (hit escape
followed by \])

Femto will pass the code to lisp for it to be evaluated.

    <Lambda (n)>

Now call factorial in the same way (mark the start of the code, move to
the end of the code and hit escape-\])

    (factorial 6)

        720

![Screenshot of Femto editor showing Lisp
evaluation.](https://github.com/hughbarney/femto/blob/master/screenshots/femto-lisp.jpg)

## Femto Startup

The Femto editor itself provides only basic buffer movement and edit
functions, everything else is done by extending the user interface using
the Lisp extension language.

The `lisp` subdirectory contains essential extensions to the Femto
editor as well as examples. With `make install` these are copied to a
system wide location, together with the `femto.rc` file.

When Femto starts it loads the `femto.rc` file, which in turn loads the
extensions with their respective key bindings and then shows a startup
message in the *scratch* buffer.

Just before showing the startup message a user specific `femto.rc` file
is loaded from the directory `.config/femto` in your HOME directory, if
available.

The `femto.rc` file and the extensions are loaded by default from
`/usr/local/share/femto`. This path is made available in the Lisp
interpreter as `script_dir`. This default directory can be changed at
compile time by changing SCRIPTDIR. At startup `femto` overrides the
default with the value of the environment variable FEMTOLIB if set.

## Modes

Modes control the behaviour of the editor and the syntax highligher

cmode  
Highlights single and double quoted stings, block comments and line
comments.

lisp  
ingle line comments start with ;

`startup.lsp` defines a default `read-hook` function which sets these
modes based on the filename.

## Debugging

If the environment variable FEMTO_DEBUG exists and is not set to `0`
debug mode is enabled. In debug mode, some internal workings are logged
to the file `debug.out`, as well as the arguments of the invocations of
the `log-debug` Lisp primitive.

## Basic Femto Extension

### `femto.rc`

This module implements a simple library loader and handles batch mode.
It provides the “core” functionality comprised of the following
functionality.

Standard Lisp functions:

`list «arg»` ..

Returns a list of all *arg*uments.

`defmacro`

`defun`

`string`

`concat`

`memq`

`load`

Lisp library functions:

- `require` auto-loads a 'feature' from the library if not already
  loaded.
- `provides` is used to specify a 'feature' in a library file, it must
  be the same as the filename without the `.lsp` extension.

`flisp.lsp` complements commonly known standard lisp functions:

- `not`, `listp`, `and`, `map1`, `or`, `reduce`, `max`, `min`, `nthcdr`

`femto.lsp` extend the Lisp editor and provide basic editor
functionality.

- `load-script` `file` - load file from script directory.
- `delete-next-word`, `delete-previous-word`, `kill-to-eol` - For Emacs
  like key bindings
- `describe-key`, `find_end_p`, `find_start_p`, `find_and_eval_sexp`,
  `show-startup-message`
- `string.trim.front` `s`, `string.trim.back` `s`, `string.trim` `s`,
  `shrink` `s` - string operations
- `repeat` `n func` - Lisp functions
- `is_ctl_g` `k`, `is_escape` `k`, `is_backspace` `k`, `is_ctl_s` `k`,
  `is_control_char` `k` - helper functions
- `shell-command` - backwards compatibility wrapper for `system`, reads
  command line interactively.
- `shell-exec` `command` - convenience wrapper for `system`, used by
  `shell-command`.
- `insert-file` - prompts for a file name with incremental search and
  inserts it after the point.
- `describe-key`
- `find_start_p`, `find_end_p`, `find_and_eval_sexp` - Lisp evaluation
  in buffers.
- `transpose-chars` - swap current end previous character.

`startup.lsp` loads extensions, creates key bindings for them, shows the
startup message, loads the user specified configuration file and
processes the command line arguments. `~/.config/femto/femto.rc`. It
defines the following functions:

- `getopts` `opts` `pos` - process commandline options starting from
  position pos.
- `show-startup-message` - shows the startup banner in the \*scratch\*
  buffer.
- `edit-config` - open user specific configuration file in Femto.
- `show-info` - load the `info.lsp` file.

## Femto Extensions

Additional extensions loaded by `femto.rc`

- **dired** - enbles directory editing (Emacs style filemanager)

  - **C-x C-d** to invoke, then single character keystrokes provide menu
    options
  - **f** load file on current line
  - **x** exit dired (other features to be added)

- **grep** - enables searching for text in files and loading of the
  files at the location of the match into the editor.

  - **C-x g** to invoke, will request a search string and files to
    search
  - **C-x \`** to load the next matching file

- **bufmenu** - the classic Emacs buffer menu

  - **C-x C-b** to invoke, then single character keystrokes provide menu
    options
  - **1** loads the file on the current line in one window
  - **2** loads the file on the current line in a split window
  - **s** saves the file on the current line to disk
  - **k** unloads the file without saving
  - **x** exits bufmenu

- **git** - a simple interface to the git version control tool (similar
  to GNU Emacs magit).

  - **C-x g** to invoke, then single character keystrokes menu options

- **oxo** - a basic implementation of tick-tack-toe that runs in the
  Editor.

  - **C-x C-o** to invoke

![Femto
screenshot](https://github.com/hughbarney/femto/blob/master/screenshots/femto-oxo.jpg)

## Known Issues

`goto-line` will fail to go to the very last line. This is a special
case that could easily be fixed.

Adding a line at the bottom of a window will hide the line until the
cursor moves up and down again or the screen is refreshed.
