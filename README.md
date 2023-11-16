# Femto
Femto is an extended version of Atto Emacs with a Tiny Lisp extension languauge

![Femto screenshot](https://github.com/hughbarney/femto/blob/master/screenshots/femto-hilite.png)

![Femto screenshot](https://github.com/hughbarney/femto/blob/master/screenshots/femto-startup.jpg)

> A designer knows he has achieved perfection not when there is nothing left to add, but when there is nothing left to take away.
> -- <cite>Antoine de Saint-Exupery</cite>

## Goals of Femto Emacs

* To be an extendable version of the Atto Emacs editor using a Tiny Lisp extension language
* Provide a number of useful extension packages written in Tiny Lisp (these include an interface to **git** (similar to GNU Emacs Magit), a small version of **dired**, a buffer management menu (**buffer menu**), **defmacro** allows for a macro to be recorded and invoked using c-x e, and an interface to **grep**.
* Be easy to understand without extensive study (to encourage further experimentation).


## What does Femto bring to the party of Text Editors

As far as I know Femto is the only Emacs style editor to provide a macro recorder that generates usable Lisp code that can then be used to build a larger, more complex utility.   Whilst GNU Emacs has a macro recorder facility it only allows you to dump out the keystrokes used during macro recording.  Femto does this by writing the lisp code to a text buffer called **macro**.  Though I have tried dozens of text editors over the years (mostly on PCs, but a few on mini and mainframe computers) I am not aware of any other editor that works this way.  This feature was born out of the principle of keeping a small editor code written in C and where possible using Lisp to implement new features.  The standard Emacs macro keystrokes [C-x (, C-c ), C-x e] are all written in Lisp in the file examples/defmacro.lsp. This meant that no special C code was needed in Femto to know when it was in macro mode or not.


## Why the name Femto?

The small Emacs naming scheme appears to use sub-unit prefixes in decending order with each further reduction of functionality. The Nano and Pico Emacs editors have been around for a while.

* Nano means 10 to the power of minus 9
* Pico means 10 to the power of minus 12 
* Femto means 10 to power of minus 15
* Atto means 10 to power of minus 18
* Zepto means 10 to the power of minus 21
* Zep is smaller version of Zepto Emacs 

In Defining Atto as the lowest functional Emacs I have had to consider the essential feature set that makes Emacs, 'Emacs'. I have defined this point as a basic Emacs command set and key bindings; the ability to edit multiple files (buffers), and switch between them; edit the buffers in mutliple windows, cut, copy and paste; forward and reverse searching, a replace function and basic syntax hilighting. The proviso being that all this will fit in less than 2000 lines of C.

Femto is an extended version of Atto Emacs with its own extension languauge


## History

* In late 2015 Hugh Barney wrote the Atto editor 'A minimum functioning Emacs is less than 2000 lines of C'.  Atto was based on Anthony Howe's editor (commonly known as Anthony's Editor or AE, [2]). 
* **Femto** is based on the Atto codebase [0]
* **Femto** was originally an intermediate project to form a codebase for the FemtoEmacs Editor [8], [9] which was a collaboration between Hugh Barney, Ed Costa and Lucas Guerra.  FemtoEmacs uses Jeff Bezanson's Femtolisp LISP [10] implementation as the basis for its extension language.  However the Femtolisp codebase is in excess of 12K line of code and fairly difficult to understand how to use it inside an embedded application.
* In late 2016 Hugh Barney decided to look for a smaller lisp implementation for Femto and settled on Tiny-Lisp[7] by Mattias Pirstitz.
* **Zepl** was an initial project that established the suitability of Tiny-Lisp for use within an Emacs type editor. The results surpassed expectations.
* In late 2017 Hugh Barney decided to return to the **Femto** editor and extend it using Tiny-Lisp.

For a full version history please refer to the file CHANGE.LOG.md


## Comparisons with Other Emacs Implementations

Femto has almost the same level of functionality as MicroEmacs 3.10 for a codebase 1/10 of the size.

    Editor         Binary   BinSize     KLOC  Files

    atto           atto       33002     1.9k     10
    pEmacs         pe         59465     5.7K     16
    Esatz-Emacs    ee         59050     5.7K     14
    femto          femto     356399     6.3k     18 **
    GNOME          GNOME      55922     9.8k     13
    Zile           zile      257360    11.7k     48
    Mg             mg        585313    16.5K     50
    uEmacs/Pk      em        147546    17.5K     34
    Pico           pico      438534    24.0k     29
    Nano           nano      192008    24.8K     17
    jove           jove      248824    34.7k     94
    Qemacs         qe        379968    36.9k     59
    ue3.10         uemacs    171664    52.4K     16 ++
    GNUEmacs       emacs   14632920   358.0k    186

## Femto Key Bindings
    C-A   begining-of-line
    C-B   backward-character
    C-D   delete-char
    C-E   end-of-line
    C-F   forward Character
    C-G	  Abort (at prompts)
    C-H   backspace
    C-I   handle-tab
    C-J   newline
    C-K   kill-to-eol
    C-L   refresh display
    C-M   Carrage Return
    C-N   next line
    C-P   previous line
    C-R   search-backwards
    C-S	  search-forwards
    C-U   Undo
    C-V   Page Down
    C-W   Kill Region (Cut)
    C-X   CTRL-X command prefix
    C-Y   Yank (Paste)

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

A region is defined as the area between this mark and the current cursor position. The kill buffer is the text which has been most recently deleted or copied.

Generally, the procedure for copying or moving text is:
1. Mark out region using M-<spacebar> at the beginning and move the cursor to the end.
2. Delete it (with ^W) or copy it (with M-W) into the kill buffer.
3. Move the cursor to the desired location and yank it back (with ^Y).

### Searching
    C-S or C-R enters the search prompt, where you type the search string
    BACKSPACE - will reduce the search string, any other character will extend it
    C-S at the search prompt will search forward, will wrap at end of the buffer
    C-R at the search prompt will search backwards, will wrap at start of the buffer
    ESC will escape from the search prompt and return to the point of the match
    C-G abort the search and return to point before the search started



## Lisp Interaction

There are two ways to interract with Tiny-Lisp within Femto.

* You can use C-] to find the last s-expression above the cursor and send it to be evaluated.
* You can mark a region and send the whole region to be evaluated.

### Lisp Interaction - finding and evaluating the last s-expression

This works in almost the same way as GNU Emacs in the scratch buffer.


### Lisp Interaction - mark and evaluating a region

Type a lisp function into the editor.

for example:

    1: --------------
    2: (defun factorial (n)
    3:   (cond ((= n 0) 1)
    4:     (t (* n (factorial (- n 1))))))
    5:--------------

Place the cursor at the beginning of line 1 and set a mark (hit control-spacebar).

Now move the cursot to line 5 and evaluate the block of code (hit escape followed by ])

Femto will pass the code to lisp for it to be evaluated.
 
    <Lambda (n)>

Now call factorial in the same way (mark the start of the code, move to the end of the code and hit escape-])

    (factorial 6)

    720

![Femto screenshot](https://github.com/hughbarney/femto/blob/master/screenshots/femto-lisp.jpg)


## Femto Startup

The Femto editor itself provides only basic buffer movement and edit
functions, everything else is done by extending the user interface
using the Lisp extension language.

The `lisp` subdirectory contains essential extensions to the Femto
editor as well as examples.  With `make install` these are copied to a
system wide location, together with the `femto.rc` file.

When Femto starts it loads the `femto.rc` file, which in turn loads
the extensions with their respective key bindings and then shows a
startup message in the *scratch* buffer.

Just before showing the startup message a user specific `femto.rc`
file is loaded from the directory `.config/femto` in your HOME
directory if available.

If `femto` should be run from the source directory build it with

    CPPFLAGS=-DE_INITFILE=\\\"femto.rc\\\" make SCRIPTDIR=\\\"lisp\\\"

It will then read `femto.rc` the current directory and the extensions
from the `lisp` subdirectory.

## Basic Femto Extension

`femto.rc` and `femto.lsp` extend the Lisp editor and provide basic editor functionality.

* `load-script` *`file`* - load file from script directory.
* `edit-config` - open user specific configuration file in Femto.
* `delete-next-word`, `delete-previous-word`, `kill-to-eol` - For Emacs like key bindings
* `describe-key`, `find_end_p`, `find_start_p`, `find_and_eval_sexp`, `show-startup-message`
* `concat` *`args`*, `string.trim.front` *`s`*, `string.trim.back` *`s`*, `string.trim` *`s`*, `shrink` *`s`* - string operations
* `max` *`a b`*, `min` *`a b`*, `repeat` *`n func`* - Lisp functions
* `is_ctl_g` *`k`*, `is_escape` *`k`*, `is_backspace` *`k`*, `is_ctl_s` *`k`*, `is_control_char` *`k`* - helper functions


## Femto Extensions

Additional extensions loaded by `femto.rc`

* **grep** - enables searching for text in files and loading of the files at the location of the match into the editor.
* **bufmenu** - the classic Emacs buffer menu
* **git** - a simple interface to the git version control tool (similar to GNU Emacs magit).
* **oxo** - a basic implementation of tick-tack-toe that runs in the Editor.

![Femto screenshot](https://github.com/hughbarney/femto/blob/master/screenshots/femto-oxo.jpg)


## Lisp Function Interface
```lisp

;;
;; cursor movement
;;

(forward-char)                          ;; move the point forward 1 character
(forward-word)                          ;; move the point forward by 1 word
(forward-page)                          ;; move forward by 1 page
(backward-char)                         ;; move backward 1 character
(backward-word)                         ;; move forward 1 word
(backward-page)                         ;; move backward by 1 page
(next-line)                             ;; move to the next line
(previous-line)                         ;; move to the previous line
(beginning-of-line)                     ;; go to the beginning of the current line
(end-of-line)                           ;; go to the end of the current line
(beginning-of-buffer)                   ;; go to the beginning of the buffer
(end-of-buffer)                         ;; go to the end of the buffer
(goto-line 11)                          ;; go to the specified line
(set-point 1234)                        ;; set the point to the value specified
(get-point)                             ;; returns the current point

;;
;; buffer handling
;;

(get-buffer-count)                      ;; return the number of buffers, includes all special buffers and *buffers*
(get-buffer-name)                       ;; return the name of the current buffer
(select-buffer "mybuf.txt")             ;; select a buffer called mybuf.txt
(kill-buffer "mybuf.txt")               ;; kill the buffer called mybuf.txt, unsaved changes are discarded
(rename-buffer "newname.txt")           ;; rename the current buffer to a new name
(list-buffers)                          ;; list all the buffers in a buffer called *buffers*
(find-file "file.txt")                  ;; loads file into a new buffer
(save-buffer)                           ;; saves the current buffer to disk

;;
;; window handling
;;

(delete-other-windows)                  ;; make current window the only window
(other-window)                          ;; moves to the next window down on the screen
(split-window)                          ;; splits the current window

;;
;; cut, copy, paste and the clipboard
;;

(set-mark)                              ;; sets the mark at the current point in the buffer
(copy-region)                           ;; copies the current region into the clipboard
(kill-region)                           ;; kills the current region and copies it into the clipboard
(yank)                                  ;; pastes the clipboard into the current buffer
(get-clipboard)                         ;; returns the contents of the clipboard as a string
(set-clipboard var)                     ;; sets up clipboard with contents of string var
(delete)                                ;; deletes the character at the point
(backspace)                             ;; deletes the character to the left of the point

;;
;; keyboard handling
;;

(get-char)                              ;; return the character at the current position in the file
(get-key)                               ;; wait for a key press, return the key or "" if the key was a command key
(get-key-name)                          ;; return the name of the key pressed eg: c-k for control-k.
(get-key-funcname)                      ;; return the name of the function bound to the key
(getch)                                 ;; calls the c function getch and returns the keystroke
(set-key "key-name" "lisp-func")        ;; binds a key to a lisp function, see keynames see "Keys Names below"

;;
;; string handling
;; 

(string? s)                             ;; return true if s is a string
(string.length "string")                ;; return the length of the string
(string.ref s pos)                      ;; return the character at position pos (0 based) in string s
(string.trim " abc ")                   ;; return a string with the spaces trimmed off the beginning and the end
(string.append "string1" "string2")     ;; concatenate 2 strings returning a new string
(string.substring string n1 n2)         ;; return a substring of string from ref n1 to n2
(string->number s)                      ;; return a number converted from the string, eg "99" => 99
(number->string n)                      ;; return a strung representation of the number, eg 99.56 => "99.56"

;;
;; number handling
;;

(number->string)                        ;; convert a number type to a string
(number? var)                           ;; return t if the variable is a number
(ascii 67)                              ;; return the ASCII character as a string for the number
(ascii->number "C")                     ;; return the ASCII value for the character passed in as a single char string

;;
;; interaction with the user
;;

(message str)                           ;; set the message line to the string
(clear-message-line)                    ;; clear the message line
(prompt "prompt" "initial response")    ;; prompts for a value on the command line and returns the response
(show-prompt p r)                       ;; display the prompt and response but do not go into editing mode of the response
(update-display)                        ;; calls the display function so that the screen is updated
(refresh)                               ;; marks all windows for updates and the calls update-display

;;
;; lisp interaction
;;

(load "filename")                       ;; load and evaluate the lisp file
(eval-block)                            ;; passes the marked region to be evaluated by lisp, displays the output
(log-message string)                    ;; log the message to the *messages* buffer
(log-debug string)                      ;; write the string to the debug.out file

;;
;; miscellaneous
;;

(insert-string "string")                ;; insert the string into the buffer at the current location
(search-forward "accelerate")           ;; search forward from the point value passed in for the string supplied
(search-backwards "string")             ;; search backwards from the point value passed in for the string supplied
(get-version-string)                    ;; return the version string for this version of femto
(shell-command cmd_string)              ;; send the cmd_string to the shell and colect the output in the *output* buffer
(os.getenv("PATH")                      ;; return the value of the environment variable eg "PATH"
(add-mode-global("undo")                ;; set a global mode on for all buffers (only supports "undo" at present)
(exit)                                  ;; exit femto


```

## Building on Ubuntu (using UTF8 support in ncurse / ncursesw)

When building on Ubuntu you will need to install the libcurses dev package.
NOTE: As of Femto 1.2 you will also need the libncursesw (wide) library

	$ sudo apt-get install apt-file
	$ apt-file update

now search for which package would have curses.h

	$ apt-file search curses.h

	libncurses5-dev: /usr/include/curses.h

	$ sudo apt-get install libncurses5-dev libncursesw5-dev


## Future Enhancements

The following enhancements are envisaged.

* Directory and file manegement (Dired) functionality.  A basic start has been made with dired.lsp

## Known Issues

Goto-line will fail to go to the very last line.  This is a special case that could easily be fixed.

## Copying

Femto code is released to the public domain. hughbarney@gmail.com November 2017

## References

 * [0] Atto Emacs - https://github.com/hughbarney/atto
 * [1] Perfect Emacs - https://github.com/hughbarney/pEmacs
 * [2] Anthony's Editor - https://github.com/hughbarney/Anthony-s-Editor
 * [3] MG - https://github.com/rzalamena/mg
 * [4] Jonathan Payne, Buffer-Gap: http://ned.rubyforge.org/doc/buffer-gap.txt
 * [5] Anthony Howe,  http://ned.rubyforge.org/doc/editor-101.txt
 * [6] Anthony Howe, http://ned.rubyforge.org/doc/editor-102.txt
 * [7] Tiny-Lisp,  https://github.com/matp/tiny-lisp
 * [8] FemtoEmacs, https://github.com/FemtoEmacs/Femto-Emacs
 * [9] FemtoEmacs, https://github.com/hughbarney/Femto-Emacs
 * [10] Femtolisp,  https://github.com/JeffBezanson/femtolisp
