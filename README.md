# Femto
Femto is an extended version of Atto Emacs with a Tiny Lisp extension languauge

> A designer knows he has achieved perfection not when there is nothing left to add, but when there is nothing left to take away.
> -- <cite>Antoine de Saint-Exupery</cite>

## Goals of Femto Emacs

* To be an extendable version of the Atto Emacs editor using a Tiny Lisp extension language
* Provide a rich level of functionality in the smallest amount of code
* Be easy to understand without extensive study (to encourage further experimentation).

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


## Derivation
* Femto is based on the Atto codebase [0], which in turn was based on Anthony Howe's editor (commonly known as Anthony's Editor or AE, [2]). 
* Femto was originally an intermediate project to form a codebase for the FemtoEmacs Editor [8], [9] which was a collaboration between Hugh Barney, Ed Costa, ...  FemtoEmacs uses xxx Femtolisp LISP implementation as the basis for its extension language.  However the Femtolisp codebase is in excess of 12K line of code an fairly difficult to understand how to use it inside an embedded application.
* In late 2016 Hugh Barney decided to look for a smaller lisp implementation for Femto and settled on Tiny-Lisp[7] by Mattias Pirstitz.
* Zepl was an initial project that established the suitability of Tiny-Lisp for use within an Emacs type editor.

## Femto Extensions

* grep - enables searching for text in files and loading of the files at the location of the match into the editor.
* bufmenu - the classic Emacs buffer menu
* oxo - a basic implementation of tick-tack-toe that runs in the Editor.

## Comparisons with Other Emacs Implementations

    Editor         Binary   BinSize     KLOC  Files

    femto          femto      43397     2.1k     11
    atto           atto       33002     1.9k     10
    pEmacs         pe         59465     5.7K     16
    Esatz-Emacs    ee         59050     5.7K     14
    GNOME          GNOME      55922     9.8k     13
    Zile           zile      257360    11.7k     48
    Mg             mg        585313    16.5K     50
    uEmacs/Pk      em        147546    17.5K     34
    Pico           pico      438534    24.0k     29
    Nano           nano      192008    24.8K     17
    jove           jove      248824    34.7k     94
    Qemacs         qe        379968    36.9k     59
    ue3.10         uemacs    171664    52.4K     16
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

## List Function Interface
```lisp

;;
;; cursor movement
;;

(forward-char)                          ;;
(forward-word)                          ;;
(forward-page)                          ;;
(backward-char)                         ;;
(backward-word)                         ;;
(backward-page)                         ;;
(next-line)                             ;;
(previous-line)                         ;; 
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

(get-buffer-count)                      ;;
(get-buffer-name)                       ;;
(select-buffer)                         ;;
(kill-buffer)                           ;;
(rename-buffer)                         ;;
(list-buffers)                          ;;
(find-file "file.txt")                  ;; xxx
(save-buffer)                           ;; saves the current buffer to disk

;;
;; window handling
;;

(delete-other-windows)                  ;;
(other-window)                          ;;
(split-window)                          ;;

;;
;; cut, copy, paste and the clipboard
;;

(set-mark)                              ;; sets the mark at the current point in the buffer
(copy-region)                           ;; copies the current region into the clipboard
(kill-region)                           ;; kills the current region and copies it into the clipboard
(yank)                                  ;; pastes the clipboard into the current buffer
(get-clipboard)                         ;; returns the contents of the clipboard as a string
(set-clipboard var)                     ;; sets up clipboard with contents of string var
(delete)
(backspace)

;;
;; keyboard handling
;;

(get-char)                              ;; return the character at the current position in the file
(get-key)                               ;; wait for a key press, return the key or "" if the key was a command key
(get-key-name)                          ;; return the name of the key pressed eg: c-k for control-k.
(get-key-funcname)                      ;; return the name of the function bound to the key
(getch)                                 ;; calls the c function getch and returns the keystroke
(set-key "key-name" "(lisp-func)")      ;; binds a key to a lisp function, see keynames see "Keys Names below"

;;
;; string handling
;; 

(string? symbol)                        ;; return true if symbol is a string
(string.length)                         ;;
(string.ref)                            ;;
(string.trim)                           ;;
(string.append "string1" "string2")     ;; concatenate 2 strings returning a new string
(string.substring string n1 n2)         ;; return a substring of string from ref n1 to n2
(string->number s)                      ;; return a number converted from the string, eg "99" => 99
(number->string n)                      ;; return a strung representation of the number, eg 99.56 => "99.56"

;;
;; number handling
;;

(number->string)                        ;;
(number?)                               ;;
(ascii)                                 ;;
(ascii->number)                         ;;

;;
;; interaction with the user
;;

(message)                               ;;
(clear-message-line)                    ;;
(prompt "prompt" "initial response")    ;; prompts for a value on the command line and returns the response
(show-prompt)                           ;; display the prompt and response but do not go into editing mode of the response
(display)                               ;; calls the display function so that the screen is updated
(refresh)

;;
;; lisp interaction
;;

(load "filename")                       ;; load and evaluate the lisp file
(eval-block)                            ;; passes the marked region to be evaluated by lisp, displays the output
log-message                             ;;
log-debug                               ;;

;;
;; miscellaneous
;;

(insert-string "string")                ;; insert the string into the buffer at the current location
(search-forward "accelerate")           ;; search forward from the point value passed in for the string supplied
search-backwards                        ;;
get-version-string                      ;;
shell-command                           ;;
os.getenv                               ;;
add-mode-global                         ;;
exit                                    ;;


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
  Femto code is released to the public domain.
  hughbarney@gmail.com November 2017

## References
    [0] Atto Emacs - https://github.com/hughbarney/atto
    [1] Perfect Emacs - https://github.com/hughbarney/pEmacs
    [2] Anthony's Editor - https://github.com/hughbarney/Anthony-s-Editor
    [3] MG - https://github.com/rzalamena/mg
    [4] Jonathan Payne, Buffer-Gap: http://ned.rubyforge.org/doc/buffer-gap.txt
    [5] Anthony Howe,  http://ned.rubyforge.org/doc/editor-101.txt
    [6] Anthony Howe, http://ned.rubyforge.org/doc/editor-102.txt
    [7] Tiny-Lisp,  https://github.com/matp/tiny-lisp
    [8] FemtoEmacs, https://github.com/FemtoEmacs/Femto-Emacs
    [9] FemtoEmacs, https://github.com/hughbarney/Femto-Emacs

