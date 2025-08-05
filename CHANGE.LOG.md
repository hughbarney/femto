## Femto 2.23/fLisp 0.5 Aug 2025

Added cmode, single / double quote matching, single line comments
Setup default mode for basic syntax highlighting text, symbols and numbers

## Femto 2.22/fLisp 0.5 Aug 2025

Added toggle-text-mode as start of being able to have different syntax high light
behaviour in different buffers

Fixed reporting of 'Line 0, not found' when files are loaded

## Femto 2.21/fLisp 0.5 May 2025

Fix remaining gc issues with macros and re-instate and macro.


## Femto 2.21/fLisp 0.4 May 2025

The fLisp interpreter has been refactored. Code has been moved around
and put into order. The GC tracing code has been changed and does not
use the heap anymore. GC tracing has been reduced as much as
possible. The interpreter does not rely on global variables anymore,
everything is hold in a single `Interpreter` struct.

Garbage collection has been stressed extensively and no segfault has
been experienced anymore.


## Femto 2.21 28 October 2024

This release introduces a separation between the fLisp interpreter and
the Femto editor.  A simple Lisp command line interpreter can be built
as `flisp`.

The editor extensions are extracted out of lisp.c and included on
demand.  lisp.c code has been completely reordered and follows
roughly the order: memory, reader, evaluation, writer, primitives,
interpreter.

Lisp initialization allocates memory dynamically, it receives now the
input and output stream as parameters and exposes them in Lisp as
`STDIN` and `STDOUT`.

Lisp primitives `fread`, `write` and `gc` have been added, `load`,
`print` and `printc` are moved out into the `core` and `stdlib` Lisp
library.  `:` has been added to the syntax, to allow for keyword
arguments.

Both inline documentation and the fLisp manual have been extended.
The latter includes information on the Lisp libraries and embedding of
fLisp in C code.

The Lisp tests have been reactivated and use the `flisp` interpreter
instead of the `femto` binary.  The Lisp debug facility has been
stabilized and is used in more places.

Some potential buffer overflow and missing initialization has been
fixed.


## Femto 2.20 18 October 2024

fLisp input/output is now based on libc file I/O instead of the
original Stream emulation.  The interpreter is extended with a stream
object type. The load_file() function has been eliminated, the (load)
function switches streams instead.

The Lisp reader now does overflow/underflow checking of numbers.

The build system allows to enable interpreter extensions at build
time, the editor functions are available via the
FLISP_EDITOR_EXTENSION and there is a demo FLISP_FILE_EXTENSION, which
exposes the stream functionality to Lisp.

A simplistic standalone Lisp interpreter can be built via the `flisp`
target and a README.flisp.md file is provided.

Code has been extensively documented in the source.


## Femto 2.19 11 October 2024

Change in the public C interface of the fLisp interpreter, which
allows for improved error handling.  The rework also resolved some
segmentation fault issues and uncovered a (not yet fixed) bug in the
garbage collector.  To mitigate, the default object memory size is
increased to 4 MByte.

The documentation is corrected and vastly extended.


## Femto 2.18 27 March 2024

This version features a major refactoring of the Lisp related code,
leading to a deviation from Tiny-Lisp. Since femto-lisp
is already taken by a not-so-femto Lisp implementation we call this
new dialect fLisp.

Major visible changes are:
* Core Lisp functionality is now closer to Emacs Elisp and Common Lisp and
  less Scheme'ish.
* Lisp defun's formerly compiled into the femto binary are moved out
  into the `femto.lsp` and `stdlib.lsp` startup file. These are not
  automatically loaded in batch mode.

fLisp counts with an initial test suite runnable from the `makefile`
and the Lisp documentation has been complemented to cover all callable
functions and factored out into its own file.

Some memory leaks and exceptions have been detected and fixed, both in
Tiny-Lisp as in femto code.


## Femto 2.17 08 December 2023
* When the environment variable FEMTO_BATCH exists, femto runs in batch mode
* When the environment variable FEMTO_DEBUG exists, femto writes some internal logging to debug.log
* When the environment variable FEMTORC is set, femto interprets its
  value as the path to the init file and tries to load it instead of
  the default init_file.
* A new primitive (signal 'error-symbol error-details) has been
  added. It throws an exception. The interface is built analogous to
  Elisp. It was just used for testing, but might be useful in the
  future anyway.

## Femto 2.16 08 December 2023
* code reformatted into K&R using 4 spaces for indents
* version number bumped but no functional changes

## Femto 2.15 25 November 2023
* fixed compiler warnings

## Femto 2.14 25 November 2023
* introduced shell-exec and rewrote shell-command in lisp

## Femto 2.13 23 November 2023
* added info.lsp to support info screen, invoked by C-x h
* made describe-bindings, describe-functions callable through lisp

## Femto 2.12 22 November 2023
* elegant command line argument handling past to lisp for processing - kudos and thanks to jorge-leon 

## Femto 2.11 23 August 2020
* used the Atto solution for jumping to eof and redisplay

## Femto 2.10 20 January 2018
* fixed lnend() behavior at end of file

## Femto 2.9 16 January 2018
* fixed kill-to-eol, no longer starts to delete backwards if at end of file

## Femto 2.8 15 January 2018
* fixed registration of commands for use by esc-x execute-command. These are now held in a dynamic list which means new command created and bound to keys will get registered and be available to be called at the command line. All lisp extension packages have had code updated so that functions not longer have the () around the names.

## Femto 2.7 13 January 2018
* fixed display issue (jumping to eof and redisplay) when paging down or deleting lines on last screen of the file (issue present in Anthons Editor).

## Femto 2.6 11 January 2018
* fixed undo so that it handles replaced text
* fixed creation of c-x ` key

## Femto 2.5 10 Demember 2017
* updated lisp function reference
* retored kill-to-eol back to femto.rc, added grep.lsp
* added (prompt) and (show_prompt), grep.lsp invoke with 'c-x g' and grep-next 'c-x `', added key definitions for "c-x a" to "c-x z"
* Added rename-buffer, get-version-string, added dired.lsp
* added shell-command, set-clipboard bufmenu.lsp restore clipboard on exit, added delete-next-word delete-previous-word
* removed hltest.c from src directory, updated TODO.txt
* added clear-message-line, forward-word, forward-page, backward-page, backward-word
* added esc-.; some changes to femto.rc; started testfunc.lsp; started prompt.lsp; wrote list examples

## Femto 2.4 7 December 2017
* added (get-buffer-name) (string.ref) (string.trim), fixed bufmenu.lsp, removed c code for string_trim().

## Femto 2.3 5 December 2017
*  added (save-buffer) (delete-buffer) (select-buffer) (search-forward) (search-backward) (find-file)

## Femto 2.2 1 December 2017
* added (delete-other-windows) (list-buffers) (split-window) (other-window) (get-clipboard) (get-buffer-count)

## Femto 2.1 30 November 2017
* added tiny_lisp support
* added ability to configure keyboard using (set-key)
* load femto.rc file

## Femto 2.0 29 November 2017
* brought up to date with FemtoEmacs undo, Atto highlighting, Atto UTF8 fixes
* fix compile warning around stdup define _XOPEN_SOURCE 500
* file headers changed to reference 'femto'
* removed old undoset code
* added undo functionality, set global_undo_mode = 1, so undo active by default
* added Atto style basic syntax highlighting
* added in atto fixes for UTF8, wide character display

## Femto 1.8 12 April 2016
* fixed UTF8 support so that it moves up and down lines, correctly maintaining column

## Femto 1.7 22 August 2016
* fixed bug when pasting any empty clipboard

## Femto 1.6 21 August 2016
* fixed defect with killbuffer when creating a new scratch buffer

## Femto 1.5 20 June 2016
* Added automatic matching of parenthesis {}() and []
* Fixed bug where a new file created when file not found did not have a buffer created

## Femto 1.4 13 June 2016
* Initialised buffer name to empty string
* Added basic colour scheme

## Femto v1.3 12 June 2016
* Fixed defect with paste command introduced in v1.1
* Added messages on copy and cut to show how many bytes

## Femto v1.2 3 June 2016
* Added UTF8 support

## Femto v1.1 31 May 2016
* Added list-buffers C-x C-b
* fixed problem of opening up multiple output windows
* refactored paste.  Paste now calls insert-string with contents of scrap

## Femto v1.0 29 May 2016
* Added filename completion (use TAB to complete)
* Added shell-command (C-x @), output is read into a buffer
