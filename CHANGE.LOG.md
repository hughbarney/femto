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
