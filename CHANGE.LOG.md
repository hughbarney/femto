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
