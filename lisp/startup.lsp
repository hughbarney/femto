;; Femto startup

(defun show-startup-message()
  (cond ((eq "*scratch*" (get-buffer-name))
	 (insert-string "\n\n\n\n")
	 (insert-string "  /**********************************\n")
	 (insert-string "\n\n")
	 (insert-string "   / _| ___ _ __ ___ | |_ ___     \n")
	 (insert-string "  | |_ / _ \ '_ ` _ \| __/ _ \    \n")
	 (insert-string "  |  _|  __/ | | | | | || (_) |   \n")
	 (insert-string "  |_|  \___|_| |_| |_|\__\___/'    \n\n  ")
	 (insert-string "\n\n")
	 (insert-string "  C-x h   for help\n\n")
	 (insert-string "  **********************************/\n\n\n\n")
	 (insert-string "  Tiny Emacs clone with Tiny-Lisp extension language\n  ")
	 (insert-string (get-version-string))
	 (insert-string "\n\n\n")
	 (end-of-buffer))))

(defun getopts (opts pos)
  (setq o (car opts))
  (cond
    ((eq nil o))
    ((eq o "+") (getopts (cdr opts) 0))
    ((eq (string.ref o 0) "+")
     (getopts (cdr opts) (string->number (string.substring o 1 (- (string.length o) 1)))))
    (t
     (find-file o)
     (goto-line pos)
     (getopts (cdr opts) 0))))

(defun confn(fn)
  (concat ~ "/" config_dir "/" fn))

(defun edit-config()
  (find-file (confn config_file)))

(provide 'startup)

;;
;;  Load extensions
;;
(require 'flisp)
(require 'femto)
(require 'bufmenu)
(require 'dired)
(require 'grep)
(require 'defmacro)

;; Put oxo and git into your personal configuration
;; (require 'oxo)
;; Note: git segfaults if (required), can be (load-script)ed though
;; (load-script "git.lsp")

;; Note: segfaults when loaded directly
;; autoload with c-x h
(defun show-info ()
  (require 'info)
  (show-info))

;;
;;  Key Bindings, setkey is used to bind keys to user defined functions in lisp
;;

(set-key "c-x @" "shell-command") ;; femto
(set-key "esc !" "shell-command")
(set-key "c-x i" "insert-file")

(set-key "esc-right" "delete-next-word")
(set-key "esc-left" "delete-previous-word")
(set-key "c-k" "kill-to-eol")
(set-key "c-x ?" "describe-key")
(set-key "c-]" "find_and_eval_sexp")
(set-key "c-x c-o" "oxo")
(set-key "c-x c-b" "buffer-menu")
(set-key "c-x c-d" "dired")
(set-key "c-x c" "edit-config")
(set-key "c-x h" "show-info")
(set-key "c-x g" "grep-command")

(show-startup-message)

;; Load and edit user specific config
(setq
 config_dir ".config/femto"
 config_file "femto.rc")

;; Note: catch exception
;;(eq nil (load (confn config_file))) ; segfaults
(load "/home/jorge/.config/femto/femto.rc")
(log-debug "user config loaded\n")
(getopts argv 0)

