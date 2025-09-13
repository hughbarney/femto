;; Femto startup

(defun show-startup-message()
  (cond ((eq "*scratch*" (get-buffer-name))
	 (insert-string "\n\n\n\n")
	 (insert-string " ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")
	 (insert-string " ;\n")
	 (insert-string " ;\n")
	 (insert-string " ;\n")
	 (insert-string " ;  / _| ___ _ __ ___ | |_ ___     \n")
	 (insert-string " ; | |_ / _ \ '_ ` _ \| __/ _ \    \n")
	 (insert-string " ; |  _|  __/ | | | | | || (_) |   \n")
	 (insert-string " ; |_|  \___|_| |_| |_|\__\___/'    \n")
	 (insert-string " ;\n")
	 (insert-string " ;\n")
	 (insert-string " ;\n")
	 (insert-string " ;\n")
	 (insert-string " ; C-x h   for help\n")
	 (insert-string " ;\n")
	 (insert-string " ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n\n\n\n")
	 (insert-string "  Tiny Emacs clone with Tiny-Lisp extension language\n  ")
	 (insert-string (get-version-string))
	 (insert-string "\n\n\n")
	 (end-of-buffer))))

(defun getopts (opts pos)
  (cond
    ((null opts))
    ((consp opts)
     (cond
       ((string-equal "-" (substring (car opts) 0 1))
	(throw wrong-type-argument "(getopts opts pos) - unknown option" (car opts)) )
       ((eq "+" (car opts)) (getopts (cdr opts) 0))
       ((eq "+" (substring (car opts) 0 1))
	(getopts (cdr opts) (string-to-number (substring (car opts) 1))))
       (t
	(find-file (car opts))
	(cond ((> pos 0) (goto-line pos)))
	(getopts (cdr opts) 0))))
    (t (throw wrong-type-argument "(getopts opts pos) - opts must be list"))))

;; Load and edit user specific config
(setq
 config_dir ".config/femto"
 config_file "femto.rc")

(defun confn(fn)
  (concat (getenv "HOME") "/" config_dir "/" fn))

(defun edit-config()
  (find-file (confn config_file)))

(defun log (level result . message)
  (cond (result
	 (log-debug (concat  level":"(car result)": "message" '"(caddr result)"' - "(cadr result)"\n")) )
	 (t (log-debug (concat level": "message"\n"))) ))

(provide 'startup)

;;
;;  Load extensions
;;
(require 'defmacro)
(require 'bufmenu)
(require 'dired)
(require 'grep)
(require 'git)

(defun oxo ()
  ;; autoload info with c-x c-o
  (require 'oxo)
  (oxo) )

(defun show-info ()
  ;; autoload info with c-x h
  (require 'info)
  (show-info) )

;;
;;  Key Bindings, setkey is used to bind keys to user defined functions in lisp
;;

(set-key "c-t" "transpose-chars")
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
(set-key "c-x c-g" "git-menu")

(show-startup-message)

;;
;; This is used to set modes for different types of file when they get loaded
;;
(defun read-hook (s)
  (cond
    ((string-search (get-buffer-file-extension) "|c|h|cpp|") (add-mode "cmode"))
    ((string-search (get-buffer-file-extension) "|rc|lsp|")  (add-mode "lispmode"))
    ((string-search (get-buffer-file-extension) "|py|")      (add-mode "python"))))

;;
;; Mark the scratch buffer as unmodified, set to lispmode so that the comment comes up green
;;
(add-mode "lispmode")
(delete-mode "modified")

;;
;; Try to load the user rc file
;;
(let ((rcfile (confn config_file)) (result nil))
  (setq result (catch (fstat rcfile)))
  (cond ((car result) (log 'ERROR result "locating rc file"))
	(t
	 (cond ((car (setq result (catch (load rcfile))))
		(log 'ERROR result "rc file not loaded"))
	       (t (log 'NOTICE nil "rc file '"rcfile"' loaded\n")) ))))

;;
;; Try to parse the commandline arguments
;;
(let ((result (catch (getopts argv 0))))
  (cond ((car result) (log 'ERROR result "parsing command line"))))
