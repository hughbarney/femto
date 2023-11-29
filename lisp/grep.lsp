;;
;; simple interface to the grep command
;; invoke using "c-x g"
;; visit next match using "c-x `"
;;

(require 'flisp)

(setq grep-buf "*grep*")
(setq grep-line 1)
(setq grep-cmd "grep -ni")
(setq grep-search "")
(setq grep-search-files "*.c *.h")

(defun grep-command()
  (kill-buffer grep-buf)
  (setq grep-line 0)
  (setq grep-search (prompt "grep search for: " grep-search))
  (if (> (string.length grep-search) 0)
  (progn
    (setq grep-search-files (prompt "grep search files: " grep-search-files))
    (setq grep-query (concat grep-cmd " " grep-search " " grep-search-files))
    (shell-command grep-query)
    (set-key "c-x `" "grep-next")
    (rename-buffer grep-buf))))

(defun grep-next()
  (select-buffer grep-buf)
  (setq grep-line (+ grep-line 1))
  (goto-line grep-line)
  (beginning-of-line)
  (set-mark)
  (search-forward ":")
  (backward-char)
  (copy-region)
  (setq grep-fname (get-clipboard))
  (forward-char)
  (set-mark)
  (search-forward ":")
  (backward-char)
  (copy-region)
  (setq grep-match-line (string->number (get-clipboard)))
  (find-file grep-fname)
  (goto-line grep-match-line))

(provide 'grep)
