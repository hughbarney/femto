;;
;; Simple version of magit for Femto
;; Not yet finnished
;; 
;; use the arrow keys to move up or down the list
;; then select one of the following letters
;;
;; Suggested first implementation does the following
;;   s - stage the file
;;   c - commit the stage files
;;   u - refresh the status
;;   x - exit
;;
;; (load_script "git.lsp")   ;; to load
;; (git)                     ;; to call
;;

(setq git-status-cmd "git status --porcelain")
(setq git-buffer "*git*")
(setq git-commit-buffer "*commit*")
(setq git-obuf "*scratch*")
(setq out-buffer "*output*")
(setq git-line 1)
(setq git-name "")
(setq git-commit-file "")
(setq git-start-line 1)
(setq git-last-line 1)
(setq git-status1 ".")
(setq git-status2 ".")
(setq git-status "[..]")
(setq git-ops 0)
(setq git-max-ops 300)

(defun git-menu()
  (delete-other-windows)
  (setq git-obuf (get-buffer-name))
  (kill-buffer git-buffer)
  (shell-command git-status-cmd)
  (rename-buffer git-buffer)
  (beginning-of-buffer)
  (git-init)
  (git-get-last-line)
  (beginning-of-buffer)
  (git-get-info)
  (git-loop))

(defun git-init()
  (setq git-start-line 1)
  (setq git-last-line 1)
  (setq git-line 1)
  (setq git-ops 0)
  (setq git-name "")
  (setq git-status1 ".")
  (setq git-status2 ".")
  (setq git-status "[..]")
  (select-buffer git-buffer)
  (end-of-buffer)
  (previous-line)
  (beginning-of-line))

(defun git-get-last-line()
  (setq git-last-line 1)
  (end-of-buffer)
  (previous-line)
  (beginning-of-line)
  (git-count-line))

(defun git-count-line()
  (if (> (get-point) 0)
  (progn
    (setq git-last-line (+ 1 git-last-line))
    (previous-line)
    (beginning-of-line)
    (git-count-line))))

(defun git-loop()
  (setq git-ops (+ git-ops 1))
  (if (< git-ops git-max-ops)
  (progn
    (message (concat git-status "(" git-name ") git menu: c,s,u,x"))
    (update-display)
    (setq git-key (get-key))
    (if (eq git-key "")
      (git-handle-arrow-key)
      (git-handle-command-key git-key))
    (git-loop))))

(defun git-handle-arrow-key()
  (setq git-key (get-key-funcname))
  (if (eq git-key "(previous-line)") (git-move-line -1))
  (if (eq git-key "(next-line)") (git-move-line 1))
  (git-get-info))


(defun git-handle-command-key(k)
  (if (eq k "x")
  (progn
    (select-buffer git-obuf)
    (kill-buffer git-buffer)
    (setq git-ops (+ git-max-ops 1))))
  (if (eq k "s")
  (progn
    (shell-command (concat "git add " git-name))
    (kill-buffer out-buffer)
    (git-menu)))
  (if (eq k "c")
  (progn
    (if (eq "commit" (git-get-commit-string))
      (shell-command (concat "git commit -F " git-commit-file)))
    (git-menu)))
  (if (eq k "u")
  (progn
    (shell-command (concat "git reset HEAD " git-name))
    (kill-buffer out-buffer)
    (git-menu))))

;;
;; not yet complete, need to write temp file so we can use it for the 
;; commit comments, not yet figired how to make this uniq and manage the file
;;
(defun git-get-commit-string()
  (kill-buffer git-commit-buffer)
  (split-window)
  (select-buffer git-commit-buffer)
  (message "c-c c-c to commit, c-c c-q to cancel")
  (update-display)
  (setq r (get-commit-key))
  (if (eq r "commit")
    (progn
      (setq git-commit-file (get-temp-file))
      (select-buffer git-commit-buffer)
      (beginning-of-buffer)
      (set-mark)
      (end-of-buffer)
      (copy-region)
      (find-file git-commit-file)
      (yank)
      (save-buffer (get-buffer-name))
      (kill-buffer (get-buffer-name))
      (kill-buffer git-commit-buffer)
      "commit")
    (progn
      "cancel")))

(defun get-commit-key()
  (setq k (get-key))
  (if (eq k "")
  (progn
    (setq git-key (get-key-funcname))
    (cond
      ((eq git-key "(cc-commit)") "commit")
      ((eq git-key "(cc-cancel)") "cancel")
      (t (get-commit-key))))
  (progn
    (insert-string k)
    (update-display)
    (get-commit-key))))

(defun git-move-line(n)
  (setq git-line (max git-start-line (min (+ git-line n) git-last-line))))

(defun git-get-info()
  (goto-line git-line)
  (beginning-of-line)
  (setq git-status1 (get-char))
  (forward-char)
  (setq git-status2 (get-char))
  (forward-char)
  (forward-char)
  (set-mark)
  (setq p (get-point))
  (end-of-line)
  (copy-region)
  (set-point p)
  (setq git-status (concat "[" (git-status-to-text git-status1) ":" (git-status-to-text git-status2) "]"))
  (setq git-name (string.trim (get-clipboard))))

;;
;; nice lookup table for git status
;;

(defun git-status-to-text(s)
  (cond
   ((eq s "M") "modified")
   ((eq s "A") "added")
   ((eq s "D") "deleted")
   ((eq s "R") "renamed")
   ((eq s "C") "copied")
   ((eq s "U") "updated")
   ((eq s "?") "untracked")
   ((eq s " ") "")
   (t          "unknown")))

;;
;; setup key binding
;;
(set-key "c-x c-g" "(git-menu)")
(set-key "c-c c-c" "(cc-commit)")
(set-key "c-c c-q" "(cc-cancel)")

