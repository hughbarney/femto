;;
;; Simple version of magit for Femto
;;
;; (load_script "git.lsp")   ;; to load
;; (git-menu)                ;; to call
;;


(setq git-help-string
"
Simple version of magit for Femto
Hugh Barney, Jan 2018

When invoked the output of 'git status --porcelain' is displayed
This shows a list of untracked and unstaged changes
The first column shows the status of any staged changes
The second column shows the status of any unstaged changes

----staged chages
|-----unstaged changes
SU
----------sample window---------
 M examples/git.lsp
?? dd
?? log.txt
?? ww

-------------------------------

The key to the file status in the first and second columns is:

 M  modified 
 A  added 
 D  deleted 
 R  renamed 
 C  copied 
 U  updated 
 ?  untracked 


Use the Up and Down arrow keys to move up or down the list of files
then select one of the following letters to operate on the file

  s - stage the file
  c - commit the stage files
  u - un-stage the file
  d - diff the changes (git style)
  x - exit
  p - push to master

")

(setq git-status-cmd "git status --porcelain")
(setq git-buffer "*git*")
(setq git-commit-buffer "*commit*")
(setq git-diff-buffer "*git-diff*")
(setq git-help-buffer "*help*")
(setq git-obuf "")
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
  (if (eq git-obuf "") (setq git-obuf (get-buffer-name)))
  (kill-buffer git-buffer)
  (shell-command git-status-cmd)
  (rename-buffer git-buffer)
  (beginning-of-buffer)
  (git-init)
  (git-get-last-line)
  (beginning-of-buffer)
  (git-get-info)
  (git-loop)
  (setq git-obuf ""))

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
    (message (concat git-status "(" git-name ") git menu: c,d,s,u,p,x h=help"))
    (update-display)
    (setq git-key (get-key))
    (if (eq git-key "")
      (git-handle-arrow-key)
      (git-handle-command-key git-key))
    (git-loop))))

(defun git-handle-arrow-key()
  (setq git-key (get-key-funcname))
  (if (eq git-key "previous-line") (git-move-line -1))
  (if (eq git-key "next-line") (git-move-line 1))
  (git-get-info))

(defun git-handle-command-key(k)
  (cond
    ((or (eq k "x") (eq k "q"))
      (select-buffer git-obuf)
      (kill-buffer git-buffer)
      (setq git-ops (+ git-max-ops 1)))
    ((eq k "s")
      (if (eq git-status2 "D") (setq git-minus-u "-u ") (setq git-minus-u "")) 
      (shell-command (concat "git add " git-minus-u git-name))
      (kill-buffer out-buffer)
      (git-menu))
    ((eq k "c")
      (if (eq "commit" (git-get-commit-string))
        (shell-command (concat "git commit -F " git-commit-file)))
      (git-menu))
    ((eq k "p")
      (message "pushing commits to master ...")
      (update-display)
      (shell-command "git push -v origin master")
      (select-buffer out-buffer)
      (message "output of push command, press space to continue")
      (update-display)
      (get-key)
      (git-menu))
    ((eq k "u")
      (shell-command (concat "git reset HEAD " git-name))
      (kill-buffer out-buffer)
      (git-menu))
    ((eq k "h")
      (git-help))
    ((eq k "d")
      (git-diff))))

(defun git-diff()
  (shell-command "git diff")
  (select-buffer out-buffer)
  (rename-buffer git-diff-buffer)
  (message "git diff output, navigate or 'q' to quit")
  (update-display)
  (view-file-to-quit)
  (kill-buffer git-diff-buffer)
  (git-menu))

(defun git-help()
  (setq buf (get-buffer-name))
  (select-buffer git-help-buffer)
  (insert-string git-help-string)
  (beginning-of-buffer)
  (message "help, 'q' to quit")
  (update-display)
  (view-file-to-quit)
  (select-buffer buf)
  (delete-other-windows)
  (kill-buffer git-help-buffer)
  (git-menu))

;;
;; split window and get commit comments, write to a file or abort
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
  (update-display)
  (setq k (get-key))
  (if (eq k "")
  (progn
    (setq git-key (get-key-funcname))
    (cond
      ((eq git-key "cc-commit") "commit")
      ((eq git-key "cc-cancel") "cancel")
      (t (execute-key) (get-commit-key))))
  (progn
    (insert-string k)
    (get-commit-key))))


(defun view-file-to-quit()
  (setq k (get-key))
  (cond
    ((eq k "q") t)
    ((eq k "")
      (exec-view-key)
      (view-file-to-quit))
    (t (view-file-to-quit))))

(defun exec-view-key()
  (setq f (get-key-funcname))
  (if (or 
    (eq f "page-down") 
    (eq f "page-up")
    (eq f "next-line")
    (eq f "previous-line")
    (eq f "beginning-of-buffer")
    (eq f "end-of-buffer"))
 (progn 
   (execute-key)
   (update-display))))

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
;; setup key bindings
;;
(set-key "c-x c-g" "git-menu")

;; used during commit pop up window
(set-key "c-c c-c" "cc-commit")
(set-key "c-c c-q" "cc-cancel")

