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
(setq git-obuf "*scratch*")
(setq git-line 1)
(setq git-name "")
(setq git-start-line 1)
(setq git-last-line 1)
(setq git-is-dir nil)
(setq git-is-link nil)
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
    (message (concat "(" git-name ") git menu: s,c,x"))
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
        (git-menu)))
   (if (or (eq k "f") (eq k "\n"))
   (progn
        (if git-is-dir (git-open-dir) (git-open-file))
        (kill-buffer git-buffer)
        (setq git-ops (+ git-max-ops 1)))))

(defun git-open-file()
  (find-file (concat git-dir "/" git-name)))

(defun git-open-dir()
  (if (eq ".." git-name)
  (progn
    (setq git-dir (git-up-dir git-dir)))
  (progn
    (setq git-dir (concat git-dir "/" git-name))))
  (git))

(defun git-move-line(n)
  (setq git-line (max git-start-line (min (+ git-line n) git-last-line))))

(defun git-get-info()
  (goto-line git-line)
  (beginning-of-line)
  (repeat 2 forward-char)
  (setq ch (get-char))
  (setq git-is-dir (eq "d" ch))
  (setq git-is-link (eq "l" ch))
  (if git-is-link
  (progn
    (end-of-line)
    (search-backward " ")
    (search-backward " ")
    (search-backward " "))
  (progn
    (end-of-line)
    (search-backward " ")))
  (forward-char)
  (forward-char)
  (set-mark)
  (setq p (get-point))
  (if git-is-link (search-forward " ") (end-of-line))
  (copy-region)
  (set-point p)
  (setq git-name (string.trim (get-clipboard))))


;;
;; setup key binding
;;
(set-key "c-x c-g" "(git-menu)")
