;;
;; Dired extension for Femto
;;
;; 
;; use the arrow keys to move up or down the list
;; then select one of the following letters
;;
;; Suggested first implementation does the following
;;   f or CR - if a directory change to that directory and reload the ls output
;;   f or CR - open the file in the editor
;;   x       - exit dired
;;
;; This is just a start. Completion of this DIRED is left as challenge for the
;; reader.  For an example see the dired.cmd sample that was written for MicroEMACS
;;
;;
;;
;; (load "path/dired.lsp")   ;; to load
;; (dired)                   ;; to call
;;
;;
(require 'femto)


;;
(setq dired-dir "")
(setq dired-ls-cmd "ls -la ")
(setq dired-buffer "*dired*")
(setq de-obuf "*scratch*")
(setq de-name-start-col 47)
(setq de-dir-start-col 2)
(setq de-line 1)
(setq de-name "")
(setq de-start-line 1)
(setq de-last-line 1)
(setq de-is-dir nil)
(setq de-is-link nil)
(setq de-ops 0)
(setq de-max-ops 300)
(setq de-debug nil)

(defun dired ()
  (cond 
     ((eq dired-dir "") (setq dired-dir (get-cwd))))
  (delete-other-windows)
  (setq de-obuf (get-buffer-name))
  (kill-buffer dired-buffer)
  (shell-command (concat dired-ls-cmd dired-dir))
  (rename-buffer dired-buffer)
  (beginning-of-buffer)
  (set-mark)
  (goto-line 3)
  (kill-region)
  (de-init)
  (de-get-last-line)
  (beginning-of-buffer)
  (de-get-info)
  (de-loop))


(defun de-init()
  (setq de-start-line 1)
  (setq de-last-line 1)
  (setq de-line 1)
  (setq de-ops 0)
  (setq de-name "")
  (select-buffer dired-buffer)
  (end-of-buffer)
  (previous-line)
  (beginning-of-line)
  (de-insert-space))



(defun de-insert-space()
  (insert-string "  ")
  (previous-line)
  (beginning-of-line)
  (cond ((eq " " (get-char)))
	(t (de-insert-space))))

(defun de-get-last-line()
  (setq de-last-line 1)
  (end-of-buffer)
  (previous-line)
  (beginning-of-line)
  (de-count-line))

(defun de-count-line()
  (cond
    ((> (get-point) 0)
     (setq de-last-line (+ 1 de-last-line))
     (previous-line)
     (beginning-of-line)
     (de-count-line))))

(defun de-loop()
  (setq de-ops (+ de-ops 1))
  (cond
    ((< de-ops de-max-ops)
     (message "dired menu: f,x")
     (update-display)
     (setq de-key (get-key))
     (cond
       ((eq de-key "") (de-handle-arrow-key (get-key-funcname)))
       (t (de-handle-command-key de-key)))
     (de-loop))))

(defun de-handle-arrow-key(de-key)
  (cond
    ((eq de-key "previous-line") (de-move-line -1))
    ((eq de-key "next-line") (de-move-line 1)))
  (de-get-info))

(defun de-handle-command-key(k)
  (cond
    ((memq k '("x" "q"))
     (select-buffer de-obuf)
     (kill-buffer dired-buffer)
     (setq de-ops (+ de-max-ops 1)))
    ((memq k '("f" "\n"))
     (cond
       (de-is-dir (de-open-dir))
       (t (de-open-file)))
     (kill-buffer dired-buffer)
     (setq de-ops (+ de-max-ops 1)))
    (t (log-debug (concat "command key=" k "\n")))))


(defun de-open-file()
  (find-file (concat dired-dir "/" de-name)))

(defun de-open-dir()
  (dired-debug de-name)
  (cond
    ((eq ".." de-name) (setq dired-dir (de-up-dir dired-dir)))
    (t (setq dired-dir (concat dired-dir "/" de-name))))
  (dired))

(defun de-move-line(n)
  (setq de-line (max de-start-line (min (+ de-line n) de-last-line))))

(defun de-get-info()
  (goto-line de-line)
  (beginning-of-line)
  (repeat de-dir-start-col forward-char)
  (setq ch (get-char))
  (setq de-is-dir (eq "d" ch))
  (setq de-is-link (eq "l" ch))
  (cond
    (de-is-link
     (end-of-line)
     (search-backward " ")
     (search-backward " ")
     (search-backward " "))
    (t
     (end-of-line)
     (search-backward " ")))
  (forward-char)
  (forward-char)
  (set-mark)
  (setq p (get-point))
  (cond (de-is-link (search-forward " ")) (t (end-of-line)))
  (copy-region)
  (set-point p)
  (setq de-name (string-trim (get-clipboard))))

;;
;; reduces a directory path by one sub-directory
;;
(defun de-up-dir(d)
  (cond
    ((eq 1 (length d)) d)
    ((eq "/" (substring d (- (length d) 1) (- (length d) 1)))
     (substring d 0 (- (length d) 2)))
    (t (de-up-dir (substring d 0 (- (length d) 2))))))


;;
;; get current working directory, using let for lobal variables
;;
(defun get-cwd() 
   (let ((obuf (get-buffer-name))
        (current_working_directory ""))
   (shell-command "pwd")   
   (select-buffer "*output*")
   (beginning-of-buffer)
   (set-mark)
   (end-of-line)
   (copy-region)
   (setq current_working_directory (string-trim (get-clipboard)))    
   (select-buffer obuf)
   (kill-buffer "*output*")
   current_working_directory))

;;
;; keep this so we can debug dired if needed
;;
(defun dired-debug(m)
  (cond
    ((eq dired-debug t) (log-message (concat m "\n")))))



(provide 'dired)
