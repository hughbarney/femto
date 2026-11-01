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
(setq de-line 1)
(setq de-line-start 0)
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
  ;; trailing "/" makes ls list the contents of a symlinked directory
  (shell-command (concat dired-ls-cmd (de-shell-quote (concat dired-dir "/"))))
  (rename-buffer dired-buffer)
  (beginning-of-buffer)
  (set-mark)
  (goto-line 3)
  (kill-region)
  (de-init)
  (beginning-of-buffer)
  (de-get-info)
  (de-loop))


;; Note: dired works on logical lines (goto-line, search-forward).
;; beginning-of-line, end-of-line and previous-line move by screen
;; row, which breaks on listing lines wider than the terminal.
(defun de-init()
  (setq de-start-line 1)
  (setq de-line 1)
  (setq de-ops 0)
  (setq de-name "")
  (select-buffer dired-buffer)
  (beginning-of-buffer)
  (setq de-last-line (de-count-newlines 0))
  (de-indent-lines 1))

;; count the remaining newlines from point
(defun de-count-newlines (n)
  (cond
    ((search-forward "\n") (de-count-newlines (+ n 1)))
    (t n)))

;; indent listing lines n .. de-last-line by two spaces
(defun de-indent-lines (n)
  (cond
    ((<= n de-last-line)
     (goto-line n)
     (insert-string "  ")
     (de-indent-lines (+ n 1)))))

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
       (de-is-link
        (cond ((de-target-is-dir) (de-open-dir))
              (t (de-open-file))))
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

;; Parse the `ls -la` line de-line:
;;   perms links owner group size month day time|year name[ -> target]
;; The name is everything after the 8th field, so names and link
;; targets may contain spaces.
(defun de-get-info()
  (setq de-text (de-line-text de-line))
  (setq de-type (de-skip-spaces de-text))
  (setq de-type (cond ((eq de-type "") "") (t (substring de-type 0 1))))
  (setq de-is-dir (eq "d" de-type))
  (setq de-is-link (eq "l" de-type))
  (setq de-rest (de-drop-fields de-text 8))
  (setq de-arrow (cond (de-is-link (string-search " -> " de-rest))))
  (setq de-name (cond (de-arrow (substring de-rest 0 de-arrow)) (t de-rest)))
  ;; leave the cursor on the start of the name
  (set-point (+ de-line-start (- (string-length de-text) (string-length de-rest)))))

;; text of logical line n, without its newline; sets de-line-start
(defun de-line-text (n)
  (goto-line n)
  (setq de-line-start (get-point))
  (set-mark)
  (cond ((search-forward "\n") (backward-char))
        (t (end-of-buffer)))
  (copy-region)
  (set-point de-line-start)
  (get-clipboard))

(defun de-skip-spaces (s)
  (cond ((eq s "") s)
        ((eq (substring s 0 1) " ") (de-skip-spaces (substring s 1)))
        (t s)))

(defun de-skip-word (s)
  (cond ((eq s "") s)
        ((eq (substring s 0 1) " ") s)
        (t (de-skip-word (substring s 1)))))

;; drop n space separated fields and the spaces after them
(defun de-drop-fields (s n)
  (cond ((= n 0) (de-skip-spaces s))
        (t (de-drop-fields (de-skip-word (de-skip-spaces s)) (- n 1)))))

;; quote s for /bin/sh: wrap in '...' and escape embedded quotes
(defun de-shell-quote (s)
  (concat "'" (de-quote-chars s) "'"))

(defun de-quote-chars (s)
  (cond ((eq s "") "")
        ((eq (substring s 0 1) "'") (concat "'\\''" (de-quote-chars (substring s 1))))
        (t (concat (substring s 0 1) (de-quote-chars (substring s 1))))))

;; does the symlink de-name point to a directory?  (test -d follows links)
(defun de-target-is-dir ()
  (= 0 (system (concat "test -d " (de-shell-quote (concat dired-dir "/" de-name))))))

;;
;; reduces a directory path by one sub-directory
;;
(defun de-up-dir(d)
  (cond
    ;; d is 1 char then we must be at root, return "/"
    ((eq 1 (length d)) "/")
    ;; if last char is / then we have reached the next sub-directory up, remove last char and return
    ((eq "/" (string-last-char d)) (string-shrink-left d))
    ;; otherwise call again but with last char removed
    (t (de-up-dir (string-shrink-left d))) ))


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
   ;; not end-of-line: that stops at the screen edge on long paths
   (cond ((search-forward "\n") (backward-char))
         (t (end-of-buffer)))
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
