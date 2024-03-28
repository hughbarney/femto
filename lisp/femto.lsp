;;
;; Basic Femto extensions
;;

(require 'flisp)

(defun repeat (n func)  
  (cond ((> n 0) (func) (repeat (- n 1) func))))

;; OS interaction
;; Note: this emulates the original femto shell-command.
;;   The move from C to Lisp allows implementation of more
;;   powerful system interaction in the future
(defun shell-command arg
  (cond
    (arg (setq command arg))
    (t (setq command (prompt-filename "Command: "))))
  (cond (command (shell-exec command))))

(defun shell-exec (command)
  (setq temp (get-temp-file))
  (setq rc (system (concat command " > " temp " 2>&1 <&-")))
  (cond
    ((eq command ""))
    ((or (eq rc -1) (eq rc 127))
	(concat "error: failed to execute" command ": "  rc))
    (t
     (cond (not (eq rc 0)) (message "warning: " command " exited: " rc))
     (select-buffer "*output*")
     (erase-buffer)
     (insert-file-contents-literally temp)
     (system (concat "rm -f " temp))
     (clear-message-line)
     )))

(defun insert-file ()
  (setq fn (prompt-filename "Insert file: "))
  (cond (fn (insert-file-contents-literally fn))))

;; trim all spaces from front of a string
(defun string.trim.front(s)
  (cond
    ((= 0 (string.length s)) "")
    ((not (eq (string.substring s 0 0) " ")) s)
    ((< (string.length s) 2) "")
    (t (string.trim.front (string.substring s 1 (- (string.length s) 1)))) ))

;; trim all spaces from back of a string
(defun string.trim.back(s)
  (setq p (- (string.length s) 1))
  (cond
    ((= p -1) "")
    ((not (eq (string.substring s p p) " ")) s)
    ((< (string.length s) 2) "")
    (t (string.trim.back (string.substring s 0 (- (string.length s) 2)))) ))

;; trim spaces off front and back of a string
(defun string.trim(s)
  (string.trim.back (string.trim.front s)))


;; delete next word
(defun delete-next-word()
  (backward-word)
  (forward-word)
  (set-mark)
  (forward-word)
  (kill-region))

;; previous word
(defun delete-previous-word()
  (forward-word)
  (backward-word)
  (forward-char)
  (set-mark)
  (backward-word)
  (backward-word)
  (forward-char)
  (kill-region))

(defun kill-to-eol()
  (cond
    ((eq (get-point) (get-point-max)) nil)
    ((eq "\n" (get-char)) (delete))
    (t
     (set-mark)
     (end-of-line)
     (cond ((eq (get-point) (get-mark)) (delete))
	   (t (kill-region))) )))

;; shrink string by dropping off last char
(defun shrink(s)
  (cond
    ((< (string.length s) 2) "")
    (t (string.substring s 0 (- (string.length s) 2)))  ))

;; some keystroke checks that we will use later
(defun is_ctl_g(k)
  (eq k (ascii 7)))

(defun is_escape(k)
  (eq k (ascii 27)))

(defun is_backspace(k)
  (or (eq k (ascii 8)) (eq k (ascii 127))))

(defun is_ctl_s(k)
  (eq k (ascii 19)))

(defun is_control_char(k)
  (and (>= (ascii->number k) 0) (<= (ascii->number k) 31)))


;; prompt for a keystroke then show its name
(defun describe-key()
  (show-prompt "Describe Key: " "")
  (setq key (get-key))
  (cond
    ((not (eq key "")) (message key))
    (t (message (concat (get-key-name) " runs command " (get-key-funcname))))))

;;
;; GNU Emacs style lisp interaction.
;; Place cursor behind an s-expression, type C-] and the
;; block will be evaluated.
;;

;; find the end of the s-expression and set cursor on next cell
(defun find_end_p()
  (setq k (get-char))
  (cond 
    ((eq 0 (get-point)) -1)
    ((eq ")" k) (forward-char) (get-point))
    ((or (eq "" k) (eq " " k) (eq "\t" k) (eq "\n" k)) (backward-char) (find_end_p))
    (t -1) ))

;; find the start of the s-expression
;; assumes start is always in first character of line
;; this means comments and strings dont need to be handled
(defun find_start_p()
  (beginning-of-line)
  (setq kyy (get-char))
  (cond 
    ((and (eq 0 (get-point)) (not (eq kyy "("))) -1) 
    ((eq kyy "(") (get-point))
    (t (previous-line) (find_start_p)) ))

;;
;; find the start and end of the s-expression
;; set the mark and the start and point at the end
;; call eval-block
(defun find_and_eval_sexp()
  (setq o_point (get-point))
  (setq lb_count 0)
  (setq rb_count 0)
  (setq start_p -1)
  (setq end_p (find_end_p))
  (cond ((> end_p -1) (setq start_p (find_start_p))))
  (cond
    ((and (> start_p -1) (> end_p -1))
     (set-point start_p)
     (set-mark)
     (set-point end_p)
     (eval-block))
    (t
     (set-point o_point)
     (cond
       ((eq -1 start_p) (message "could not find start of s-expression"))
       ((eq -1 end_p) (message "could not find end of s-expression"))) )))

(provide 'femto)
