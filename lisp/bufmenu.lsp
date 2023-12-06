;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; buffer menu extension for FemtoEmacs, written in Femtolisp with FemtoEmacs bindings
;; Hugh Barney September 2016
;;
;; when run produces a list of buffers
;; use the arrow keys to move up or down the list
;; then select one of 1,2,s,k,x
;;
;; 1 - switch to buffer as a single window
;; 2 - split screen and select buffer and original buffer
;; s - save buffer if modified
;; k - kill buffer
;; x - exit buffer menu
;;
;;
;; (load "examples/bufmenu.scm")
;; (buffer-menu)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The following Editor functions are needed to implement a buffer menu extension
;;
;; (gotoline 10)
;; (update-display)
;; (message "msg")
;; (beginning-of-line)
;; (forward-char)
;; (set-mark)
;; (copy-region)
;; (get-key-funcname)
;; 
;; (delete-other-windows)
;; (list-buffers)
;; (split-window)
;; (other-window)
;; (get-clipboard)
;; 
;; (save-buffer bufm-buf)
;; (get-buffer-count)
;; (get-buffer-name)
;; (search-forward "str")
;; (select-buffer "*scratch*"))
;; (string.trim)
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'femto)

(setq bufm-line 3)   
(setq bufm-start-line 3)
(setq bufm-last-line 3)
(setq bufm-ops 0)
(setq bufm-max-ops 400)
(setq bufm-debugging nil)
(setq bufm-stop nil)
(setq bufm-obuf "")
(setq bufm-oclip "")
(setq bufm-buf "")
(setq bufm-key "")

;;
;; (buffer-menu)
;;
;; List buffers, the starts on line 3.
;; (get-buffer-count) will report 1 additional buffer than listed
;; as *buffers* is hidden from the list when created
;; loop round (with a limit of 400 operations) until bufm-stop gets set to t
;;


(defun buffer-menu()
  (bufm-debug "buffer-menu")
  (setq bufm-ops 0)
  (setq bufm-obuf (get-buffer-name))
  (setq bufm-oclip (get-clipboard))
  (list-buffers)
  (setq bufm-line bufm-start-line)   
  (setq bufm-last-line (+ bufm-start-line (get-buffer-count)))
  (setq bufm-last-line (- bufm-last-line 2))
  (setq bufm-stop nil)
  (goto-line bufm-line)
  (delete-other-windows)
  (setq bufm-buf (bufm-get-bufn))
  (bufm-loop-payload)
  (set-clipboard bufm-oclip)
  (update-display))

;;
;; (bufm-loop-payload)
;;
;; executed until bufm-stop is t
;; set the message on the input line
;; wait for a key to be pressed and dispatch it to the 
;; bound key or single key handlers 
;;

(defun bufm-loop-payload()
  (bufm-debug "bufm-payload")
  (setq bufm-ops (+ bufm-ops 1))
  (message "buffer menu: 1,2,s,k,x")
  (update-display)
  (setq bufm-key (get-key))
  (cond ((eq bufm-key "") (bufm-handle-bound-key))
        (t (bufm-handle-single-key bufm-key)))
  (cond
    ((or (> bufm-ops bufm-max-ops) bufm-stop) (setq bufm-stop t))
    (t (bufm-loop-payload))))

;;
;; (bufm-handle-bound-key)
;;
;; handle up / down arrow
;; increment or decrement bufm-line appropriately
;; check the result is not outside the lines that contain buffer names
;; retrieve the name of the buffer into bufm-buf
;;

(defun bufm-handle-bound-key()
  (bufm-debug "bufm-handle-bound-key")
  (setq bufm-key (get-key-funcname))
  (cond ((eq bufm-key "previous-line") (bufm-move-line -1))
	((eq bufm-key "next-line") (bufm-move-line 1)))
  (setq bufm-buf (bufm-get-bufn)))

;;
;; (bufm-handle-single-key)
;;
;; 1 select the buffer as a single window
;; 2 split selected buffer in one window with the original buffer in the other
;; k kill the selected buffer
;; s save the selected buffer
;; x exit buffer-menu
;;

(defun bufm-handle-single-key(k)
  (bufm-debug "bufm-handle-single-key")
  (setq bufm_count (get-buffer-count))
  (cond
    ((memq k '("x" "q"))
     (goto-line bufm-start-line)
     (beginning-of-line)
     (cond ((search-forward bufm-obuf) (select-buffer bufm-obuf))
           (t (select-buffer "*scratch*")))
     (setq bufm-stop t)
     (update-display))
    ((and (eq k "1") (> bufm_count 1))
     (select-buffer bufm-buf)
     (delete-other-windows)
     (setq bufm-stop t))
    ((and (eq k "2") (> bufm_count 1))
     (select-buffer bufm-buf)
     (split-window)
     (select-buffer bufm-obuf)
     (other-window)
     (setq bufm-stop t))
    ((and (eq k "s") (> bufm_count 1))
     (save-buffer bufm-buf)
     (list-buffers)
     (goto-line bufm-line))
    ((and (eq k "k") (> bufm_count 1))
     (kill-buffer bufm-buf)
     (list-buffers)
     (setq bufm-last-line (+ bufm-start-line (get-buffer-count)))
     (setq bufm-last-line (- bufm-last-line 2))
     (bufm-move-line 0)
     (goto-line bufm-line)
     (setq bufm-buf (bufm-get-bufn)))))


;;
;; (bufm-get-bufn)
;;
;; retrieve the buffer name on the current line in the buffer list
;; trim away leading and trailing spaces
;;

(defun bufm-get-bufn()
  (bufm-debug "bufm-get-bufn")
  (goto-line bufm-line)
  (beginning-of-line)
  (repeat 11 forward-char)
  (set-mark)
  (repeat 17 forward-char)
  (copy-region)
  (beginning-of-line)
  (string.trim (get-clipboard)))

;;
;; (bufm-move-line)
;;
;; increment line by n (could +1 or -1)
;; check that bufm-line is between bufm-start-line and bufm-last-line
;; and adjust the value if required
;;
(defun bufm-move-line(n)
  (bufm-debug "bufm-move-line")
  (setq bufm-line (max bufm-start-line (min (+ bufm-line n) bufm-last-line))))

;;
;; procedures to assist debugging and tracing
;; output is sent to file debug.out
;;

(defmacro log-var (var)
  (list 'log-debug (list 'concat (symbol-name var) "=" var "\n")))

(defun bufm-debug(msg)
  (cond (bufm-debugging
	 (log-debug (string.append msg "\n"))
	 (log-var bufm-line)
	 (log-var bufm-start-line)
	 (log-var bufm-last-line)
	 (log-var bufm-obuf)
	 (log-var bufm-buf)
	 (log-debug "\n\n"))))

(provide 'bufmenu)
