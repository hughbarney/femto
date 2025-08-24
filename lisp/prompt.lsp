;;
;; This is not complete. The idea here is to look at building a
;; command line history that could be traversed using up and down arrow
;;
;;

(require 'femto)
(require 'stdlib)

;;
;; set to empty
;;
(setq cmd_list ())
nil

;;
;;
(defun save_response(c)
  (setq cmd_list (append cmd_list (list c)))
  c)


;;
;; prompt for string and return response, handle backspace, cr and c-g
;;
(defun cmd_prompt(q response)
  (show_prompt q response)
  (setq key (get-key))
  (if (eq key "")
  (progn
    (setq fkey (get-key-funcname))
    (if (eq fkey "previous-line") (cmd-move-line -1))
    (if (eq fkey "next-line") (cmd-move-line 1))
    (setq response (nth cmd-line cmd-list))))
  (cond
    ((eq key "\n") (save_response response))
    ((is_ctl_g key) "")
    ((is_backspace key) (cmd_prompt q (string-shrink-left response)))
    ((is_control_char key) (cmd_prompt q response))
    (t (cmd_prompt q (concat response key)))) )


;;
;; (cmd-move-line)
;;
;; increment line by n (could +1 or -1)
;; check that cmd-line is between 0 and last index of cmd-list
;; and adjust the value if required
;;
(defun bufm-move-line(n)
   (bufm-debug "buf-move-line")
   (setq bufm-line (max bufm-start-line (min (+ bufm-line n) bufm-last-line))))


(cmd_prompt ": " "")

