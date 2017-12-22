;;
;; define a macro c-x ( and the record keystrokes to a *macro* buffer so that 
;; it can be evaluated and made ready
;;


(setq dm-buffer "*macro*")
(setq dm-max-ops 10)
(setq dm-ops 0)

(defun dm-record-key(k)
  (setq dm-obuf (get-buffer-name))
  (insert-string k)
  (select-buffer dm-buffer)
  (insert-string "(insert-string \"")
  (insert-string k)
  (insert-string "\")\n")
  (select-buffer dm-obuf)
  (update-display))

(defun dm-record-action(k)
  (log-debug "dm-record-action\n")
  (setq dm-obuf (get-buffer-name))
  (execute-key)
  (select-buffer dm-buffer)
  (insert-string k)
  (insert-string "\n")
  (select-buffer dm-obuf)
  (update-display))

(defun dm-init()
  (setq dm-buffer "*macro*")
  (setq dm-max-ops 10)
  (setq dm-ops 0)
  (kill-buffer dm-buffer))

(defun dm-start-macro()
  (dm-init)
  (message "c-x ( macro recording started")
  (update-display)
  (dm-get-key))

(defun dm-end-macro()
  (setq dm-obuf (get-buffer-name))
  (select-buffer dm-buffer)
  (insert-string ")")
  (select-buffer dm-obuf)
  (setq dm-ops (+ 1 dm-max-ops))
  (message "macro recording completed"))

;; prompt for string and return response, handle backspace, cr and c-g
(defun dm-get-key()
  (setq dm-ops (+ 1 dm-ops))
  (if (> dm-ops dm-max-ops)
  (progn
    (dm-end-macro))
  (progn
    (setq key (get-key))
    (if (not (eq key ""))
    (progn
       (dm-record-key key)
       (dm-get-key))
    (progn
      (cond
        ((eq "(end-macro)" (get-key-funcname)) (dm-end-macro) t)
        ((is_ctl_g key) "")
        (t (dm-record-action (get-key-funcname)) (dm-get-key)))  )))))



