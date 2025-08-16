;;
;; a do until construct
;;

(defun countdown (n)
  (log-message (concat "countdown from " n "\n"))
  (let do ((i n))
       (log-message (concat i "\n"))
       (cond ((> i 0)   (do (- i 1)) )) ) )




;; ---- in the *messages* buffer we see --------
countdown from 5
5
4
3
2
1
0
countdown from 3
3
2
1
0

;;
;;  for loop 1
;;

(let for ((i 1)  (end 10) (step 2))
    (log-message (concat "counter: " i))
    (setq i (+ i step))
    (cond ((< i end)  (for i))))


;;error: '#<Lambda (i end step)>', (env) expects at least 3 arguments


;; 
;; To get a while/for loop one could write:
;;

(let ((i 1) (end 10) (step 2))
  (let repeat ()
       (cond ((< i end) ;; (
          (log-message (concat "counter: " i "\n"))
          (setq i (+ i step))
          (repeat)))))
nil



;; works

