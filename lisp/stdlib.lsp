;; Commonly used Lisp functions which are not used in the Femto libraries
;; leg20231203
;;
;; Only uses core functions

(defun atom (x) (null (consp x)))
(defun zerop (x) (= x 0))

(defmacro if args
  (list 'cond (list (car args) (car (cdr args))) (cons 't (cdr (cdr args)))))

(require 'flisp)
;; Note: not yet refactored to core functions
;;  only do with unit tests
(defun equal (x y)
  (or (and (atom x) (atom y)
	   (eq x y))
      (and (consp x) (consp y)
	   (equal (car x) (car y))
	   (equal (cdr x) (cdr y)))))

;; Note: to be enhanced
(defun _append (xs y)
  (cond ((null xs) y)
	(t (cons (car xs) (_append (cdr xs) y)))))

(defun __append (xs y)
  (cond ((null xs) y)
	((consp xs)
	 (cons (car xs) (_append (cdr xs) . y)))
	(t (throw wrong-type-argument "(append arg args) - arg must be list" ))))

(defun append args
  (cond
    ((null args) nil)
    ((null (cdr args)) (car args))
    (t (__append (car args) (cdr args)))))

(defun print (x . args)
  (cond
    ((null args) (write x :readably t))
    (t (write x :readably t :stream (car args)))))

(defun princ (x . args)
  (cond
    ((null args) (write x :readably nil))
    (t (write x :readably nil :stream (car args)))))


(provide 'stdlib)
