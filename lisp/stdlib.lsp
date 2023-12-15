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

(defun append (xs y)
  (cond ((null xs) y)
	(t (cons (car xs) (append (cdr xs) y)))))


(provide 'stdlib)
