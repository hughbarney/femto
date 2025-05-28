;; flisp Language

;; Expected Lisp idioms

(require 'core)

(setq not null)
(defun listp (x) (cond ((null x)) ((consp x))))

;; Note: macro evaluation is broken: env lookups do not work
;; Workaround: we use defun instead, so we loose shortcircuit evaluation.
;; (defmacro and args
;;   (cond
;;     ((null args))
;;     ((cond
;;        ((cdr args) (cond ((car args) (cons 'and  (cdr args)))))
;;        (t (car args))))))

(defun and args
  (cond
    ((null args))
    ((cond
       ((cdr args) (cond ((car args) (cons 'and  (cdr args)))))
       (t (car args))))))

(defun map1 (func xs)
  (cond (xs (cons (func (car xs)) (map1 func (cdr xs))))))

(defmacro or args
  (cond (args (cons (quote cond) (map1 list args)))))

(defun reduce (func seq start)
  (cond ((null seq) start)
        (t (reduce func (cdr seq) (func (car seq) start)))))

(defun max (n . args)
  (cond
    ((null (numberp n))
     (signal 'wrong-type-argument (list "not a number" n)))
    ((null args) n)
    (t (reduce
	(lambda (a b) (cond ((< a b) b) (t a)))
	args n))))

(defun min (n . args)
  (cond
    ((null (numberp n))
     (signal 'wrong-type-argument (list "not a number" n)))
    ((null args) n)
    (t (reduce (lambda (a b) (cond ((< a b) a) (t b)))
	args n)) ))

(defun nthcdr (n list)
  (cond 
    ((> 0 n) (signal 'args-out-of-range '("negativ index" n)))
    ((= 0 n) list)
    (t (nthcdr (- n 1) (cdr list)))))

(defun nth (n list)
  (car (nthcdr n list)))

(provide 'flisp)
