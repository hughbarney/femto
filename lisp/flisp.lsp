;; flisp Language

;; Standard Lisp and Scheme functions

(require 'core)

(setq not null)
(defun listp (x) (cond ((null x)) ((consp x))))

(defmacro and args
  (cond
    ((null args))
    ((null (cdr args)) (car args))
    (t (list 'cond (list (car args) (cons 'and (cdr args)))))))

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
     (throw flisp-wrong-type "not a number" n))
    ((null args) n)
    (t (reduce
	(lambda (a b) (cond ((< a b) b) (t a)))
	args n))))

(defun min (n . args)
  (cond
    ((null (numberp n))
     (throw flisp-wrong-type "not a number" n))
    ((null args) n)
    (t (reduce (lambda (a b) (cond ((< a b) a) (t b)))
	args n)) ))

(defun nthcdr (n list)
  (cond 
    ((> 0 n) (throw flisp-read-range "negativ index" n))
    ((= 0 n) list)
    (t (nthcdr (- n 1) (cdr list)))))

(defun nth (n list)
  (car (nthcdr n list)))

(defun mapcar (func xs)
  (cond (xs (cons (func (car xs)) (map1 func (cdr xs))))))

(defun cadr (l) (car (cdr l)))
(defun cddr (l) (cdr (cdr l)))

(defmacro let args
  (cond
    ((consp (car args))
;;; bindings: (car args)
;;; body:     (cdr args)
     (cons ; apply
      (cons 'lambda (cons (mapcar car (car args)) (cdr args))) ; (lambda (names) body)
      (mapcar cadr (car args)))) ; (values)
    ((symbolp (car args))
;;; label:    (car args)
;;; bindings: (cadr args)
;;; body:     (cddr args)
     (list
      (list 'lambda '()
	    (list 'define (car args)
		  (cons 'lambda (cons (mapcar car (cadr args)) (cddr args))))
	    (cons (car args) (mapcar cadr (cadr args))))))
    (t (throw flisp-wrong-type "let: first argument neither label nor binding" (car args)))))

(provide 'flisp)
