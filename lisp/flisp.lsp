;; flisp Language

;; Standard Lisp and Scheme functions

(require 'core)

(defun listp (x) (cond ((null x)) ((consp x))))

(defmacro and args
  (cond
    ((null args))
    ((null (cdr args)) (car args))
    (t (list 'cond (list (car args) (cons 'and (cdr args)))))))

(defmacro or args
  (cond (args (cons (quote cond) (map1 list args)))))

(defun reduce (func seq start)
  (cond ((null seq) start)
        (t (reduce func (cdr seq) (func (car seq) start)))))

(defun max (n . args)
  (cond
    ((null (numberp n))
     (throw 'wrong-type-argument "not a number" n))
    ((null args) n)
    (t (reduce
	(lambda (a b) (cond ((< a b) b) (t a)))
	args n))))

(defun min (n . args)
  (cond
    ((null (numberp n))
     (throw 'wrong-type-argument "not a number" n))
    ((null args) n)
    (t (reduce (lambda (a b) (cond ((< a b) a) (t b)))
	args n)) ))

(defun nthcdr (n list)
  (cond 
    ((> 0 n) (throw 'range-error "negativ index" n))
    ((= 0 n) list)
    (t (nthcdr (- n 1) (cdr list)))))

(defun nth (n list)
  (car (nthcdr n list)))

(defun fold-right (f e l)
  (cond
    ((null l) e)
    (t (f (car l) (fold-right f e (cdr l))))))

(defun unfold (func init pred)
  (cond ((pred init) (cons init nil))
	(t (cons init (unfold func (func init) pred)))))

(defun iota (count . args)
  (let (
	(count (- count 1))
	(start (cond ((car args)) (t 0)))
	(step (cond ((cadr args)) (t 1))))
    (let (
	  (func (lambda (n) (setq count (- count 1)) (+ n step)))
	  (pred (lambda (n) (= 0 count))))
      (unfold func start pred))))

(provide 'flisp)
