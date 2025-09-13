;; flisp Language

;; Standard Lisp and Scheme functions

(require 'core)

(defun listp (o) (cond ((null o)) ((consp o))))

(defmacro and args
  (cond
    ((null args))
    ((null (cdr args)) (car args))
    (t (list 'cond (list (car args) (cons 'and (cdr args)))))))

(defmacro or args  (cons 'cond (mapcar list args)))

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
     (throw wrong-type-argument "not a number" n))
    ((null args) n)
    (t (reduce (lambda (a b) (cond ((< a b) a) (t b)))
	args n)) ))

(defun nthcdr (i l)
  (cond
    ((not (integerp i))
     (throw wrong-type-argument
       (concat "(nthcdr i l) - i expected type-integer, got: " (type-of i))
       i))
    ((< i 0) (throw range-error "negative index" i))
    ((null l) nil)
    ((= 0 i) l)
   ((not (consp l))
    (throw wrong-type-argument
      (concat "(nthcdr i l) - l expected type-cons, got: " (type-of l))
      l ))
    (t (nthcdr (- i 1) (cdr l)))))

(defun nth (n list)
  (car (nthcdr n list)))

(defun fold-right (f o l)
  (cond
    ((null l) o)
    (t (f (car l) (fold-right f o (cdr l))))))

(defun unfold (func o p)
  (cond ((p o) (cons o nil))
	(t (cons o (unfold func (func o) p)))))

(defun iota (count . args)
  (let (
	(count (- count 1))
	(start (cond ((car args)) (t 0)))
	(step (cond ((cadr args)) (t 1))))
    (let (
	  (func (lambda (n) (setq count (- count 1)) (+ n step)))
	  (pred (lambda (n) (= 0 count))))
      (unfold func start pred))))

;;
;; Commonly used Lisp functions which are not other wise used in the Femto libraries
;;
(defun atom (o) (null (consp o)))
(defun zerop (n) (= n 0))

(defmacro if args
  (list 'cond (list (car args) (car (cdr args))) (cons 't (cdr (cdr args)))))

(defun equal (o1 o2)
  (or (and (atom o1) (atom o2)
	   (eq o1 o2))
      (and (consp o1) (consp o2)
	   (equal (car o1) (car o2))
	   (equal (cdr o1) (cdr o2)))))

(provide 'flisp)
