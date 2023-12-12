;; flisp Language

(defun load-script(fn)
  (load (concat script_dir "/" fn)))

;; Expected Lisp idioms

(defun null (x) (eq x nil))
(setq not null)
(defun listp (x) (cond ((eq nil x)) ((consp x))))

(defmacro and args
  (cond
    ((eq nil args))
    ((eq nil (cdr args)) (car args))
    ((eq nil (car args)) nil) ;; Note: unnecessary optimization?
    (t (cons 'cond (list (list (car args) (cons 'and (cdr args)))))) ))

(defun map1 (func xs)
  (cond (xs (cons (func (car xs)) (map1 func (cdr xs))))))

(defmacro or args
  (cond (args (cons (quote cond) (map1 list args)))))

(defun reduce (func seq start)
  (cond ((eq nil seq) start)
        (t (reduce func (cdr seq) (func (car seq) start)))))

(defun max args
  (cond
    ((eq nil args) (signal 'wrong-number-of-arguments '(max 0)))
    ((eq nil (cdr args))
     (cond ((numberp (car args)) (car args))
	   (t (signal 'wrong-type-argument (list "not a number" (car args))))))
    (t (reduce
	(lambda (a b) (cond ((< a b) b) (t a)))
	(cdr args)
	(car args)))))

(defun min args
  (cond
    ((eq nil args) (signal 'wrong-number-of-arguments '(min 0)))
    ((eq nil (cdr args))
     (cond ((numberp (car args)) (car args))
	   (t (signal 'wrong-type-argument (list "not a number" (car args))))))
    (t (reduce
	(lambda (a b) (cond ((< a b) a) (t b)))
	(cdr args)
	(car args)))))

(defun nthcdr (n list)
  (cond 
    ((> 0 n) (signal 'args-out-of-range '("negativ index" n)))
    ((= 0 n) list)
    (t (nthcdr (- n 1) (cdr list)))))

(defun nth (n list)
  (car (nthcdr n list)))

(provide 'flisp)
