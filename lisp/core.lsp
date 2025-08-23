;; -*-Lisp-*-
;;
;; Core fLisp extensions
;;

(setq list (lambda args args))

(setq defmacro
      (macro (name params . body)
	     (list (quote setq) name (list (quote macro) params . body))))

(defmacro defun (name params . body)
  (list (quote setq) name (list (quote lambda) params . body)))

(defun curry (func arg1)
  (lambda (arg2) (func arg1 arg2)))

(defun typep (type object)  (same type (type-of object)))

(setq
 integerp (curry typep type-integer)
 doublep (curry typep type-double)
 stringp (curry typep type-string)
 symbolp (curry typep type-symbol)
;;; consp is a primitive
 lambdap (curry typep type-lambda)
 macrop (curry typep type-macro)
 streamp (curry typep type-stream))

(defun numberp (o) (cond  ((integerp o)) ((doublep o))))

(defun number-to-string (num)
  (cond
    ((numberp num)
     (let ((f (open "" ">")))
       (write num :stream f)
       (prog1
	   (cadr (file-info f))
	 (close f))))
    (t 	(throw wrong-type-argument
	  (concat "(number-to-string number) - number expected " type-integer " got: " (type-of i))))))

(defun eq (o1 o2)
  (cond
    ((same o1 o2))
    ((same (type-of o1) (type-of o2))
     (cond
       ((stringp o1) (string-equal o1 o2))
       ((integerp o1) (i= o1 o2))
       ((doublep o1) (d= o1 o2))))))

(setq not null)

(defun fold-left (f i l)
  (cond ((null l) i)
	(t (fold-left f (f i (car l)) (cdr l))) ))

(defun length (o)
  (cond
    ((null o) 0)
    ((stringp o) (string-length o))
    ((consp o)
     (fold-left (lambda (x y) (+ x 1)) 0 o))
    (t (throw wrong-type-argument "(length object) - expected type-cons or type-string" o))))


(defun string (s)
  ;; Convert argument to string.
  ;; Common Lisp
  (cond
    ((eq nil s) "")
    ((numberp s) (number-to-string s))
    ((stringp s) s)
    ((symbolp s) (symbol-name s))
    ((consp s) (string-append (string (car s)) (string (cdr s))))
    (t (throw wrong-type-argument "cannot convert to string" s))))

(defun concat args
  ;; Concatenate all arguments to a string.
  ;; Elisp
  (cond
    ((eq nil args) "")
    ((eq nil (cdr args)) (string (car args)))
    (t (string-append (string (car args)) (concat (cdr args)))) ))

(defun memq (o l)
  ;; If object o in list l return sublist of l starting with o, else nil.
  ;; Elisp
  (cond
    ((eq nil o) nil)
    ((eq nil l) nil)
    ((eq o (car l)) l)
    (t (memq o (cdr l)))))

(defun map1 (func xs)
  (cond (xs (cons (func (car xs)) (map1 func (cdr xs))))))

(defun cadr (l) (car (cdr l)))
(defun cddr (l) (cdr (cdr l)))

;;; Wrap all math to Integer operations
(defun nfold (f i l);  (3)  (1 2 3)
  (cond
    ((null l) i)
    ((null (cdr l)) (f i (car l)))
    ( t (fold-left f (f (car l) (cadr l)) (cddr l)))))

(defun coerce (ifunc dfunc x y)
  (cond  ((doublep x) (cond ((integerp y) (dfunc x (double y))) (t (dfunc x y))))
         ((doublep y) (cond ((integerp x) (dfunc (double x) y)) (t (dfunc x y))))
         (t (ifunc x y))))

(defun coercec (ifunc dfunc) ; coerce "curry"
  (lambda (x y) (coerce ifunc dfunc x y)))

(defun +  args (fold-left (coercec i+ d+)  0 args))
(defun -  args (nfold     (coercec i- d-)  0 args))
(defun *  args (fold-left (coercec i* d*)  1 args))
(defun /  args (nfold     (coercec i/ d/)  1 args))
(defun %  args (nfold     (coercec i% d%)  1 args))

(defun fold-leftp (predicate start list)
  (cond ((null list))
	((predicate start (car list)) (fold-leftp predicate (car list) (cdr list)))))

(defun =  (x . args) (fold-leftp (coercec i=  d=)  x args))
(defun <  (x . args) (fold-leftp (coercec i<  d<)  x args))
(defun <= (x . args) (fold-leftp (coercec i<= d<=)  x args))
(defun >  (x . args) (fold-leftp (coercec i>  d>)  x args))
(defun >= (x . args) (fold-leftp (coercec i>= d>=)  x args))


(defmacro let args
  (cond
    ((consp (car args))
     (cond
       ((consp (cadr args))
;;; bindings: (car args)
;;; body:     (cdr args)
	(cons ; apply
	 (cons 'lambda (cons (map1 car (car args)) (cdr args))) ; (lambda (names) body)
	 (map1 cadr (car args)))) ; (values)
       (t (throw wrong-type-argument "let: first argument neither label nor binding" (car args)))))
    ((symbolp (car args))
;;; label:    (car args)
;;; bindings: (cadr args)
;;; body:     (cddr args)
     (list
      (list 'lambda '()
	    (list 'define (car args)
		  (cons 'lambda (cons (map1 car (cadr args)) (cddr args))))
	    (cons (car args) (map1 cadr (cadr args))))))
    (t (throw wrong-type-argument "let: first argument neither label nor binding" (car args)))))

(defun prog1 (arg . args) arg)

;; load
(defun fload (f)
  (let loop ((o  nil) (r nil))
       (setq o (read f :eof))
       (cond ((eq o :eof)  r)
	     (t (setq r (eval o))
		(loop nil r)))))

(defun load args
  (let ((f (open (car args))))
    (prog1 (fload f)
      (close f))))

;; Features
(setq features nil)

(defun provide args
  ;; args: (feature [subfeature ..])
  ;; Elisp, subfeatures not implemented
  (cond ((memq (car args) features) (car args))
	(t (setq features (cons (car args) features)))))

(defun require (feature . args)
  ;; args: (feature [filename [noerror]])
  ;; Elisp optional parameters not implemented
  (cond
    ((memq feature features) feature)
    (t
     ;; Emacs optionally uses provided filename here
     (setq path (concat script_dir "/" (symbol-name feature) ".lsp"))
     (setq r (catch (load path)))
     (cond ((null (car r)) (cond ((memq feature features)  feature)))))))

(provide 'core)
