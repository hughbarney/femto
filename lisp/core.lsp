;; -*-Lisp-*-
;;
;; Core fLisp extensions
;;

(bind list (lambda args args) t)

(bind defmacro
      (macro (name params . body)
	     (list 'bind name (list (quote macro) params . body) 't) )
      t )

(defmacro setq args
  (cond (args
	 (cond ((null (cdr args))
		(throw wrong-number-of-arguments "(setq [s v ..]) expects a multiple of 2 arguments") )
	       ((null (cdr (cdr args)))
		(list 'bind (car args) (car (cdr args)) t) )
	       (t
		(list 'progn
		      (list 'setq (car args) (car (cdr args)))
		      (cons 'setq (cdr (cdr args))) ))))))

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

(defun cadr (l) (car (cdr l)))
(defun cddr (l) (cdr (cdr l)))
(defun caddr (l) (car (cdr (cdr l))))

;;; https://www.scheme.com/tspl2d/objects.html#g2052
(defun append lists
  (let f ((ls nil) (lists lists))
       (cond
	 ((null lists) ls)
         (t
	  (let g ((ls ls))
	       (cond
		 ((null ls) (f (car lists) (cdr lists)))
                 (t
		  (cond ((not (consp ls))
			 (throw invalid-value
			   (concat "(append lists) - list expected type-list, got " (type-of ls))
			   ls)))
		  (cons (car ls) (g (cdr ls)))) ))))))


(defun fold-left (f i l)
  (cond ((null l) i)
	(t (fold-left f (f i (car l)) (cdr l))) ))

(defun flip (func)  (lambda (o1 o2) (func o2 o1)))
(defun reverse (l)  (fold-left (flip cons) nil l))

(defun apply (f . args)
  (cond
    ((null args) (f))
    (t
     (let ((rev (reverse args)))
       (cond
	 ((consp (car rev))
          ;; if last element is list splice it
	  (eval (cons f (append (reverse (cdr rev)) (car rev)))) )
	 (t (f . args)) )))))

(defun print (o . fd)
  (cond
    ((null fd) (write o t))
    (t (write o t car fd)) ))

(defun princ (o . fd)
  (cond
    ((null fd) (write o nil))
    (t (write o nil (car fd))) ))

(defun string-to-number (string)
  (let ((f (open string "<")) (result nil))
    (setq  result (catch (read f)))
    (close f)
    (cond ((car result) 0)
	  (t (cond ((numberp (caddr result)) (caddr result))
		   (t 0))))))
	  
(defun eq (o1 o2)
  (cond
    ((same o1 o2))
    ((same (type-of o1) (type-of o2))
     (cond
       ((stringp o1) (string-equal o1 o2))
       ((integerp o1) (i= o1 o2))
       ((doublep o1) (d= o1 o2))))))

(setq not null)

(defun length (o)
  (cond
    ((null o) 0)
    ((stringp o) (string-length o))
    ((consp o)
     (fold-left (lambda (x y) (+ x 1)) 0 o))
    (t (throw wrong-type-argument "(length object) - expected type-cons or type-string" o))))


(defun string (o)
  ;; Convert argument to string.
  ;; Common Lisp
  (cond
    ((eq nil o) "")
    ((stringp o) o)
    ((symbolp o) (symbol-name o))
    ((consp o) (string-append (string (car o)) (string (cdr o))))
    (t (let ((f (open "" ">")))
	 (write o t f)
	 (prog1
	     (cadr (file-info f))
	   (close f) )))))

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
    ((eq nil l) nil)
    ((eq o (car l)) l)
    (t (memq o (cdr l)))))

(defun mapcar (func xs)
  (cond (xs (cons (func (car xs)) (mapcar func (cdr xs))))))

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
	 (cons 'lambda (cons (mapcar car (car args)) (cdr args))) ; (lambda (names) body)
	 (mapcar cadr (car args)))) ; (values)
       (t (throw wrong-type-argument "let: first argument neither label nor binding" (car args)))))
    ((symbolp (car args))
;;; label:    (car args)
;;; bindings: (cadr args)
;;; body:     (cddr args)
     (list
      (list 'lambda '()
;;;	    (list 'define (car args)
	    (list 'bind (car args)
		  (cons 'lambda (cons (mapcar car (cadr args)) (cddr args))))
	    (cons (car args) (mapcar cadr (cadr args))))))
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
