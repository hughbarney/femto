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

(defun typep (type object)  (eq type (type-of object)))

(setq intergerp (curry typep type-integer))
(setq numberp (curry typep type-number))
(setq stringp (curry typep type-string))
(setq symbolp (curry typep type-symbol))
;;; consp is a primitive
(setq lambdap (curry typep type-lambda))
(setq macrop (curry typep type-macro))
(setq streamp (curry typep type-stream))

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

(defmacro let args
  (cond
    ((consp (car args))
;;; bindings: (car args)
;;; body:     (cdr args)
     (cons ; apply
      (cons 'lambda (cons (map1 car (car args)) (cdr args))) ; (lambda (names) body)
      (map1 cadr (car args)))) ; (values)
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

(defun length (o)
  (cond
    ((null o) 0)
    ((stringp o) (string-length o))
    ((consp o)
     (let count ((o o) (len 1))
	  (cond
	    ((null (cdr o)) len)
	    (t (count (cdr o) (+ len 1))))))
    (t (throw wrong-type-argument "(length object) - expected type-cons or type-string" o))))

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
