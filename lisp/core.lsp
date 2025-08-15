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

(setq
 flisp-ok 0
 flisp-error 1
 flisp-return 2
 flisp-break 3
 flisp-eof 4
 flisp-user 5
 flisp-read-incomplete 6
 flisp-read-invalid 7
 flisp-read-range 8
 flisp-wrong-type 9
 flisp-invalid-value 10
 flisp-parameter-error 11
 flisp-io-error 12
 flisp-oom 13
 flisp-gc-error 14
 )

(defun string (s)
  ;; Convert argument to string.
  ;; Common Lisp
  (cond
    ((eq nil s) "")
    ((numberp s) (number-to-string s))
    ((stringp s) s)
    ((symbolp s) (symbol-name s))
    ((consp s) (string.append (string (car s)) (string (cdr s))))
    (t (throw flisp-wrong-type "cannot convert to string" s))))

(defun concat args
  ;; Concatenate all arguments to a string.
  ;; Elisp
  (cond
    ((eq nil args) "")
    ((eq nil (cdr args)) (string (car args)))
    (t (string.append (string (car args)) (concat (cdr args)))) ))

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
    (t (throw flisp-wrong-type "let: first argument neither label nor binding" (car args)))))

(defun prog1 (arg . args) arg)

;; load
(defun fload (f . r)
  (setq o (fread f :eof))
  (cond
    ((eq o :eof) (car r))
    (t
     (setq r (eval o))
     (fload f r))))

(defun load (p)
  (let ((f (fopen p "r")))
    (prog1 (fload f)
      (fclose f))))


;; Features
(setq features nil)

(defun provide args
  ;; args: (feature [subfeature ..])
  ;; Elisp, subfeatures not implemented
  (setq features (cons (car args) features)))

(defun require (feature . args)
  ;; args: (feature [filename [noerror]])
  ;; Elisp optional parameters not implemented
  (cond
    ((memq feature features) feature)
    (t
     ;; Emacs optionally uses provided filename here
     (setq path (concat script_dir "/" (symbol-name feature) ".lsp"))
     (setq r (catch (load path)))
     (cond
       ((= (car r) 0)
	(cond ((memq feature features)	feature)))))))

(provide 'core)
