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
  (setq f (fopen p "r"))
  (setq r (fload f))
  ;(fclose f)
  r
  )



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
     (load path)
     ;; Emacs checks if load fails and returns nil instead of feature.
     (cond ((memq feature features)	feature)))))

(provide 'core)
