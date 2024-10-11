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

(defun string (s)
  ;; Convert argument to string.
  ;; Common Lisp
  (cond
    ((eq nil s) "")
    ((numberp s) (number-to-string s))
    ((stringp s) s)
    ((symbolp s) (symbol-name s))
    ((consp s) (string.append (string (car s)) (string (cdr s))))
    (t (signal 'wrong-type-argument (list "cannot convert to string" s)))))

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

;; Features
(setq features nil)

(defun provide args
  ;; args: (feature [subfeature ..])
  ;; Elisp, subfeatures not implemented
  (setq features (cons (car args) features)))

(defun require args
  ;; args: (feature [filename [noerror]])
  ;; Elisp, optional parameters not implemented
  (setq feature (car args))
  (cond ((memq feature features) feature)
	(t
	 ;; Emacs optionally uses provided filename here
	 (setq path (concat script_dir "/" (symbol-name feature) ".lsp"))
	 (load path)
	 ;; Emacs checks if load fails and returns nil instead of feature.
	 (cond ((memq feature features)	feature)))))

(provide 'core)
