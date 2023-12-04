;; flisp Language

;; Femto backwards compatible defun`s
;; Note: The libraries should be refactored and this defuns deprecated
(setq number? numberp)
(setq number->string number-to-string) ; Elisp
(setq string? stringp)
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
    (t (list 'cond (list (car args) (cons 'and (cdr args)))))))

(defun map1 (func xs)
  (cond (xs (cons (func (car xs)) (map1 func (cdr xs))))))

(defmacro or args
  (cond (args (cons (quote cond) (map1 list args)))))


;; Note: consider moving to std from here

(defmacro if args
  (list 'cond (list (car args) (car (cdr args))) (cons 't (cdr (cdr args)))))

(defun equal (x y)
  (or (and (atom x) (atom y)
	   (eq x y))
      (and (consp x) (consp y)
	   (equal (car x) (car y))
	   (equal (cdr x) (cdr y)))))

(defun nth (n xs)
  (if (= 0 n)
      (car xs)
      (nth (- n 1) (cdr xs))))

(defun append (xs y)
  (if (eq nil xs)
      y
      (cons (car xs) (append (cdr xs) y))))


(provide 'flisp)
