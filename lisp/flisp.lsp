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

;; Note: consider moving to std from here
(defun atom (x) (eq nil (consp x)))
(defun zerop (x) (= x 0))

;; Note: contains if
(defmacro and args
  (cond ((eq nil args) t)
	((eq nil (cdr args)) (car args))
	(t (list (quote if) (car args) (cons (quote and) (cdr args))))))

;; Note: contains if
(defun map1 (func xs)
  (if (eq nil xs)
      nil
      (cons (func (car xs))
	    (map1 func (cdr xs)))))

;; Note: contains if
(defmacro or args
  (if (eq nil args)
      nil
      (cons (quote cond) (map1 list args))))

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
