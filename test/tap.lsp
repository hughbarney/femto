;;; Poor mans unit test framework in Femto Lisp

(require 'core)

(setq reportFd (open ">3"))

(defun pr args
  (mapcar (lambda (o) (princ o reportFd)) args)
  (princ "\n" reportFd))

;(defmacro test (comment . body)
  ;;; Note: my macro skills fail on me again, this does not work
  ;;; want: `(setq tests (cons (cons ,comment (lambda () ,body)) tests))
;  (list 'setq 'tests (list 'cons (list 'cons comment (list 'lambda '()  body)) 'tests)))

(defun tap-register (comment test)
  (setq tests (cons (cons comment (lambda () (null (test)))) tests)))

(defun ok (num test)
  ;; Ok runs 'test' and prints out a TAP14 conform message
  (let ((result (catch ((cdr test)))))
    (cond ((car result) (pr "not ok " num " - " (car test) " test failed with '" (car result) ": " (cadr result)))
	  ((cond ((caddr result) (pr "not ok " num  " - " (car test) " " (caddr result)))
		((pr "ok " num " - " (car test)))))))
  (+ num 1))

(defun tap (suite)
  (setq tests nil)
  (let ((result (catch (load suite))))
    (cond ((car result) (pr "error: failed to load test suite" suite ":" (cadr result)))
	  (t
	   (pr "TAP version 14")
	   (pr "1.." (length (caddr result)))
	   (pr "# " suite)
	   (fold-left ok 1 (reverse tests))))))
