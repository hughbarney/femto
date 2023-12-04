;; Commonly used Lisp functions which are not used in the Femto libraries
;; leg20231203
;;
;; Only uses core functions

;; Note: only for testing while moving things over, remove afterwards

(defun atom (x) (eq nil (consp x)))
(defun zerop (x) (= x 0))

(provide 'stdlib)
