;;
;;  string function library
;;
;;


;; trim all spaces from front of a string
(defun string.trim.front(s)
  (cond ((eq (substring s 0 1) " ") (string.trim.front (substring s 1)))
	(t s)))

;; trim all spaces from back of a string
(defun string.trim.back(s)
  (cond  ((eq (substring s -1)  " ") (string.trim.back (substring s 0 -1)))
	 (t s)))

;; trim spaces off front and back of a string
(defun string.trim(s)
  (string.trim.back (string.trim.front s)))


;;
;; string.ref 
;;
(defun string.ref (s r)
   (string.substring s r r))


;;
;; string.startswith - return t if string starts with search
;;
(defun string.startswith (str search)
   (eq (substring str  0 (- (length search) 1)) search))


;;
;; string.drop_first - return a string with the first char chopped off
;;   return "" when we have reached the end
;;
(defun string.drop_first(s)
  (cond
    ((< (length s) 2) "")
    (t (substring s 1 (- (length s) 1)))))


;;
;; shrink string by dropping off last char
;;  
;; should rename to string.drop_last
;;
(defun shrink(s)
  (cond
    ((< (length s) 2) "")
    (t (substring s 0 (- (length s) 2)))  ))


;;
;; string.contains - return t if search is contained in str
;;
(defun string.contains (str search)
   (cond
     ((< (length str) (length search)) nil)  
     ((eq str "") nil)
     ((eq search "") nil)
     ((string.startswith str search) t)
     ((eq (string.drop_first str) "") nil)
     (t (string.contains (string.drop_first str) search))))


(provide 'string)


