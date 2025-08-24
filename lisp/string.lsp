;;
;;  string function library
;;
;;


;; trim all spaces from front of a string
(defun string-trim-front(s)
  (cond ((eq (substring s 0 1) " ") (string-trim-front (substring s 1)))
	(t s)))

;; trim all spaces from back of a string
(defun string-trim-back(s)
  (cond  ((eq (substring s -1)  " ") (string-trim-back (substring s 0 -1)))
	 (t s)))

;; trim spaces off front and back of a string
(defun string-trim(s)
  (string-trim-back (string-trim-front s)))


;;
;; string-ref , get character at position r
;;   zero based indexing
;;
(defun string-ref (s r)
   (substring s r (+ r 1)))


;;
;; string-startswith - return t if string starts with search
;;
(defun string-startswith (str search)
  (eq 0 (string-search search str)))


;;
;; string-drop_first - return a string with the first char chopped off
;;   return "" when we have reached the end
;;
;; - leave for now, to be replaced by string-shrink-right
(defun string-drop_first(s)
  (substring s 1))


;;
;; shrink string by dropping off last char
;;  
;; should rename to string.drop_last
;;
;; - keep for now, replace with string-shrink-left
;;
(defun shrink(s)
  (substring s 0 -1))


(defun string-shrink-left(s)
  (substring s 0 -1))


;;
;; return first char of string
;;
(defun string-first-char(s)
  (substring s 0 1))

;;
;; return last char of string
;;
(defun string-last-char(s)
  (substring s -1))


;;
;; string-contains - return t if search is contained in str
;;
(defun string-contains (str search)
   (cond
     ((< (length str) (length search)) nil)  
     ((eq str "") nil)
     ((eq search "") nil)
     ((string-startswith str search) t)
     ((eq (string-drop_first str) "") nil)
     (t (string-contains (string-drop_first str) search))))


(provide 'string)


