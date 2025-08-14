;;
;;  string function library
;;
;;


;; trim all spaces from front of a string
(defun string.trim.front(s)
  (cond
    ((= 0 (string.length s)) "")
    ((not (eq (string.substring s 0 0) " ")) s)
    ((< (string.length s) 2) "")
    (t (string.trim.front (string.substring s 1 (- (string.length s) 1)))) ))

;; trim all spaces from back of a string
(defun string.trim.back(s)
  (setq p (- (string.length s) 1))
  (cond
    ((= p -1) "")
    ((not (eq (string.substring s p p) " ")) s)
    ((< (string.length s) 2) "")
    (t (string.trim.back (string.substring s 0 (- (string.length s) 2)))) ))

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
   (eq (string.substring str  0 (- (string.length search) 1)) search))


;;
;; string.drop_first - return a string with the first char chopped off
;;   return "" when we have reached the end
;;
(defun string.drop_first(s)
  (cond
    ((< (string.length s) 2) "")
    (t (string.substring s 1 (- (string.length s) 1)))))


;;
;; shrink string by dropping off last char
;;  
;; should rename to string.drop_last
;;
(defun shrink(s)
  (cond
    ((< (string.length s) 2) "")
    (t (string.substring s 0 (- (string.length s) 2)))  ))


;;
;; string.contains - return t if search is contained in str
;;
(defun string.contains (str search)
   (cond
     ((< (string.length str) (string.length search)) nil)  
     ((eq str "") nil)
     ((eq search "") nil)
     ((string.startswith str search) t)
     ((eq (string.drop_first str) "") nil)
     (t (string.contains (string.drop_first str) search))))


(provide 'string)


