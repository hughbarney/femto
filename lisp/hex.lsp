
;;
;; convert an ASCII byte in the range of 0-255 to its eqivalent value as a string in
;; hexadecimal format. Return ## if n is out of range.
;;

(defun ascii->hex(n)
  (if (or (> n 255) (< n 0))
  (progn
    "##")
  (progn
    (setq h2 (% n 16))
    (setq h1 (/ (- n h2) 16))
    (concat (string.ref "0123456789abcdef" h1) (string.ref "0123456789abcdef" h2)))))

(ascii->hex 127)
"7f"
