;;
;; start up message
;;

(defun show-startup-message()
  (if (eq "*scratch*" (get-buffer-name))
  (progn
    (insert-string "\n\n\n\n")
    (insert-string "  /**********************************\n")
    (insert-string "\n\n")
    (insert-string "   / _| ___ _ __ ___ | |_ ___     \n")
    (insert-string "  | |_ / _ \ '_ ` _ \| __/ _ \    \n")
    (insert-string "  |  _|  __/ | | | | | || (_) |   \n")
    (insert-string "  |_|  \___|_| |_| |_|\__\___/'    \n\n  ")
    (insert-string "\n\n")
    (insert-string "  **********************************/\n\n\n\n")
    (insert-string "  Tiny Emacs clone with Tiny-Lisp extension language\n  ")
    (insert-string (get-version-string))
    (insert-string "\n\n\n")
    (end-of-buffer))))
