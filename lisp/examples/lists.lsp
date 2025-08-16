(require 'flisp)

(list 1 2 3)
(1 2 3)

(append (list 1 2 3) (list 4))
(1 2 3 4)

(list "aa" "bb" "cc")
("aa" "bb" "cc")

(append (list "aa" "bb" "cc") (list "dd"))
("aa" "bb" "cc" "dd")

(nth 1 (list "aa" "bb" "cc"))
"bb"

;; set to empty
(setq cmd_list ())
nil

(listp cmd_list)
t

(defun add_to_cmds(c)
  (setq cmd_list (append cmd_list c)))

(setq s "ii")

(add_to_cmds (list s))
("gg" "ii" "ii")

