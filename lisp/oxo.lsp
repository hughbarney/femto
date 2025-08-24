;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Simple tick-tac-toe (or naughts and cross, oxo for short) 
;; implementaion for femto.
;; Hugh Barney Dec 2017
;; 
;; The following editor functions are required to implement OXO
;; 
;; (insert-string "\n ")
;; (goto-line 10)
;; (beginning-of-line)
;; (kill-to-eol)
;; (message "")
;; (display)
;; (setq key (getch))
;; (debug "(draw)\n")
;; (beginning-of-buffer)
;; (set-mark)
;; (repeat 10 next-line)
;; (kill-region)
;; (beginning-of-buffer)
;; 
;;
;;  O | X | O
;; ----------
;;  O | X | X
;; ----------
;;  O | 8 | X
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flisp)

;; set to 't' to obtain debug trace

(setq oxo-debugging nil)

(defun oxo-debug(s)
 (cond (oxo-debugging (log-debug s))))

(defun init()
 (oxo-debug "(init)\n")
 (select-buffer "*oxo*")
 (beginning-of-buffer)
 (end-of-buffer)
 (kill-region)
 (setq board (list "E" "1" "2" "3" "4" "5" "6" "7" "8" "9")))

(defun val(n)
  (oxo-debug (concat "val n=" n  "\n"))
  (oxo-debug (concat "board=" board "\n"))
  (oxo-debug (concat "nth=" (nth n board) "\n"))
  (nth n board))

(defun set-nth (list n val)
  (cond
    ((> n 0) (cons (car list) (set-nth (cdr list) (- n 1) val)))
    (t (cons val (cdr list))) ))

(defun newline_and_space()
  (oxo-debug "newline_and_space\n")
  (insert-string "\n "))

;; prompt for string and return response, handle backspace, cr and c-g
(defun inputat(ln q response)
  (oxo-debug "inputat\n")
  (goto-line ln)
  (beginning-of-line)
  (kill-to-eol)
  (insert-string (concat q response))
  (message "")
  (update-display)
  (setq key (getch))
  (cond
    ((eq key "\n") response)
    ((is_ctl_g key) "")
    ((is_backspace key) (inputat ln q (string-shrink-left response)))
    ((is_control_char key) (inputat ln q response))
    (t (inputat ln q (concat response key)))  ))

(defun draw()
  (oxo-debug "draw\n")
  (beginning-of-buffer)
  (set-mark)
  (repeat 10 next-line)
  (kill-region)
  (beginning-of-buffer)
  (insert-string (concat " " (val 1) " | " (val 2) " | " (val 3) "\n"))
  (insert-string "----------\n")
  (insert-string (concat " " (val 4) " | " (val 5) " | " (val 6) "\n"))
  (insert-string "----------\n")
  (insert-string (concat " " (val 7) " | " (val 8) " | " (val 9) "\n"))
  (repeat 5 newline_and_space)
  (beginning-of-buffer))

(setq wins (list 
  (list 1 2 3)
  (list 4 5 6)
  (list 7 8 9)
  (list 1 4 7)
  (list 2 5 8)
  (list 3 6 9) 
  (list 1 5 9) 
  (list 3 5 7) ))

(defun check_win_line(w p)
  (oxo-debug (concat "check_win_line w=" w " - p=" p "\n"))
  (and
   (eq p (val (nth 0 w)))
   (eq p (val (nth 1 w)))
   (eq p (val (nth 2 w))) ))

(defun check_for_win(l p)
  (oxo-debug (concat "check_for_win l=" l " - p=" p "\n"))
  (cond
    ((null l)
     (oxo-debug "empty l\n")
     'nil)
    ((check_win_line (car l) p)
     (oxo-debug "check_win_line true\n")
     t)
    (t
     (oxo-debug "repeat\n")
     (check_for_win (cdr l) p)) ))
  
(defun game_over()
  (oxo-debug "game_not_over\n")
  (or
   (check_for_win wins "X")
   (check_for_win wins "O")
   (board_full (cdr board)) ))

(defun get-move()
  (oxo-debug "get-move\n")
  (setq m (inputat 7 "Your move (X): " ""))
  (setq m (string-to-number m))
  (cond
    ((or (> m 9) (< m 1)) (msg "Please select a free cell between 1 and 9" t) (get-move))
    ((taken m) (msg "That cell is taken" t) (get-move)) )
  m)

(defun find_free(b)
  (cond
    ((taken (car b)) (find_free (cdr b)))
    ((string-to-number (car b)))))

(defun board_full(brd)
  (oxo-debug "board_full\n")
  (cond
    ((null brd))
    ((taken (car brd)) (board_full (cdr brd)))))

(defun taken(v)
  (or (eq "X" v) (eq "O" v) (eq "E" v)) )

(defun msg(s pause)
  (cond (pause
	 (print_message (concat s " - press a key to continue "))
	 (getch))
	((print_message s)) ))

(defun print_message(s)
  (oxo-debug "print_message\n")
  (clearline 7)
  (insert-string s)
  (message "")
  (update-display))

(defun clearline(ln)
  (oxo-debug "clearline\n")
  (goto-line ln)
  (beginning-of-line)  
  (kill-to-eol) )

;; just find first empty square
(defun computer_move()
  (oxo-debug "computer_move\n")
  (setq board (set-nth board (find_free board) "O")) )

(defun show_result()
  (draw)
  (cond
    ((check_for_win wins "X") (msg "X wins !" t))
    ((check_for_win wins "O") (msg "O wins !" t))
    ((msg "Draw !" t)) ))

(defun play_again()
  (setq m (inputat 8 "Play again (y or n) ? " ""))
  (clearline  8)
  (or (eq m "y") (eq m "Y")) )

(defun play()
   (oxo-debug "play\n")
  (draw)
  (oxo-debug "about to update display\n")
  (update-display)
  (oxo-debug "updated\n")
  (cond ((not (game_over)) (setq board (set-nth board (get-move) "X")))
	((show_result)) )
  (cond ((not (game_over))
	 (computer_move)
	 (play))
	((show_result)) ))

(defun oxo()
  (init)
  (play)
  (cond
    ((play_again) (oxo))
    (t
     (msg "Thank you for playing" t)
     (kill-buffer "*oxo*")
     (clearline 8)
     (message "")) ))

(provide 'oxo)
