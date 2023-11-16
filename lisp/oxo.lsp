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

(defun oxo_debug(s)
 (if debug_oxo (debug s) s))

(defun init()
 (oxo_debug "(init)\n")
 (select-buffer "*oxo*")
 (beginning-of-buffer)
 (end-of-buffer)
 (kill-region)
 (setq board (list "E" "1" "2" "3" "4" "5" "6" "7" "8" "9")))

(defun val(n)
 (nth n board))

(defun nth(n l)
  (cond 
    ((eq n 0) (car l))
    (t (nth (- n 1) (cdr l))) ))

(defun set-nth (list n val)
  (if (> n 0)
    (cons (car list) (set-nth (cdr list) (- n 1) val))
    (cons val (cdr list)) ))

(defun newline_and_space()
  (oxo_debug "newline_and_space\n")
  (insert-string "\n "))

;; prompt for string and return response, handle backspace, cr and c-g
(defun inputat(ln q response)
  (oxo_debug "inputat\n")
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
    ((is_backspace key) (inputat ln q (shrink response)))
    ((is_control_char key) (inputat ln q response))
    (t (inputat ln q (string.append response key)))  ))

(defun draw()
  (oxo_debug "draw\n")
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
  (and (eq p (val (nth 0 w))) (eq p (val (nth 1 w))) (eq p (val (nth 2 w)))) )

(defun check_for_win(l p)
  (cond
    ((eq l ()) nil)
    ((check_win_line (car l) p) t)
    (t (check_for_win (cdr l) p)) ))

(defun game_not_over()
  (and (not (check_for_win wins "X")) (not (check_for_win wins "O")) (not (board_full (cdr board))) ))

(defun get-move()
  (oxo_debug "get-move\n")
  (setq m (inputat 7 "Your move (X): " ""))
  (setq m (string->number m))
  (if (or (> m 9) (< m 1)) (progn (msg "Please select a free cell between 1 and 9" t) (get-move)))
  (if (not (is_free m)) (progn (msg "That cell is taken" t) (get-move)))
  m)

(defun find_free(b)
  (cond
    ((and (not (eq "X" (car b))) (not (eq "O" (car b))) (not (eq "E" (car b)))) (string->number (car b)))
    (t (find_free (cdr b))) ))

(defun is_free(n)
  (and (not (eq "X" (val n))) (not (eq "O" (val n))) (not (eq "E" (val n))) ))

(defun not_taken(v)
  (and (not (eq "X" v)) (not (eq "O" v)) (not (eq "E" v)) ))

(defun board_full(brd)
  (cond
    ((eq brd ()) t)
    ((not_taken (car brd)) nil)
    (t (board_full (cdr brd))) ))

(defun msg(s pause)
  (if pause
  (progn
    (print_message (concat s " - press a key to continue "))
    (getch))
  (progn
    (print_message s)) ))

(defun print_message(s)
  (oxo_debug "print_message\n")
  (clearline 7)
  (insert-string s)
  (message "")
  (update-display))

(defun clearline(ln)
  (oxo_debug "clearline\n")
  (goto-line ln)
  (beginning-of-line)  
  (kill-to-eol) )

;; just find first empty square
(defun computer_move()
  (oxo_debug "computer_move\n")
  (setq board (set-nth board (find_free board) "O")) )

(defun show_result()
  (draw)
  (cond
    ((check_for_win wins "X") (msg "X wins !" t))
    ((check_for_win wins "O") (msg "O wins !" t))
    (t (msg "Draw !" t)) ))

(defun play_again()
  (setq m (inputat 8 "Play again (y or n) ? " ""))
  (clearline  8)
  (or (eq m "y") (eq m "Y")) )

(defun play()
  (oxo_debug "play\n")
  (draw)
  (oxo_debug "about to update display\n")
  (update-display)
  (if (game_not_over) (setq board (set-nth board (get-move) "X")) (show_result))
  (if (game_not_over) (progn (computer_move) (play)) (show_result)) )

(defun oxo()
  (init)
  (play)
  (if (play_again)
    (oxo)
  (progn 
    (msg "Thank you for playing" t)
    (kill-buffer "*oxo*")
    (clearline 8)
    (message "")) ))

;; set to 't' to obtain debug trace
(setq debug_oxo nil)

