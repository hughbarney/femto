
(insert-string "\n ")

(gotoline 10)

(beginning-of-line)

(kill-to-eol)

(message "")

(display)

(setq key (getch))

(debug "(draw)\n")

(beginning-of-buffer)

(set-mark)

(repeat 10 next-line)

(kill-region)

(beginning-of-buffer)






