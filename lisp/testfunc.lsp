
;(load_script "testfunc.lsp")


(setq test_buf1 "temp1.txt")
(setq test_buf2 "temp2.txt")
(setq test_log_buf "*testlog*")

(defun foxline()
  (insert-string "the quick brown fox jumps over the lazy dog\n"))

(defun log-test-result(s)
  (setq obuf (get-buffer-name))
  (select-buffer test_log_buf)
  (end-of-buffer)
  (insert-string s)
  (select-buffer obuf))

(defun test1()
  (select-buffer test_buf1)
  (foxline)
  (kill-buffer test_buf1)
  (select-buffer test_buf1)
  (foxline)
  (select-buffer test_buf2)
  (kill-buffer test_buf1)
  (log-test-result "TEST1: buffer functions PASSED\n"))

(defun create-test-file()
  (select-buffer test_buf1)
  (repeat 10 foxline)
  (beginning-of-buffer)
  (set-mark)
  (goto-line 10)
  (copy-region)
  (yank))

(defun run-tests()
  (test1)
  (create-test-file)
  (select-buffer test_log_buf)
  (insert-string "TESTs completed\n"))


;(run-tests)

;;(search-forward "fox")
;;(search-backward "fox")


;ascii
;ascii->number
;backspace
;backward-char
;- backward-page
;- backward-word
;beginning-of-buffer
;beginning-of-line
;- clear-message-line
;copy-region
;delete
;delete-other-windows
;end-of-buffer
;end-of-line
;eval-block
;exit
;find-file
;forward-char
;- forward-page
;- forward-word
;get-buffer-count
;get-buffer-name
;get-char
;get-clipboard
;get-key
;get-key-funcname
;get-key-name
;get-point
;- get-version-string
;getch
;goto-line
;insert-string
;kill-buffer
;kill-region
;list-buffers
;load
;log-message
;log-debug
;message
;next-line
;number-to-string
;numberp
;getenv
;other-window
;page-down
;page-up
;previous-line
;prompt
;refresh
;save-buffer
;search-backwards
;search-forward
;select-buffer
;- set-clipboard
;set-key
;set-mark
;set-point
;- shell-command
;split-window
;string-to-number
;string-append
;string-length
;string.ref
;substring
;string-trim
;stringp
;update-display
;yank

