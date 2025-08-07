;;
;; show info screen
;;

(defun show-info()
    (select-buffer "*info*")
    (cond ((eq 0 (get-point-max))
	   (insert-string ";******************************************************************************\n")
	   (insert-string ";\n")
	   (insert-string ";Here are some basic commands to explore the Femto editor\n\n\n")
	   (insert-string ";  esc-l describe-bindings     - list all the key bindings\n")
	   (insert-string ";  C-c f describe-functions    - list all the functions\n")
	   (insert-string ";  C-x ? describe-key          - enter a key and show what its binding\n")
	   (insert-string ";  esc-a apropos               - search for commands by a search string\n")
	   (insert-string ";\n\n")
	   (insert-string ";  C-x h show-info             - show this screen\n")
	   (insert-string ";\n")
	   (insert-string ";******************************************************************************\n")
	   
	   ;; add the key bindings
	
	   (insert-string "\n;; KEY BINDINGS \n\n")
	   (kill-buffer "*help*")
	   (describe-bindings)
	   (setq bd (get-buffer-string "*help*"))
	   (select-buffer "*info*")
	   (end-of-buffer)
	   (insert-string bd)
	   
	   ;; add function list
	   
	   (insert-string "\n\n;; FUNCTIONS \n\n")
	   (kill-buffer "*help*")
	   (describe-functions)
	   (setq bd (get-buffer-string "*help*"))
	   (select-buffer "*info*")
           (add-mode "lispmode")
	   (end-of-buffer)
	   (insert-string bd)
           (delete-mode "modified")
	   (kill-buffer "*help*")

	   ;; goto to top of buffer and update screen
           
	   (delete-other-windows)
	   (beginning-of-buffer)
	   (update-display))))


;;
;; return the contents of a buffer
;;
(defun get-buffer-string(tbuf)
   (select-buffer tbuf)
   (set-point 0)
   (set-mark)
   (end-of-buffer)
   (copy-region)
   (get-clipboard))


(provide 'info)
