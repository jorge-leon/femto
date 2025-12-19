;;
;; show info screen
;;

(defun show-info()
    (switch-to-buffer "*info*")
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
	   (describe-bindings)
	   (insert-buffer-substring-no-properties "*help*")
	   (end-of-buffer)
	   
	   ;; add function list
	   
	   (insert-string "\n\n;; FUNCTIONS \n\n")
	   (describe-functions)
	   (insert-buffer-substring-no-properties "*help*")

           (add-mode "lispmode")
           (restore-buffer-modified-p nil)

	   ;; goto to top of buffer and update screen
           
	   (beginning-of-buffer)
	   (delete-other-windows)
	   (update-display))))


(provide 'info)
