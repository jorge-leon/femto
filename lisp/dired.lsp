;;
;; Femto directory editor
;;
;; - No support for flagging or marking - only "direct" operations are
;;   available.
;; - No support for numeric prefixes, only the current
;;   entry is operated on.
;; - No support for showing/hiding subdirectories
;;
;; No support for renaming buffers or buffer filenames
;; recursively. Make sure to only operate on files/directories which
;; are not visited.
;;
;; Supported operations, * whishlist
;; + .. dired-create-directory
;; C .. dired-do-copy
;; D .. dired-do-delete
;; G .. dired-do-chgrp
;; M .. dired-do-chmod
;; O .. dired-do-chown
;; R .. dired-do-rename
;; S .. dired-do-symlink
;; T .. dired-do-touch
;; Y .. dired-do-relsymlink    * 
;; Z .. dired-do-compress      *
;; f, RET .. dired-find-file
;; o .. dired-fine-file in other window *
;; g .. "revert-buffer" - dired-reload
;; ^ .. dired-up-directory
;; j .. dired-goto-file        *
;; q .. "quit-window"

(require 'femto)

(setq dired-ls-cmd "ls -lA -- ")
(setq dired-max-ops 300)

(defun dired-interactive ()
  (let* ((filename (buffer-filename))
	 ;; Emacs convention: directory names are given with a trailing /
	 (default (if filename  (or (file-name-directory filename) "")
		      (string-append (getcwd) "/") ))
	 (directory (prompt-filename "Dired (directory): " default)) )
    (if-not directory  (message "Canceled")
	    (when (string-equal "" directory)  (setq directory default))
	    (unless (string-equal "/" (substring directory 0 1))
	      (setq directory (concat (getcwd) "/" directory)) )
	    (dired directory) )))

(defun dired (directory)  (switch-to-buffer (dired_init directory)))

(defun dired_init (directory)
  (unless (string-equal "/" (substring directory -1))
    (setq directory (string-append directory "/")) )
  (fstat directory) ; throws error if path with trailing slash is not a directory

  (delete-other-windows)
  (let ((buffer (find-buffer-visiting directory)))
    (unless buffer
      (setq buffer (get-buffer-create (generate-new-buffer-name "*dired*")))
      (buffer-mode buffer 'Dired)
      (buffer-readonly-p buffer t)
      (buffer-undo-p buffer nil)
      (buffer-special-p buffer t)
      (set-buffer buffer)
      (set-visited-file-name directory) )
    buffer ))

;;; Hook function
(defun dired-after-switch-to-buffer-function ()
  (when (eq (buffer-mode) 'Dired)
    (dired-reload)
    (dired-loop dired-max-ops) ))

;;Note: until this works
;;   (add-hook 'after-switch-to-buffer-hook 'dired-after-switch-to-buffer-function)
;; We do the deed manually. Or better, we implement per mode or buffer keymaps
(setq after-switch-to-buffer-hook
      (cons dired-after-switch-to-buffer-function
	    after-switch-to-buffer-hook))

(defun dired-reload ()
  (let* ((point (get-point))
	 (stream (popen (concat dired-ls-cmd (buffer-filename)))) )
      (erase-buffer)
      (buffer-fread stream)
      (pclose stream)
      (beginning-of-buffer)
      (repeat 2 kill-to-eol) ; remote "Total" line
      (set-point point)
      (restore-buffer-modified-p nil)
      (message "dired: q, g, ^, + C, D, G, M O, R, S, T, f/Ret") ))


(defun dired-loop (ops)
  ;;;
  ;;; dired_process-key returns either
  ;;; - a symbol to indicate an action:
  ;;;   - :quit .. quit and switch to an"other" bufer
  ;;;   - :exit .. exit Femto
  ;;;   - :continue .. the command loop
  ;;;   - :update   .. reload the buffer, then :continue
  ;;;   - :cancel   .. show cancel command message, then :continue
  ;;;   - :no-deletions .. show no deletions message, then :continue
  ;;; - a cons (:switch buffer) to quit and switch to buffer
  (update-display)
  (let* ((other (other-buffer))
	 (result (catch (dired_process-key)))
	 (code  (caddr result)) )
    (log 'DEBUG nil "dired:"ops": process-key: '"result"'")
    (log 'DEBUG result "dired: result "ops)
    (if (cond
	  ((i= ops 0) nil)
	  ((memq code '(quit exit)) nil)
	  ((and (consp code) (eq (car code) :switch))
	   (setq other (cdr code))
	   nil )
	  ((car result) (message (join "\n" code)) :continue)
	  ((eq code :cancel) (message "Canceled") :continue)
	  ((eq code :update) (dired-reload) :continue)
	  ((eq code :no-deletions) (message "No deletions performed") :continue)
	  (:continue) )
	(dired-loop (- ops 1))
	;; quit or exit
	(cond
	  ((eq code :exit) save-buffers-kill-terminal)
	  (t
	   (restore-buffer-modified-p nil)
	   (switch-to-buffer other)
	   (clear-message-line) )))))

(defun dired_process-key ()
  (let ((key (get-key)))
    (if-not (eq key "")  (dired_handle-command-key (intern key))
	    (dired_handle-arrow-key (intern (get-key-funcname))) )))

(defun dired_handle-arrow-key (func)
  (log 'DEBUG nil "dired: arrow key: "func)
  (when (memq func ; list of allowed "arrow" keys
	      '(previous-line next-line  scroll-up scroll-down
		forward-word forward-char  backward-word backward-char
		beginning-of-line end-of-line  beginning-of-buffer end-of-buffer
		next-buffer save-buffers-kill-terminal))
    (eval-expression (concat "("func")")) )
  (cond
    ((eq func 'save-buffers-kill-terminal) :exit)
    ((memq func '(previous-line next-line))
     (beginning-of-line)
     (repeat 8 forward-word)))
  
  :continue )

(defun dired_handle-command-key (key)
  (cond
    ((eq key 'q)  :quit)
    ((eq key 'g)  :update)
    ((eq key '^)  (cons :switch (dired_init-up-directory)))
    ((eq key '+)  (dired-create-directory-interactive))
    ((eq key 'C)  (dired-do-copy-interactive))
    ((eq key 'D)  (dired-do-delete))
    ((eq key 'G)  (dired-do-chgrp))
    ((eq key 'M)  (dired-do-chmod))
    ((eq key 'O)  (dired-do-chown))
    ((eq key 'R)  (dired-do-rename))
    ((eq key 'T)  (dired-do-touch))
    ((eq key 'S)  (dired-do-symlink))
    ((or (eq key 'f) (eq key (intern "\n")))
     (let* ((info  (dired-get-info))
	    (type  (car info))
	    (name  (cdr info))
	    (path  (if (string-equal type "d")  (concat (buffer-filename) name "/")
		       (string-append (buffer-filename) name)) ))
       (log 'DEBUG nil "dired: info "info", path "path)
       (cond ((string-equal type "d") (cons :switch (dired_init path)))
	     ;; Note: 
	     ((memq type '("-" "l")) (cons :switch (find-file-noselect path)))
	     (t (message (concat "Error: cannot open file of type: " type)) :continue) )))
    (t (message (concat "dired: unhandled key="key)) :continue) ))

(defun dired-get-info ()
  (beginning-of-line)
  (let* ((type (get-char))
	 (is-link (string-equal type "l")) )
    (repeat 8 forward-word)
    ;; Note: in case the filename starts with a space
    (backward-word) (forward-char) (forward-char)
    (let ((point (get-point)))
      (set-mark)
      (if-not is-link (end-of-line)
	      (search-forward " ->")
	      (repeat 3 backward-char) )
      (copy-region)
      (set-point point)
      (cons type (get-clipboard)) )))

(defun dired_init-up-directory ()
  (dired_init (file-name-directory (substring (buffer-filename) 0 -1))) )

(defun dired-create-directory-interactive ()
  (let ((directory  (prompt-filename "Create directory " (buffer-filename))))
    (if-not directory  :cancel
	    (mkdir directory t)
	    :update )))

(defun dired-do-copy-interactive ()
  (let* ((info    (dired-get-info))
	 (isdir   (string-equal "d" (car info)))
	 (name    (cdr info))
	 (source  (string-append (buffer-filename) name))
	 (prompt  (if isdir "Copy "  "Copy file "))
	 (dest  (prompt-filename (concat prompt name" to ") (buffer-filename))))
    (if-not dest :cancel
	    (shell-command-lines (if isdir "cp -a "  "cp ") source dest "2>&1")
	    :update )))

;;; Note: We also must delete associated buffers! or prevent deletion
;;; when a buffer is associated
(defun dired-do-delete ()
  (let* ((info     (dired-get-info))
	 (isdir    (string-equal "d" (car info)))
	 (name     (cdr info))
	 (path     (concat (buffer-filename) name))
	 (response (prompt (concat "Delete "name" yes or no "))) )
    (cond ((null response)  :cancel)
	  ((string-equal response "yes")
	   (if isdir (dired_delete-dir path)
	       (shell-command-lines "rm" path "2>&1")
	       :update ))
	  (:no-deletions) )))

(defun dired_delete-dir (path)
  (cond  ((shell-command-lines "ls -A" path "2>&1")
	  (let ((response (prompt (concat "Recursively delete "path"? (yes, no) "))))
	    (cond ((null response) :cancel)
		  ((string-equal "yes" response)
		   (shell-command-lines "rm -rf "path)
		   :update )
		  (:no-deletions) )))
	 (t
	  (shell-command-lines "rmdir "path "2>&1")
	  :update )))

(defun dired-do-chgrp ()
  (let* ((name  (cdr (dired-get-info)))
	 (path  (concat (buffer-filename) name))
	 (response (prompt (concat "Change Group of "name" to: "))) )
    (if-not response :cancel
	    (shell-command-lines "chgrp" response path "2>&1")
	    :update )))

(defun dired-do-chmod ()
  (let* ((name  (cdr (dired-get-info)))
	 (path  (concat (buffer-filename) name))
	 (response (prompt (concat "Change Mode of "name" to: "))) )
    (if-not response :cancel
	    (shell-command-lines "chmod" response path "2>&1")
	    :update )))

(defun dired-do-chown ()
  (let* ((name  (cdr (dired-get-info)))
	 (path  (concat (buffer-filename) name))
	 (response (prompt (concat "Change Owner of "name" to: "))) )
    (if-not response :cancel
	    (shell-command-lines "chown" response path "2>&1")
	    :update )))

;;; Note: should rename a visiting buffer, or disallow renaming
(defun dired-do-rename ()
  (let* ((name  (cdr (dired-get-info)))
	 (path  (concat (buffer-filename) name))
	 (response (prompt-filename (concat "Rename "name" to: ") path)) )
    (if-not response :cancel
	    (shell-command-lines "mv" path response "2>&1")
	    :update )))

(defun dired-do-symlink ()
  (let* ((name  (cdr (dired-get-info)))
	 (directory (buffer-filename))
	 (path      (string-append directory name))
	 (response (prompt-filename (concat "Symlink "name" from: ") (buffer-filename))) )
    (if-not response :cancel
	    (shell-command-lines "ln -s" path response "2>&1")
	    :update )))

(defun dired-do-touch ()
  (let* ((name  (cdr (dired-get-info)))
	 (path  (concat (buffer-filename) name))
	 (response (prompt (concat "Change Timestamp of "name" to (default now): "))) )
    (cond ((null response) :cancel)
	  ((string-equal response "")
	   (shell-command-lines "touch" path "2>&1")
	   :update )
	  (t
	   (shell-command-lines "touch -t" response path "2>&1")
	   :update ))))

(provide 'dired)
