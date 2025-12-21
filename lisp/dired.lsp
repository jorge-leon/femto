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
;; Supported operations, * means - in progress:
;; + .. dired-create-directory
;; C .. dired-do-copy
;; D .. dired-do-delete
;; G .. dired-do-chgrp
;; M .. dired-do-chmod
;; O .. dired-do-chown
;; R .. dired-do-rename
;; S .. dired-do-symlink       *
;; T .. dired-do-touch         *
;; Y .. dired-do-relsymlink    *
;; Z .. dired-do-compress      *
;; f, RET .. dired-find-file
;; g .. "revert-buffer" - dired-reload
;; q .. "quit-window"

(require 'femto)

(setq dired-ls-cmd "ls -lA -- ")
(setq dired-max-ops 300)

(defun dired-interactive ()
  (let* ((filename (buffer-filename))
	 ;; Emacs convention: directory names are given with a trailing /
	 (default (if (null filename) (string-append (getcwd) "/")
		      (file-name-directory filename) ))
	 (directory (prompt-filename "Dired (directory): " default)) )
    (if-not directory  (message "Canceled")
	    (when (string-equal "" directory)  (setq directory default))
	    (unless (string-equal "/" (substring directory 0 1))
	      (setq directory (concat (getcwd) "/" directory)) )
	    (dired directory) )))

(defun dired (directory)
  (unless (string-equal "/" (substring directory -1))
    (setq directory (string-append directory "/")) )
  (fstat directory) ; throws error if path with trailing slash is not a directory

  (delete-other-windows)
  (let ((buffer (find-buffer-visiting directory)))
    (unless buffer
      (setq buffer (generate-new-buffer (generate-new-buffer-name "*dired*")))
      (set-buffer buffer)
      (set-visited-filename directory) )
    (switch-to-buffer buffer) ))

;;; Hook function
(defun dired-after-switch-to-buffer-function ()
  ;; Note: to be replaced by (eq (buffer-mode) 'dired) or similar function
  (when (string-startswith (buffer-name) "*dired*")
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
      (message "dired: q, g, + C, D, G, M O, R, f/Ret") ))


(defun dired-loop (ops)
  (update-display)
  (let* ((result (catch (dired_process-key)))
	 (code  (caddr result)) )
    (log 'DEBUG nil "dired:"ops": process-key: '"result"'")
    (log 'DEBUG result)
    (if (cond
	  ((i= ops 0) nil)
	  ((eq code :quit) nil)
	  ((car result) (message (join "\n" code)) :continue)
	  ((eq code :cancel) (message "Canceled") :continue)
	  ((eq code :update) (dired-reload) :continue)
	  ((eq code :no-deletions) (message "No deletions performed") :continue)
	  (:continue) )
	(dired-loop (- ops 1))
	;; quit
	(restore-buffer-modified-p nil)
	(switch-to-buffer (other-buffer))
	(clear-message-line) )))

(defun dired_process-key ()
  (let ((key (get-key)))
    (if-not (eq key "")  (dired_handle-command-key (intern key))
	    (dired_handle-arrow-key (get-key-funcname))
	    :continue )))

(defun dired_handle-arrow-key (func)
  (log 'DEBUG nil "dired: arrow key: "func)
  (when (memq (intern func)
	      ;; Note: backward_page is registered with different names on
	      ;;   different key combinations in key.c. Emacs has scroll-up
	      ;;   and scroll-down
	      '(previous-line next-line  forward-page page-down page-up
		forward-word forward-char  backward-word backward-char
		beginning-of-line end-of-line  beginning-of-buffer end-of-buffer
		find-file))
    (let* ((input   (open func "<"))
	   (result  (catch (eval (read input)))) )
      (close input)
      (if-not (car result)  (apply (caddr result))
	      (log 'DEBUG result "dired: arrow-key") )))
  (when (memq (intern func) '(previous-line next-line))
    (beginning-of-line)
    (repeat 8 forward-word) ))

(defun dired_handle-command-key (key)
  (cond
    ((eq key 'q)  :quit)
    ((eq key 'g)  :update)
    ((eq key '+)  (dired-create-directory-interactive) :update)
    ((eq key 'C)  (dired-do-copy-interactive) :update)
    ((eq key 'D)  (dired-do-delete) :update)
    ((eq key 'G)  (dired-do-chgrp) :update)
    ((eq key 'M)  (dired-do-chmod) :update)
    ((eq key 'O)  (dired-do-chown) :update)
    ((eq key 'R)  (dired-do-rename) :update)
    ((or (eq key 'f) (eq key (intern "\n")))
     (let* ((info  (dired-get-info))
	    (type  (car info))
	    (name  (cdr info))
	    (path  (if (string-equal type "d")  (concat (buffer-filename) name "/")
		       (string-append (buffer-filename) name)) ))
       (log 'DEBUG nil "dired: info "info", path "path)
       (cond ((string-equal type "d") (dired path) :quit)
	     ((string-equal type "-") (switch-to-buffer (find-file-noselect path)) :quit)
	     (t (message (concat "Error: cannot open file of type: " type))) )))
    (t (message (concat "dired: unhandled key="key))) ))

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
(defun dired-do-delete args
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
	    (shell-command-lines "chgrp" response path "2>&1") )))

(defun dired-do-chmod ()
  (let* ((name  (cdr (dired-get-info)))
	 (path  (concat (buffer-filename) name))
	 (response (prompt (concat "Change Mode of "name" to: "))) )
    (if-not response :cancel
	    (shell-command-lines "chmod" response path "2>&1") )))

(defun dired-do-chown ()
  (let* ((name  (cdr (dired-get-info)))
	 (path  (concat (buffer-filename) name))
	 (response (prompt (concat "Change Owner of "name" to: "))) )
    (if-not response :cancel
	    (shell-command-lines "chown" response path "2>&1") )))

(defun dired-do-rename ()
  (let* ((name  (cdr (dired-get-info)))
	 (path  (concat (buffer-filename) name))
	 (response (prompt-filename (concat "Rename "name" to: ") path)) )
    (if-not response :cancel
	    (shell-command-lines "mv" path response "2>&1") )))



(provide 'dired)
