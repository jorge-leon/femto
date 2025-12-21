;;
;; Dired extension for Femto
;;
;;
;; use the arrow keys to move up or down the list
;; then select one of the following letters
;;
;; Suggested first implementation does the following
;;   f or CR - if a directory change to that directory and reload the ls output
;;   f or CR - open the file in the editor
;;   x       - exit dired
;;
;; This is just a start. Completion of this DIRED is left as challenge for the
;; reader.  For an example see the dired.cmd sample that was written for MicroEMACS
;;
;;
;;
;; (load "path/dired.lsp")   ;; to load
;; (dired)                   ;; to call
;;
;;
(require 'femto)

(setq dired-ls-cmd "ls -la -- ")
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
	    (unless (string-equal "/" (substring directory -1))
	      (setq directory (string-append directory "/")) )
	    (fstat directory) ; throws error if path with trailing slash is not a directory
	    (dired directory) )))

(defun dired (directory)
  (delete-other-windows)
  (let ((buffer (find-buffer-visiting directory)))
    (unless buffer
      (setq buffer (generate-new-buffer (generate-new-buffer-name "*dired*")))
      (set-buffer buffer)
      ;; Note: we suppose to get called with a directory.
      ;;   Should we check?
      (set-visited-filename directory) )
    (switch-to-buffer buffer) ))

;;; Hook function
(defun dired-after-switch-to-buffer-function ()
  ;; Note: to be replaced by (eq (buffer-mode) 'dired) or similar function
  (when (string-startswith (buffer-name) "*dired*")
    (dired-reload (current-buffer))
    (dired-loop dired-max-ops) ))

;;Note: until this works
;;   (add-hook 'after-switch-to-buffer-hook 'dired-after-switch-to-buffer-function)
;; We do the deed manually. Or better, we implement per mode or buffer keymaps
(setq after-switch-to-buffer-hook (cons 'dired-after-switch-to-buffer-function after-switch-to-buffer-hook))

(defun dired-reload (buffer)
  (let* ((current (current-buffer))
	 (dummy   (unless (eq buffer current)  (set-buffer buffer)))
	 (stream (popen (concat dired-ls-cmd (buffer-filename buffer)))) )
    (erase-buffer)
    (buffer-fread stream)
    (pclose stream)
    (beginning-of-buffer)
    (set-mark)
    (goto-line 3)
    (kill-region)
    (beginning-of-buffer)
    (buffer-flag-modified buffer nil)
    (unless (eq buffer current) (set-buffer current)) ))

(defun dired-loop (ops)
  (unless (i= 0 ops)
    (message "dired menu: q-uit, g reload, + mkdir, C-opy, D-elete, f/RET find-file or dired")
    (update-display)
    (unless (eq (dired_process-key) :quit)  (dired-loop (- ops 1))) ))

(defun dired_process-key ()
  (let ((key (get-key)))
    (if (eq key "") (dired_handle-arrow-key (get-key-funcname))
	(dired_handle-command-key (intern key)) )))

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
    ;; Note: all are "interactive"
    ;; + .. dired-create-directory
    ;; C .. dired-do-copy
    ;; D .. dired-do-delete
    ;; G .. dired-do-chgrp         *
    ;; M .. dired-do-chmod         *
    ;; O .. dired-do-chown         *
    ;; R .. dired-do-rename        *
    ;; S .. dired-do-symlink       *
    ;; T .. dired-do-touch         *
    ;; Y .. dired-do-relsymlink    *
    ;; Z .. dired-do-compress      *
    ;; f, RET .. dired-find-file
    ;; g .. "revert-buffer" - dired-reload
    ;; q .. "quit-window"
  (cond
    ((eq key 'q)
     (switch-to-buffer (other-buffer))
     (message "")
     :quit )
    ((eq key 'g)  (dired-reload (current-buffer)))
    ((eq key '+)  (dired-create-directory-interactive))
    ((eq key 'C)  (dired-do-copy-interactive))
    ((eq key 'D)  (dired-do-delete))
    ((or (eq key 'f) (eq key (intern "\n")))
     (let* ((info  (dired-get-info))
	    (type  (car info))
	    (name  (cdr info))
	    (path  (cond
		     ((string-equal name "..")
		      (file-name-directory (substring (buffer-filename) 0 -1)) )
		     ((string-equal type "d")  (concat (buffer-filename) name "/"))
		     (t (string-append (buffer-filename) name)) )))
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
    (if-not directory  (message "Canceled")
	    (mkdir directory t)
	    (dired-reload (current-buffer)) )))

(defun dired-do-copy-interactive ()
  (let* ((info    (dired-get-info))
	 (isdir   (string-equal "d" (car info)))
	 (name    (cdr info))
	 (source  (string-append (buffer-filename) name))
	 (prompt  (if isdir "Copy "  "Copy file "))
	 (cmd     (if isdir "cp -a "  "cp "))
	 (dest  (prompt-filename (concat prompt name" to ") (buffer-filename))))
    (if-not dest (message "Canceled")
	    (system (concat cmd source " " dest))
	    (dired-reload (current-buffer)) )))

;; Note: If args is set is must be the number of following lines to
;;   delete
(defun dired-do-delete args
  (let* ((info     (dired-get-info))
	 (isdir    (string-equal "d" (car info)))
	 (name     (cdr info))
	 (path     (concat (buffer-filename) name))
	 (response (prompt (concat "Delete "name" yes or no "))) )
    (cond ((null response)  (message "Canceled"))
	  ((string-equal response "yes")
	   (if isdir (dired_delete-dir path)
	       (let ((result  (catch (system (concat "rm "path)))))
		 (if (car result)  (message (concat "Error: "(cadr result)))
		     (dired-reload (current-buffer)) ))))
	  (t  (message "No deletions performed")) )))

(defun dired_delete-dir (path)
  (let* ((stream (popen (concat "ls -A "path)))
	 (line   (prog1 (fgets stream) (pclose stream)))
	 (empty  (eq line end-of-file)) )
    (cond  ((shell-command-lines "ls -A" path)
	    (system (concat "rmdir "path))
	    (dired-reload (current-buffer)))
	   (t
	    (let ((response (prompt (concat "Recursively delete "path"? (yes, no) "))))
	      (cond ((null response) (message "Canceled"))
		    ((string-equal "yes" response)
		     (let ((result (catch (system (concat "rm -rf "path)))))
		       (if (car result) (message (concat "Error: "(cadr result)))
			   (dired-reload (current-buffer)) )))
		    (t (message "No deletions performed")) ))))))

(provide 'dired)
