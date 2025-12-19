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
	 (default (if (null filename) (getcwd)
		      (file-name-directory filename) ))
	 (response (prompt-filename (concat "Dired (directory): " default))) )
    (dired (if response  response  default)) ))


(defun dired (directory)
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
    (message "dired menu: f,x")
    (update-display)
    (unless (eq (dired_process-key) :quit)  (dired-loop (- ops 1))) ))

(defun dired_process-key ()
  (let ((key (get-key)))
    (if (eq key "") (dired_handle-arrow-key (get-key-funcname))
	(dired_handle-command-key key) )))

(defun dired_handle-arrow-key (func)
  (log 'DEBUG nil "dired: arrow key: "func)
  (when (memq (intern func)
	      ;; Note: backward_page is registered with different names on
	      ;;   different key combinations in key.c. Emacs has scroll-up
	      ;;   and scroll-down
	      '(previous-line next-line  forward-page page-down page-up
		forward-word forward-char  backward-word backward-char
		beginning-of-line end-of-line  beginning-of-buffer end-of-buffer))
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
    ((memq key '("x" "q"))
     (switch-to-buffer (other-buffer))
     (message "")
     :quit )
    ((memq key '("f" "\n"))
     (let* ((info  (dired-get-info))
	    (type  (car info))
	    (name  (cdr info))
	    (path  (if (string-equal name "..")  (file-name-directory (buffer-filename))
			   (concat (buffer-filename) "/" name) )))
       (log 'DEBUG nil "dired: " info)
       (cond ((string-equal type "d") (dired path) :quit)
	     ((string-equal type "-") (switch-to-buffer (find-file-noselect path)) :quit)
	     (t (message (concat "Error: cannot open file of type: " type))) )))
    ((eq key "?") (log 'DEBUG nil "dired: info '"(car (dired-get-info))"' '"(cdr (dired-get-info))"'\n"))
    (t (log 'DEBUG nil "dired: unhandled command key="key)) ))

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

(provide 'dired)
