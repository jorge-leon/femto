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


;;
(setq dired-dir "")
(setq dired-ls-cmd "ls -la ")
(setq dired-name-start-col 47)
(setq dired-dir-start-col 2)
(setq dired-line 1)
(setq dired-start-line 1)
(setq dired-last-line 1)
(setq dired-max-ops 300)
(setq dired-debug nil)

(defun dired-interactive ()
  (let* ((filename (get-buffer-filename))
	 (default (if (null filename) (getcwd)
		      (file-name-directory filename) ))
	 (response (prompt-filename (concat "Dired (directory): " default))) )
    (dired (if response  response default)) ))


(defun dired (directory)
  (delete-other-windows)
  (let ((buffer (find-buffer-visiting directory)))
    (if buffer (switch-to-buffer buffer)
	(let ((buffer  (generate-new-buffer (generate-new-buffer-name "*dired*"))))
	  (switch-to-buffer buffer)
	  (set-visited-filename directory)
	  (dired-reload buffer) )))
  (dired-loop dired-max-ops) )

(defun dired-reload (buffer)
  (let* ((current (current-buffer))
	 (dummy   (unless (eq buffer current)  (set-buffer buffer)))
	 (stream (popen (concat dired-ls-cmd (get-buffer-filename)))) )
    (erase-buffer)
    (buffer-fread stream)
    (pclose stream)
    (beginning-of-buffer)
    (set-mark)
    (goto-line 3)
    (kill-region)
    (dired-init)
    (dired-get-last-line)
    (beginning-of-buffer)
    (dired-get-info)
    (buffer-flag-modified buffer nil)
    (unless (eq buffer current) (set-buffer current)) ))

(defun dired-init ()
  (setq dired-start-line 1)
  (setq dired-last-line 1)
  (setq dired-line 1)
  (end-of-buffer)
  (previous-line)
  (beginning-of-line)
  (dired-insert-space) )

(defun dired-insert-space ()
  (insert-string "  ")
  (previous-line)
  (beginning-of-line)
  (unless (eq " " (get-char))  (dired-insert-space)) )

(defun dired-get-last-line ()
  (setq de-last-line 1)
  (end-of-buffer)
  (previous-line)
  (beginning-of-line)
  (dired-count-line) )

(defun dired-count-line ()
  (when (> (get-point) 0)
    (setq dired-last-line (+ 1 dired-last-line))
    (previous-line)
    (beginning-of-line)
    (dired-count-line) ))

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
      (unless (car result)  (apply (caddr result))) ))
  (when (memq (intern func) '(previous-line next-line))
    (beginning-of-line)
    (repeat 9 forward-word) ))

(defun dired_handle-command-key (key)
  (cond
    ((memq key '("x" "q"))
     (switch-to-buffer (other-buffer))
     (message "")
     :quit )
    ((memq key '("f" "\n"))
     (let* ((info      (dired-get-info))
	    (type      (car info))
	    (name      (cdr info))
	    (path      (if (string-equal name "..")  (file-name-directory (get-buffer-filename))
			   (concat (get-buffer-filename) "/" name) )))
       (log 'DEBUG "dired: " info)
       (cond ((string-equal type "d") (dired path))
	     ((string-equal type "f") (find-file path))
	     (t (message (concat "Error: cannot open file of type: " type))) )))
    ((eq key "?") (log-debug (concat "dired: info '"(car (dired-get-info))"' '"(cdr (dired-get-info))"'\n")))
    (t (log 'DEBUG "dired: unhandled command key="k)) ))

(defun dired-get-info ()
  (beginning-of-line)
  (forward-word)
  (let* ((type (get-char))
	 (is-link (string-equal type "l")) )
    (repeat 8 forward-word)
    ;; Note: in case the filename starts with a space
    (backward-word) (forward-char) (forward-char)
    (set-mark)
    (if-not is-link (end-of-line)
	    (search-forward " ->")
	    (repeat 3 backward-char) )
    (copy-region)
    (cons type (get-clipboard)) ))

;;
;; keep this so we can debug dired if needed
;;
(defun dired-debug(m)
  (when dired-debug (log 'DEBUG "dired:" (concat m "\n"))) )



(provide 'dired)
