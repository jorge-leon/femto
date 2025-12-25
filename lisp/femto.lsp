;; -*-Lisp-*-
;;
;; Basic Femto extensions
;;

(require 'flisp)
(require 'string)
(require 'file)

;;; Utilities

;;; Emacs compatibility
(setq command-line-args argv)
(setq invocation-name argv0)

(defun load-script(fn)
  (load (concat script_dir "/" fn)))

(defun insert-file-contents-literally (path)
  (let* ((result (fstat path))
	 (size  (prop-get result :size))
	 (type  (prop-get result :type))
	 (check (unless (eq type "f")  (throw invalid-value "not a regular file" filename)))
	 (fd    (open path))
	 (len   (buffer-fread fd size)) )
    (close fd)
    (when (not (= size len))
      (throw io-error (concat "short read: expected "size" bytes, got "len) path) )))

(defun insert-file ()
  (setq fn (prompt-filename "Insert file: "))
  (cond (fn (insert-file-contents-literally fn))))


;; OS interaction
;; Note: this emulates the original femto shell-command.
;;   The move from C to Lisp allows implementation of more
;;   powerful system interaction in the future
;; Note: we have (popen) now, which should be used instead of shell-exec.
(defun shell-command arg
  (let ((command nil))
    (cond
      (arg (setq command (string-trim (concat arg))))
      (t (setq command (prompt-filename "Command: "))))
    (cond (command (shell-exec command)))))

(defun shell-exec (command)
  (cond ((string-equal "-" (substring command 0 1))
	 (throw invalid-value "shell command must not start with a hypen" command)))
  (setq temp (get-temp-file))
  (setq rc (system (concat command " > " temp " 2>&1 </dev/null")))
  (cond
    ((eq command ""))
    ((or (eq rc -1) (eq rc 127))
	(concat "error: failed to execute" command ": "  rc))
    (t
     (cond (not (eq rc 0)) (message "warning: " command " exited: " rc))
     (switch-to-buffer "*output*")
     (erase-buffer)
     (insert-file-contents-literally temp)
     (system (concat "rm -f " temp))
     (clear-message-line) )))

(defun shell-command-lines (command . args)
  (let* ((cmd (join " " (cons command args)))
	 (stream  (popen cmd)) )
    (let loop ((lines nil))
	 (let ((line (fgets stream)))
	   (if (eq line end-of-file) (shell_pclose stream (reverse lines))
	       (loop (cons (shell_strip_eol line) lines))) ))))

(defun shell_pclose (stream output . allowed)
  (let ((rc (pclose stream)))
    (if (memq rc (or allowed '(0))) output
	(throw io-error (concat "exit status:"rc) output) )))

(defun shell_strip_eol (line)
  (if (memq (substring line -1) '("\n" "\r"))
      (shell_strip_eol (substring line 0 -1))
      line ))

(defun repeat (n func)
  (cond ((> n 0) (func) (repeat (- n 1) func))))

;;; Note: propably inefficient implementation of (posix-filename) validation.
;;; (string-restrict-chars-p rl s)
;;; Returns t if all characters in s are members of list of charachters rl
(defun string-restrict-chars-p (rl s)
  (fold-left and t (mapcar (curry (flip memq) rl) (string-split "" s))) )

(setq file-name_posix_chars
      (curry string-restrict-chars-p
	     (string-split "" "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789._-/") ))

(defun posix-filename (s)
  (and (not (string-equal "-" (substring s 0 1)))
       (consp (file-name_posix_chars s)) ))


;; delete next word
(defun delete-next-word()
  (backward-word)
  (forward-word)
  (set-mark)
  (forward-word)
  (kill-region))

;; previous word
(defun delete-previous-word()
  (forward-word)
  (backward-word)
  (forward-char)
  (set-mark)
  (backward-word)
  (backward-word)
  (forward-char)
  (kill-region))

(defun kill-to-eol()
  (cond
    ((eq (get-point) (get-point-max)) nil)
    ((eq "\n" (get-char)) (delete))
    (t
     (set-mark)
     (end-of-line)
     (cond ((eq (get-point) (get-mark)) (delete))
	   (t (kill-region))) )))

;; some keystroke checks that we will use later
(defun is_ctl_g(k)
  (eq k (ascii 7)))

(defun is_escape(k)
  (eq k (ascii 27)))

(defun is_backspace(k)
  (or (eq k (ascii 8)) (eq k (ascii 127))))

(defun is_ctl_s(k)
  (eq k (ascii 19)))

(defun is_control_char(k)
  (and (>= (ascii->number k) 0) (<= (ascii->number k) 31)))


;; prompt for a keystroke then show its name
(defun describe-key()
  (message "Describe key: ")
  (update-display)
  (let ((key (get-key)))
    (if-not (eq key "") (message key)
	    (message (concat (get-key-name) " runs command " (get-key-funcname))) )))

;;
;; GNU Emacs style lisp interaction.
;; Place cursor behind an s-expression, type C-] and the
;; block will be evaluated.
;;

;; find the end of the s-expression and set cursor on next cell
(defun find_end_p()
  (setq k (get-char))
  (cond
    ((eq 0 (get-point)) -1)
    ((eq ")" k) (forward-char) (get-point))
    ((or (eq "" k) (eq " " k) (eq "\t" k) (eq "\n" k)) (backward-char) (find_end_p))
    (t -1) ))

;; find the start of the s-expression
;; assumes start is always in first character of line
;; this means comments and strings dont need to be handled
(defun find_start_p()
  (beginning-of-line)
  (setq kyy (get-char))
  (cond
    ((and (eq 0 (get-point)) (not (eq kyy "("))) -1)
    ((eq kyy "(") (get-point))
    (t (previous-line) (find_start_p)) ))

;;
;; find the start and end of the s-expression
;; set the mark and the start and point at the end
;; call eval-block
(defun find_and_eval_sexp()
  (setq o_point (get-point))
  (setq lb_count 0)
  (setq rb_count 0)
  (setq start_p -1)
  (setq end_p (find_end_p))
  (cond ((> end_p -1) (setq start_p (find_start_p))))
  (cond
    ((and (> start_p -1) (> end_p -1))
     (set-point start_p)
     (set-mark)
     (set-point end_p)
     (eval-block)
     (message ""))
    (t
     (set-point o_point)
     (cond
       ((eq -1 start_p) (message "could not find start of s-expression"))
       ((eq -1 end_p) (message "could not find end of s-expression"))) )))

(defun transpose-chars ()
  (cond
    ((= (get-point) 0) (message "Beginning of buffer"))
    (t
      (cond
	((eq (get-char) "\n")
	  (setq p (get-point))
	  (backward-char)
	  (transpose-chars)
	  (set-point p))
	(t
	  (backward-char)
	  (setq c (get-char))
	  (delete)
	  (forward-char)
	  (insert-string c))))))

;;; Hooks
(defun run-hook (hookvar)
  (log 'DEBUG nil "(run-hook "hookvar)
  (let process-hook-function ((hook (eval hookvar)))
       (log 'DEBUG nil "hook: "(eval hookvar)", elements: "(length hook))
       (cond
	 ((null hook)  nil)
	 ((consp hook)
	  (let ((result (catch ((car hook)))))
	    (log 'DEBUG result)
	    (when (car result)
	      (log-debug (concat "(run-hook hook) failed on function: "(car hook)": "result"\n")) )
	    (process-hook-function (cdr hook)) ))
	 (t  (throw invalid-value "(run-hook hook) - hook is not a list" hook)) )))

(defmacro run-hooks hooks
  (when hooks
    (list 'progn
	  (list 'catch (list 'run-hook (car hooks)))
	  (cons 'run-hooks (cdr hooks)) )))

;;; Note: pending a working implementation
;; (defun add-hook (hook-sym function-sym)
;;   (unless (memq function-sym (eval hook-sym))
;;       (bind hook-sym (cons function-sym (eval hook-sym)) t) ))


;;; Buffers

;; Note: currently buffers are not Lisp objects, their handle is their name string.
(defun buffer-name arg  (if arg  (car arg)  (current-buffer)))

;;; This hook is run after switching to a buffer
(setq after-switch-to-buffer-hook ())

(defun current-buffer () (buffer-next))

(defun switch-to-buffer (name)
  (buffer-show name)
  (let ((result (catch (run-hooks 'after-switch-to-buffer-hook))))
    (log 'DEBUG result "after-switch-to-buffer-hook") )
  name )

(defun restore-buffer-modified-p (bool)  (buffer-modified-p (current-buffer) bool))
(defun set-buffer-modified-p (bool)  (buffer-modified-p (current-buffer) bool) (refresh))

(defun buffer-list ()
  (let ((current (current-buffer)))
    (let loop ((buffers (list current)) (next (buffer-next current)))
	 (if (eq next current) (reverse buffers)
	     (loop (cons next buffers) (buffer-next next)) ))))

(defun buffer-basename (name)  (car (string-split "/" name)))

(defun buffer-list_filtered (name)
  (filter (lambda (b)  (string-equal (buffer-basename b) name)) (buffer-list)) )

(defun buffer-name_split (name)
  (let ((parts (string-split "/" name)))
    (if (cdr parts) (cons (car parts) (string-to-number (cadr parts)))
	(cons (car parts) 0) )))

(defun buffer-name_index (name)  (cdr (buffer-name_split name)))

(defun generate-new-buffer-name (name)
  (let ((index (buffer-name_maxindex name)))
    (if index (concat name "/" (+ 1 index))
	name )))

(defun buffer-name_maxindex (name)
  (when (memq name (buffer-list))
    (apply max (mapcar buffer-name_index (buffer-list_filtered name))) ))

(defun create-file-buffer (filename)
  (get-buffer-create (generate-new-buffer-name (file-name-nondirectory filename))) )

;;; (rename-buffer name[ unique-p])
(defun rename-buffer (name . opts)
  (if (and opts (car opts))  (set-buffer-name (generate-new-buffer-name name))
      (set-buffer-name name) ))

(defun next-buffer ()  (switch-to-buffer (buffer-next (current-buffer))))
(defun other-buffer ()
  (let ((visible (remove buffer-special-p (buffer-list))))
    (if visible (car visible)
	(car (filter buffer-special-p (buffer-list))) )))

;;; args: start end, default beginning, end
(defun insert-buffer-substring-no-properties (from-buffer-or-name . args)
  (let* ((start    (if args  (car args)  0))
	 (end      (and args (cdr args) (cadr args)))
	 (current  (current-buffer)) )
    (set-buffer from-buffer-or-name)
    (set-point start)
    (set-mark)
    (if end (set-point end)
	(end-of-buffer) )
    (copy-region)
    (set-buffer current)
    (insert-string (get-clipboard)) ))

;;; Interactive
(defun kill-buffer-interactive ()
  (let ((response (prompt "Kill buffer: " (buffer-name))))
    (if (string-equal "" response) (message "Canceled")
	(kill-buffer response) )))

;;; (kill-buffer[ name])
;;; Offers saving modified buffers, switches to other buffer if the buffer to kill is current.
(defun kill-buffer args
  (let* ((current (current-buffer))
	 (buffer  (if (null args) current
		      (set-buffer (car args)) )))
    (if (not (buffer-modified-p)) (kill-buffer_and_switch buffer current)
	(let ((response
	       (prompt (concat "Buffer "buffer" modified; kill anyway? (yes/no/save and then kill) "))))
	  (cond ((string-equal response "yes")  (kill-buffer_and_switch buffer current))
		((string-equal response "save")
		 (save-buffer)
		 (kill-buffer_and_switch buffer current) )) ))))

(defun kill-buffer_and_switch (buffer current)
  (if (eq buffer current)  (switch-to-buffer (other-buffer))
      (set-buffer current) )
  (delete-buffer buffer) )

;;; find-file

(defun find-file ()
  (let ((filename (prompt-filename "Find file: ")))
    (if-not filename  (message "Canceled")
	    (let ((buffer (find-buffer-visiting filename)))
	      (if buffer  (switch-to-buffer buffer) ; file already loaded
		  (switch-to-buffer (find-file-noselect filename))  )))))

(defun find-file-noselect (filename)
  (let ((result (catch (open filename "r+"))))
    (cond
      ((null (car result))                   (find-file_load (caddr result) filename nil))
      ((eq (car result) :permission-denied)  (find-file_load (open filename) filename t))
      ((eq (car result) :not-found)          (find-file_new filename))
      ((eq (car result) :is-directory)       (find-file_directory filename))
      (t (throw (car result) (cadr result) filename)) )))

(defun find-file_load (fd filename read-only)
  (let* ((current (current-buffer))
	 (new (set-buffer (create-file-buffer filename)))
	 (result
	  (catch
	      (progn
		(insert-file-contents-literally filename)
		(set-visited-filename filename)
		(restore-buffer-modified-p nil)
		;; Note: read-only mode pending implementation
		(when read-only  (buffer-modified-p nil "read-only"))
		(after-find-file) ))))
    (set-buffer current)
    (when (car result) (apply throw result))
    new ))

(defun find-file_new (filename)
  (let* ((current (current-buffer))
	 (new (set-buffer (create-file-buffer filename)))
	 (result
	  (catch
	      (progn
		(set-visited-filename filename)
		(restore-buffer-modified-p nil)
		(after-find-file) ))))
    (when (car result) (apply throw result))
    (message "(New file)")
    new ))

(setq
 find-file-extension-highlight-mode
      '("c" C  "h" C  "cpp" C
	"rc"  Lisp  "lsp" Lisp
	"py"  Python)
      find-file-hook nil)

(defun after-find-file ()
  (buffer-mode nil (prop-get find-file-extension-highlight-mode (file-name-extension (buffer-filename))))
  (run-hooks 'find-file-hook) )

(defun find-file_directory (filename)
  ;; Note: Workaround for current dired implementation.  Instead
  ;; dired should receive the directory as filename and it/we
  ;; should provide a file buffer with (set-visited-filename
  ;; directory)
  (setq dired-dir filename)
  (dired)
  nil ; signals not to switch to a buffer.
  )

(defun save-buffer ()
  (if (not (buffer-modified-p)) (message "(No changes need to be saved)")
      (let* ((filename (buffer-filename))
	    (directory (file-name-directory filename)) )
	;; Note: we should handle unassociated files with write-contents-functions hooks.
	(when (null filename)
	  (throw invalid-value "(save-buffer) - is not associated with a file" (current-buffer)) )
	;; Note: we should run the special hook write-file-functions here.
	(when (and directory (memq directory '("" "./" "../")))
	  (when (car (catch (fstat directory)))
	    (if (not (string-equal "y" (prompt (concat "Directory ‘"directory"’ does not exist; create? (y or n) "))))
		(message "Cancelled")
		;; else
		(mkdir directory t) )))
	(let* ((fp    (open filename "w"))
	       (point (get-point))
	       (size  (get-point-max))
	       (len   (progn
			(beginning-of-buffer)
			(buffer-fwrite fp))) )
	  (set-point point)
	  (close fp)
	  (unless (i= len size)  (throw io-error "(save-buffer) - error writing buffer to file: " filename))
	  (set-buffer-modified-p nil)
	  (message (concat "Wrote "size" bytes to "filename)) ))))

(defun write-file ()
  (let ((path (prompt-filename "Write file: ")))
    (if (null path)  (message "Quit")
	(let ((path (write-file_check_path path)))
	  (if (null path)  (message "Canceled")
	      (set-visited-filename path)
	      (rename-buffer (generate-new-buffer-name (file-name-nondirectory path)))
	      (save-buffer) )))))

(defun write-file_check_path (path)
  (let* ((result (catch (fstat path)))
	 (type (if (car result)  ""   (prop-get (caddr result) :type))) )
    (cond ((car result) path) ; not found: go ahead / not accessible: err later
	  ((string-equal type "f")
	   ;; Found. Ask if we want to overwrite, if not return nil
	   (when (string-equal "y" (prompt (concat "File '"path"' exists; overwrite? (y or n) ") "")) path) )
	  ((string-equal type "d")
	   ;; directory: create file path into directory from buffer name and check again
	   (write-file_check_path (concat path "/" (buffer-basename (buffer-name)))) )
	  (t  (throw invalid-value "Not a valid file or directory" path)) )))

;;; Saving buffers

;;; (save-some-buffers) - interactively save modified file buffers
;;; returns last "query" mode, see save-buffer_query.
;;;
(defun save-some-buffers ()
  (let* ((current   (current-buffer))
	 (response
	  (let loop ((buffers (filter buffer-modified-p (remove buffer-special-p (buffer-list)))) (mode :ask))
	       (if (null buffers) mode
		   (setq mode (save-buffer_query (car buffers) mode))
		   (if (memq mode '(abort break)) mode
		       (loop (cdr buffers) mode) )))))
    (set-buffer current)
    response ))

;;; (save-buffer_query buffer mode) - interactively save buffer if file buffer
;;; mode:
;;; - ask ..
;;; - force .. save w/o asking
;;; returns:
;;; - ask   .. while asking interactively
;;; - abort .. if user quits with q
;;; - break .. if user enters . - save this buffer and exit
;;; - force .. if user enters ! - save all remaining buffers and exit
;;;
;;; Sets the current buffer and does not restore it
(defun save-buffer_query (buffer mode)
  (set-buffer buffer)
  (let ((filename  (buffer-filename buffer)))
    (if (not filename) mode ; skip buffers w/o file association
	(if (eq mode :force) (save-buffer) :force
	    (let ((response  (prompt (concat "Safe file "filename"? (y, n, !, ., q,) "))))
	      (cond ((eq mode :force) (save-buffer) :force)
		    ((eq response "y")  (save-buffer) :ask)
		    ((eq response "n") :ask)
		    ((eq response "!") (save-buffer) :force)
		    ((eq response ".") (save-buffer) :break)
		    ((eq response "q") :abort)
		    (t :abort) ))))))

(defun save-buffers-kill-terminal ()
  (unless  (eq :abort (save-some-buffers))
    (if-not (filter buffer-modified-p (remove buffer-special-p (buffer-list))) (exit)
	(when (string-equal "yes" (prompt "Modified buffers exist; exit anyway? (yes or no) "))
	  (exit) ))))

(provide 'femto)
