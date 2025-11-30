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

(defun repeat (n func)
  (cond ((> n 0) (func) (repeat (- n 1) func))))

;; OS interaction
;; Note: this emulates the original femto shell-command.
;;   The move from C to Lisp allows implementation of more
;;   powerful system interaction in the future
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

(defun insert-file ()
  (setq fn (prompt-filename "Insert file: "))
  (cond (fn (insert-file-contents-literally fn))))

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
  (show-prompt "Describe Key: " "")
  (setq key (get-key))
  (cond
    ((not (eq key "")) (message key))
    (t (message (concat (get-key-name) " runs command " (get-key-funcname))))))

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
  (let process-hook-function ((hook (eval hookvar)))
    (cond
      ((null hook)  nil)
      ((consp hook)
       (let ((result (catch ((car hook)))))
	 (cond ((car result)
		(log-debug (concat "(run-hook hook) failed on function: "(car hook)": "result"\n")) ))
	 (process-hook-function (cdr hook)) ))
      (t  (throw invalid-value "(run-hook hook) - hook is not a list\n")) )))

(defmacro run-hooks hooks
  (cond
    (hooks
     (list 'progn
	   (list 'catch (list 'run-hook (car hooks)))
	   (cons 'run-hooks (cdr hooks)) ))))

;;; Buffers

(defun create-file-buffer (filename)
  (generate-new-buffer (file-name-nondirectory filename))) ; Note: we must uniqify here

;;; find-file

(defun find-file ()
  (let ((filename (string-trim (prompt-filename "Find file: "))))
    (let ((buffer (find-buffer-visiting filename)))
      (cond (buffer  (switch-to-buffer buffer)) ; file already loaded
	    ((setq buffer (find-file-noselect filename))  (switch-to-buffer buffer)) ))))

(defun find-file-noselect (filename)
  (let ((result (catch (open filename "r+"))) (fd nil) (ro nil) (directory nil) (buffer nil))
    (cond
      ((null (car result))                   (find-file_load (caddr result) filename nil))
      ((eq (car result) :permission-denied)  (find-file_load (open filename) filename t))
      ((eq (car result) :not-found)          (find-file_new filename))
      ((eq (car result) :is-directory)       (find-file_directory filename))
      (t (throw (car result) (cadr result) filename)) )))

(defun find-file_load (fd filename read-only)
  (let ((result (fstat filename)) (size 0) (type nil))
    (setq
     size (prop-get result :size)
     type (prop-get result :type) )
    (cond ((not (eq type "f"))  (throw :invalid-value "neither file nor directory" filename)))

    ;; Note: Emacs does not switch here, but rather in find-file
    (switch-to-buffer (create-file-buffer filename))
    (set-visited-filename filename)
    (buffer-fread size fd)
    (close fd)
    (cond (read-only
	   ;; Note: read-only mode pending implementation
	   (add-mode "read-only") ))
    (after-find-file)
    (get-buffer-name) ))

(defun find-file_new (filename)
  ;; Note: Emacs does not switch here, but rather in find-file
  (switch-to-buffer (create-file-buffer filename))
  (set-visited-filename filename)
  (message "(New file)")
  (after-find-file)
  (get-buffer-name) )

(setq
 find-file-extension-highlight-mode
      '("c" "cmode"  "h" "cmode"  "cpp" "cmode"
	"rc"  "lispmode"  "lsp" "lispmode"
	"py"  "python")
      find-file-hook nil)

(defun after-find-file ()
  (let ((mode (prop-get find-file-extension-highlight-mode (get-buffer-file-extension))))
    (log-debug (concat "afer-find-file:mode: "mode"\n"))
    (cond (mode  (add-mode mode))))
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


;;;
(setq primitive-write-file write-file)
(defun femto-write-file ()
  (let ((p (prompt-filename (get-buffer-filename))))
    (let ((d (file-name-directory p)))
      (cond ((car (catch (fstat d)))
	     (cond ((string-equal "y" (response (prompt (concat "Directory ‘"d"’ does not exist; create? (y or n) ") "")))
		    (mkdir (file-name-directory d) t)
		    (throw 'not-implemented "write buffer to file" p) )
		   (t message "Cancelled") ))
	    (t (throw 'not-implemented "write buffer to file" p) )))))

(provide 'femto)
