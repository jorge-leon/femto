;; -*-Lisp-*-
;;
;; File utilities
;;

(require 'string)

;; (mkdir path . parentp)
;;
;; If path is "", "." or ".." return t.
;; If parentp is nil do plain fmkdir on path.
;; Otherwise:
;; Create parent directories if needed, then directory.
;;
;; Return nil if created successfully, not nil if already exists.
;;
;; Note: we do not sanitize path in any way.
;;
(defun mkdir (path . parentp)
  (cond
    ((memq path '("." ".." "/" "")))
    ((not parentp)  (fmkdir path))
    (t
     (let loop ((parts (string-split "/" path)) (prefix "") (result nil))
	  ;; /a/b//c -> "" "a" "b" "" "c"
	  ;; a/b//c -> "a" "b" "" "c"
	  ;; successively test for existence of directory "prefix"/"part", create if not exists
	  ;;              |            prefix
	  ;;              |     empty      |  x  |
	  ;; (car parts)--------------------------
	  ;;        empty | /(cadr parts)  |  x  |
	  ;;            a |       a        | x/a |
	  ;;
	  (cond ((null parts)  (not (car result)))
		;; Note: we could check if we have "" after the first path segment and skip over it a//b/c => a "" b c
		(t
		 (cond ((string-empty-p prefix)
			(cond ((string-empty-p (car parts))
			       (setq parts (cdr parts)  prefix (concat "/" (car parts))) )
			      (t (setq prefix (car parts))) ))
		       (t
			(cond ((string-empty-p (car parts)) ; skip empty path segments
			       (loop (cdr parts) prefix result))
			      (t (setq prefix (concat prefix "/" (car parts)))) )))
		 (setq result (catch (fstat prefix)))
		 (log-debug (concat "prefix "prefix" result "result"\n"))
		 (cond ((car result)
			(log-debug (concat"creating directory "prefix"\n"))
			(log-debug (concat "code "(car result)" message '"(cadr result)"' object"(caddr result)"\n"))
			(fmkdir prefix)	)) ; bail out on error
		 (loop (cdr parts) prefix result) ))))))

(defun file-name-directory (s)
  (cond ((memq s '("" "." ".."))  nil)
	(t
	 (let loop ((parts (string-split "/" s)) (d ""))
	      (cond ((null (cdr parts)) (cond ((string-equal "" d) nil) (t d)))
		    (t (loop (cdr parts) (string-append d (string-append (car parts)  "/")))) )))))

;;; Note: conforms to Elisp, though I'd rather 'nil' the empty string, . and ..
(defun file-name-nondirectory (s)
  (cond	((string-equal s "/") "")
	(t  (car (reverse (string-split "/" s)))) ))

(defun file-name-extension (s)
  (let ((name (file-name-nondirectory s)))
    (let ((extension (car (reverse (string-split "." name)))))
      (cond
	((eq extension name)  nil)
	((eq extension "")    nil) ; . and ..
	(t  extension) ))))

(provide 'file)
