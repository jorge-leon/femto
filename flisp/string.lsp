;;
;;  string function library
;;
;;
;;  Built Into fLisp
;;    (string-append s a)
;;    (length s)
;;    (substring s st end)
;;    (string-search haystack needle)
;;


;; trim all spaces from front of a string
(defun string-trim-front(s)
  (cond ((eq (substring s 0 1) " ") (string-trim-front (substring s 1)))
	(t s)))

;; trim all spaces from back of a string
(defun string-trim-back(s)
  (cond  ((eq (substring s -1)  " ") (string-trim-back (substring s 0 -1)))
	 (t s)))

;; trim spaces off front and back of a string
(defun string-trim(s)
  (string-trim-back (string-trim-front s)))


;;
;; string-ref , get character at position r
;;   zero based indexing
;;
(defun string-ref (s r)
   (substring s r (+ r 1)))


;;
;; string-startswith - return t if string starts with search
;;
(defun string-startswith (str search)
  (eq 0 (string-search search str)))


;;
;; shrink string right by dropping off the first char
;;
(defun string-shrink-right(s)
  (substring s 1))


;;
;; shrink string left by dropping off last char
;;  
(defun string-shrink-left(s)
  (substring s 0 -1))


;;
;; return first char of string
;;
(defun string-first-char(s)
  (substring s 0 1))

;;
;; return last char of string
;;
(defun string-last-char(s)
  (substring s -1))

;;;
;;; Test if string is empty string
;;;
(defun string-empty-p (s)
  (string-equal "" s))

;;
;; Split string s at each substring f and return list of parts.
;; If f is the empty string, split all characters.
;;
(defun string-split (f s)
  (let loop ((parts nil) (s s) (i 0) (l (length f)))
       (cond
	 ((and (string-empty-p s) (string-empty-p f)) parts)
	 ((null (setq i (string-search f s))) (append parts (list s)))
	 (t
	  (loop
	   (append parts (list (substring s 0 (cond ((i= 0 l) 1) (t i)))))
	   (substring s (+ i (max l 1)))
	   0
	   l )))))

(provide 'string)


