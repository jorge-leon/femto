;; Commonly used Lisp functions which are not used in the Femto libraries
;; leg20231203
;;

(require 'flisp)

(defun atom (o) (null (consp o)))
(defun zerop (n) (= n 0))

(defmacro if args
  (list 'cond (list (car args) (car (cdr args))) (cons 't (cdr (cdr args)))))


(defun equal (o1 o2)
  (or (and (atom o1) (atom o2)
	   (eq o1 o2))
      (and (consp o1) (consp o2)
	   (equal (car o1) (car o2))
	   (equal (cdr o1) (cdr o2)))))

;;; https://www.scheme.com/tspl2d/objects.html#g2052
(defun append lists
  (let f ((ls nil) (lists lists))
       (cond
	 ((null lists) ls)
         (t
	  (let g ((ls ls))
	       (cond
		 ((null ls) (f (car lists) (cdr lists)))
                 (t
		  (cond ((not (consp ls))
			 (throw invalid-value
			   (concat "(append lists) - list expected type-list, got " (type-of ls))
			   ls)))
		  (cons (car ls) (g (cdr ls)))) ))))))

(defun apply (f . args)
  (cond
    ((null args) (f))
    (t
     (let ((rev (reverse args)))
       (cond
	 ((consp (car rev))
          ;; if last element is list splice it
	  (eval (cons f (append (reverse (cdr rev)) (car rev)))) )
	 (t (f . args)) )))))

(defun print (x . args)
  (cond
    ((null args) (write x :readably t))
    (t (write x :readably t :stream (car args)))))

(defun princ (x . args)
  (cond
    ((null args) (write x :readably nil))
    (t (write x :readably nil :stream (car args)))))


(provide 'stdlib)
