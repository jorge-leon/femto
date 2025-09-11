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

(provide 'stdlib)
