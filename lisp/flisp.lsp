;; flisp Language

;; Standard Lisp and Scheme functions

(require 'core)

(defun listp (o) (cond ((null o)) ((consp o))))

(defun nthcdr (i l)
  (cond
    ((not (integerp i))
     (throw wrong-type-argument
       (concat "(nthcdr i l) - i expected type-integer, got: " (type-of i))
       i))
    ((< i 0) (throw range-error "negative index" i))
    ((null l) nil)
    ((= 0 i) l)
   ((not (consp l))
    (throw wrong-type-argument
      (concat "(nthcdr i l) - l expected type-cons, got: " (type-of l))
      l ))
    (t (nthcdr (- i 1) (cdr l)))))

(defun nth (n list)
  (car (nthcdr n list)))

(defun fold-right (f o l)
  (cond
    ((null l) o)
    (t (f (car l) (fold-right f o (cdr l))))))

(defun unfold (func o p)
  (cond ((p o) (cons o nil))
	(t (cons o (unfold func (func o) p)))))

(defun iota (count . args)
  (let (
	(count (- count 1))
	(start (cond ((car args)) (t 0)))
	(step (cond ((cadr args)) (t 1))))
    (let (
	  (func (lambda (n) (setq count (- count 1)) (+ n step)))
	  (pred (lambda (n) (= 0 count))))
      (unfold func start pred))))


;;; property lists

(defun prop-get (l k . p)
  (cond ((not (consp l)) (throw :invalid-value "(prop-get l k[ p]) - l is not a list" l)))
  (setq p (cond (p (car p)) (t eq)))
  (cond
     ((p (car l) k)
      (cond ((consp (cdr l)) (cadr l)) (t (cdr l))))
     (t (cond ((consp (cdr l)) ; (p' v ...)
	     (cond ((consp (cddr l)) (prop-get (cddr l) k))) )) )) )

;;
;; Commonly used Lisp functions which are not other wise used in the Femto libraries
;;
(defun atom (o) (null (consp o)))
(defun zerop (n) (= n 0))

(defun equal (o1 o2)
  (or (and (atom o1) (atom o2)
	   (eq o1 o2))
      (and (consp o1) (consp o2)
	   (equal (car o1) (car o2))
	   (equal (cdr o1) (cdr o2)))))

(provide 'flisp)
