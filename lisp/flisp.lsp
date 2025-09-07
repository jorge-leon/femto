;; flisp Language

;; Standard Lisp and Scheme functions

(require 'core)

(defun listp (o) (cond ((null o)) ((consp o))))

(defmacro and args
  (cond
    ((null args))
    ((null (cdr args)) (car args))
    (t (list 'cond (list (car args) (cons 'and (cdr args)))))))

(defmacro or args  (cons 'cond (mapcar list args)))

(defun reduce (func seq start)
  (cond ((null seq) start)
        (t (reduce func (cdr seq) (func (car seq) start)))))

(defun max (n . args)
  (cond
    ((null (numberp n))
     (throw 'wrong-type-argument "not a number" n))
    ((null args) n)
    (t (reduce
	(lambda (a b) (cond ((< a b) b) (t a)))
	args n))))

(defun min (n . args)
  (cond
    ((null (numberp n))
     (throw wrong-type-argument "not a number" n))
    ((null args) n)
    (t (reduce (lambda (a b) (cond ((< a b) a) (t b)))
	args n)) ))

(defun nthcdr (n l)
  (cond
    ((not (integerp n))
     (throw wrong-type-argument
       (concat "(nthcdr n l) - n expected type-integer, got: " (type-of n))
       n))
    ((< n 0) (throw range-error "negative index" n))
    ((null l) nil)
    ((= 0 n) l)
   ((not (consp l))
    (throw wrong-type-argument
      (concat "(nthcdr n l) - l expected type-cons, got: " (type-of l))
      l))
    (t (nthcdr (- n 1) (cdr l)))))

(defun nth (n list)
  (car (nthcdr n list)))

(defun fold-right (f e l)
  (cond
    ((null l) e)
    (t (f (car l) (fold-right f e (cdr l))))))

(defun unfold (func init pred)
  (cond ((pred init) (cons init nil))
	(t (cons init (unfold func (func init) pred)))))

(defun iota (count . args)
  (let (
	(count (- count 1))
	(start (cond ((car args)) (t 0)))
	(step (cond ((cadr args)) (t 1))))
    (let (
	  (func (lambda (n) (setq count (- count 1)) (+ n step)))
	  (pred (lambda (n) (= 0 count))))
      (unfold func start pred))))

(defun flip (func)  (lambda (o1 o2) (func o2 o1)))
(defun reverse (l)  (fold-left (flip cons) nil l))

(provide 'flisp)
