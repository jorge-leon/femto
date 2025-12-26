;; -*-Lisp-*-
;;
;; Core fLisp extensions
;;

(bind list (lambda args args) t)

(bind defmacro
      (macro (name params . body)
	     (list 'bind name (list (quote macro) params . body) 't) )
      t )

;;; conditionals

(defmacro if (pred then . else)
  (cond (else  (list 'cond (list pred then)  (cons 't else)))
	(t     (list 'cond (list pred then))) ))

(defmacro if-not (pred else . then)
  (cond (then  (list 'cond (cons pred then) (list 't else)))
	(t     (list 'cond (list pred 'nil) (list 't else))) ))

(defmacro when (pred . body)
  (cond (body  (list 'cond (cons pred body)))) )

(defmacro unless (pred . body)
  (cond (body (list 'cond (list pred nil) (cons 't body)))) )

(bind defun
      (macro (name params . body)
	     (list 'bind name (list (quote lambda) params . body) t))
      t )

;;; Accessors
(defun cadr (l) (car (cdr l)))
(defun cddr (l) (cdr (cdr l)))
(defun caddr (l) (car (cdr (cdr l))))
(defun caar (l)  (car (car l)))
(defun cdar (l)  (cdr (car l)))
(defun caaar (l) (car (car (car l))))
(defun cdaar (l) (cdr (car (car l))))

;; (setq) => nil
;; (setq a): error
;; (setq a b) => (bind a b t)
;; (setq a b ...) => (progn (setq a b) (setq ...))
(defmacro setq args
  (when args
    (unless (cdr args)
      (throw wrong-number-of-arguments "(setq [s v ..]) expects a multiple of 2 arguments") )
    (if (null (cddr args))  (list 'bind (car args) (cadr args) t)
	(list 'progn
	      (list 'setq (car args) (cadr args))
	      (cons 'setq (cddr args)) ))))

(defun curry (func arg1)
  (lambda (arg2) (func arg1 arg2)))

(defun typep (type object)  (same type (type-of object)))

(setq
 integerp (curry typep type-integer)
 doublep (curry typep type-double)
 stringp (curry typep type-string)
 symbolp (curry typep type-symbol)
;;; consp is a primitive
 lambdap (curry typep type-lambda)
 macrop (curry typep type-macro)
 streamp (curry typep type-stream))

(defun mapcar (func xs)
  (cond (xs (cons (func (car xs)) (mapcar func (cdr xs))))))

;; (let bindings[ body])
;; (let label[ bindings[ body])
;; (let ()) => nil
;; (let (bindings)) => nil
(defmacro let (b-or-l . args)
  (cond
    ((null b-or-l) nil)
    ((consp b-or-l) ; ((bindings) body)
     (if args
	 (cons ; apply
	  (cons 'lambda (cons (mapcar car b-or-l) args)); vars
	  (mapcar cadr b-or-l) ))) ; values
    ((symbolp b-or-l) ; (label (bindings) body)
     ;; bindings: (car args)
     ;; body:     (cdr args)
     (list
      (list 'lambda ()
;;;	    (list 'define (car args)
	    (list 'bind b-or-l
		  (cons 'lambda (cons (mapcar car (car args)) (cdr args))))
	    (cons b-or-l (mapcar cadr (car args))) )))
    (t (throw wrong-type-argument "(let bindings body) - bindings expected type-consp or type-symbol, got: " (type-of (car args)))) ))

;; (let* () body) => ((lambda () body))
;; (let* ((var val) ..) body) =>  ((lambda (var) (let* (..) body)) val)
(defmacro let* (bindings . body)
  (if (null bindings)  (list (cons 'lambda (cons (list) body)))
      (unless (and (consp bindings)  (consp (car bindings)))
	(throw wrong-type-argument "(let* bindings[ body]) - bindings: does not start with a binding" bindings))
      (cons (cons 'lambda (cons (list (caar bindings)) (list (cons let* (cons (cdr bindings) body)))))  (cdar bindings)) ))


(defun prog1 (arg . args) arg)

(defun string (o)
  ;; Convert argument to string.
  ;; Common Lisp
  (cond
    ((eq nil o) "")
    ((stringp o) o)
    ((symbolp o) (symbol-name o))
    ((consp o) (string-append (string (car o)) (string (cdr o))))
    (t (let ((f (open "" ">")))
	 (write o t f)
	 (prog1
	     (cadr (file-info f))
	   (close f) )))))

;; Concatenate all arguments to a string.
;; Elisp
(defun concat args
  (cond
    ((eq nil args) "")
    ((eq nil (cdr args)) (string (car args)))
    (t (string-append (string (car args)) (concat (cdr args)))) ))

(defun assert-type (o type s)
  (cond ((not (same (type-of o) type))
	 (throw invalid-value (concat s ": expected "type", got " (type-of o)) o))))

(defun assert-number (o s)
  (cond ((not (numberp o))
	 (throw invalid-value (concat s ": expected number, got " (type-of o)) o))))

(defun numberp (o) (cond  ((integerp o)) ((doublep o))))

; Schemish list filtering
(defun filter (p l)
  (when l
    (if (p (car l))  (cons (car l) (filter p (cdr l)))
	(filter p (cdr l)) )))

(defun remove (p l)
  (when l
    (if (p (car l))  (remove p (cdr l))
	(cons (car l) (remove p (cdr l))) )))

(defun fold-left (f i l)
  (if (null l)  i
      (fold-left f (f i (car l)) (cdr l)) ))

(defun flip (func)  (lambda (o1 o2) (func o2 o1)))
(defun reverse (l)  (fold-left (flip cons) nil l))

;;; https://www.scheme.com/tspl2d/objects.html#g2052
(defun append lists
  (let f ((ls nil) (lists lists))
       (if (null lists)  ls
	   (let g ((ls ls))
		(if (null ls)  (f (car lists) (cdr lists))
		    (if (consp ls) ls
			(throw invalid-value
			  (concat "(append lists) - list expected type-list, got " (type-of ls)) ))
		    (cons (car ls) (g (cdr ls))) )))))


(defun apply (f . args)
  (if  (null args)  (f)
       (let ((rev (reverse args)))
	 ;; if last element is list splice it
	 (if (consp (car rev))  (eval (cons f (append (reverse (cdr rev)) (car rev))))
	     (f . args)) )))

(defun print (o . fd)
  (if fd  (write o t (car fd))
      (write o t) ))

(defun princ (o . fd)
  (if fd  (write o nil (car fd))
      (write o nil) ))

(defun string-to-number (string)
  (let ((f (open string "<")) (result nil))
    (setq  result (catch (read f)))
    (close f)
    (if (car result)  0
	(if (numberp (caddr result))  (caddr result)
	    0 ))))
	  
(defun eq (o1 o2)
  (cond
    ((same o1 o2))
    ((same (type-of o1) (type-of o2))
     (cond
       ((stringp o1) (string-equal o1 o2))
       ((integerp o1) (i= o1 o2))
       ((doublep o1) (d= o1 o2))))))

(setq not null)

(defun length (o)
  (cond
    ((null o) 0)
    ((stringp o) (string-length o))
    ((consp o)
     (fold-left (lambda (x y) (+ x 1)) 0 o))
    (t (throw wrong-type-argument "(length object) - expected type-cons or type-string" o))))


(defun memq (o l)
  ;; If object o in list l return sublist of l starting with o, else nil.
  ;; Elisp
  (cond
    ((eq nil l) nil)
    ((eq o (car l)) l)
    (t (memq o (cdr l)))))

;;; Wrap all math to Integer operations
(defun nfold (f i l);  (3)  (1 2 3)
  (cond
    ((null l) i)
    ((null (cdr l)) (f i (car l)))
    ( t (fold-left f (f (car l) (cadr l)) (cddr l)))))

(defun fold-leftp (predicate start list)
  (cond ((null list))
	((predicate start (car list)) (fold-leftp predicate (car list) (cdr list)))))

(cond ((car (catch d=)) ;; only integer operations available
       (defun + args (fold-left i+ 0 args))
       (defun - args (nfold     i- 0 args))
       (defun * args (fold-left i* 1 args))
       (defun / args (nfold     i/ 1 args))
       (defun =  (n . args) (fold-leftp i=  n args))
       (defun <  (n . args) (fold-leftp i<  n args))
       (defun <= (n . args) (fold-leftp i<= n args))
       (defun >  (n . args) (fold-leftp i>  n args))
       (defun >= (n . args) (fold-leftp i>= n args)) )
      (t ;; with floating point we need argument coercion for arithmetics
       (defun coerce (ifunc dfunc x y)
	 (cond  ((doublep x) (if (integerp y)  (dfunc x (double y))  (dfunc x y)))
		((doublep y) (if (integerp x)  (dfunc (double x) y)  (dfunc x y)))
		(t (ifunc x y)) ))
       
       (defun coercec (ifunc dfunc) ; coerce "curry"
	 (lambda (x y) (coerce ifunc dfunc x y)))
       
       (defun +  args (fold-left (coercec i+ d+)  0 args))
       (defun -  args (nfold     (coercec i- d-)  0 args))
       (defun *  args (fold-left (coercec i* d*)  1 args))
       (defun /  args (nfold     (coercec i/ d/)  1 args))
       (defun %  args (nfold     (coercec i% d%)  1 args))
       (defun =  (n . args) (fold-leftp (coercec i=  d=)  n args))
       (defun <  (n . args) (fold-leftp (coercec i<  d<)  n args))
       (defun <= (n . args) (fold-leftp (coercec i<= d<=)  n args))
       (defun >  (n . args) (fold-leftp (coercec i>  d>)  n args))
       (defun >= (n . args) (fold-leftp (coercec i>= d>=)  n args)) ))

(defun min (n . args)
  (assert-number n "(min n[ arg ..]) n")
  (fold-left (lambda (a b) (cond ((> a b) b) (t a))) n args) )
(defun max (n . args)
  (assert-number n "(max n[ arg ..]) n")
  (fold-left (lambda (a b) (cond ((< a b) b) (t a))) n args) )

;;; Note: in CL and Scheme (or 'a) => a when a is unbound, and the value of a otherwise.
;;;   We don't know how to produce this (easily?) in fLisp
;;;   So, when a is unbound we throw an error
(defmacro or args
  (cond (args
	 (list 'cond (list (car args))
	       (list 't (cons 'or (cdr args)))))))

(defmacro and args
  (cond
    ((null args))
    ((null (cdr args)) (car args))
    (t (list 'cond (list (car args) (cons 'and (cdr args)))))))

;;; Concatenate each element of l with separator f
(defun join (f l)
  (let loop ((s "") (l l))
       (cond
	 ((null l) "")
	 ((null (cdr l)) (concat s (car l)))
	 (t  (loop (concat s (car l) f) (cdr l))) )))

;; load
(defun fload (f)
  (let loop ((o  nil) (r nil))
       (setq o (read f :eof))
       (if (eq o :eof)  r
	   (setq r (eval o))
	   (loop nil r) )))

(defun load args
  (let ((f (open (car args))))
    (prog1 (fload f)
      (close f))))

;; Features
(setq features nil)

(defun provide args
  ;; args: (feature [subfeature ..])
  ;; Elisp, subfeatures not implemented
  (if (memq (car args) features)  (car args)
      (setq features (cons (car args) features)) ))

(defun require (feature . args)
  ;; args: (feature [filename [noerror]])
  ;; Elisp optional parameters not implemented
  (if (memq feature features)  feature
      ;; Emacs optionally uses provided filename here
      (let* ((path (concat script_dir "/" (symbol-name feature) ".lsp"))
	     (r (catch (load path))) )
	(and (null (car r)) (memq feature features) feature) )))

(provide 'core)
