;;; Poor mans unit test framework in Femto Lisp

(require 'core)

(setq reportFd (open ">3"))

(defun pr args
  (map1 (lambda (o) (write o :stream reportFd)) args)
  (write "\n" :stream reportFd))

(defmacro test (comment . body)
  ;;; Note: my macro skills fail on me again, this does not work
  (list 'setq 'tests (list 'append 'tests comment (list 'lambda () body))))

(defun ok (num test)
  ;; Ok runs 'test' and prints out a TAP14 conform message
  (let ((result (catch ((cdr test)))))
    (cond ((car result) (pr "not ok " num " - " (car test) " test failed with '" (car result) ": " (cadr result)))
	  ((cond ((caddr result) (pr "not ok " num  "- " (car test) " " (caddr result)))
		((pr "ok " num " - " (car test)))))))
  (+ num 1))

(defun tap (suite)
  (setq tests nil)
  (let ((result (catch (load (concat "test/" suite))))
	(tests nil))
    (cond ((car result) (pr "error: failed to load test suite" suite ":" (cadr result)))
	  (t
	   (pr "TAP version 14")
	   (pr "1.." (length (caddr result)))
	   (pr "# " suite)
	   (fold-left ok 1 (caddr result))))))
