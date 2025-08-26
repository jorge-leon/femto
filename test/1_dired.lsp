;;; Note: dummy test suite; for now.
(defun dummy-1 () "failure simulation")
;;; Note: the following line would go into a macro '(test comment-string body)'
(setq tests (cons (cons "dummy-1 this test always fails" dummy-1) tests))
;;; Like this:
;;;(test "dummy-1 this test always fails" "failure simulation")
;;;(pr tests); debug

(defun dummy-2 () "success simulation" nil)
(setq tests (cons (cons "dummy-2 this test always succeeds" dummy-2) tests))

(defun dummy-3 () (throw 'we-just-fail-here "showcase of an error in a test function"))
(setq tests (cons (cons "dummy-3 this test throws an error" dummy-3) tests))

(require 'dired)

(defun de-up-dir-1 ()
  (cond ((not (eq (de-up-dir "/home/hugh") "/home")))))
(setq tests (cons (cons "de-dir-up-1 moves up 1 subdir" de-up-dir-1) tests))

(defun de-up-dir-2 ()
  (cond ((not (eq (de-up-dir "/") "/")))))
(setq tests (cons (cons "de-dir-up-2 root returns root" de-up-dir-2) tests))
