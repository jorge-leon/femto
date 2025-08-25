;;; Note: dummy test suite; for now.
(defun dummy-1 () "failure simulation")
;;; Note: the following line would go into a macro '(test comment-string body)'
(setq tests (append tests (list (cons "dummy-1 this test always fails" dummy-1))))

(defun dummy-2 () "success simulation" nil)
(setq tests (append tests (list (cons "dummy-2 this test always succeeds" dummy-2))))

(defun dummy-3 () (throw 'we-just-fail-here "showcase of an error in a test function"))
(setq tests (append tests (list (cons "dummy-3 this test throws an error" dummy-3))))

