;;
;; record a keyboard macro and make it available in a buffer called *macro*
;; evaluate it so that macro can be executed by calling (mymacro)
;;   c-x (  ;; starts the recording
;;   c-x )  ;; stops the recording
;;   c-x e  ;; executes the macro
;;   esc-e  ;; executes the macro
;;
(require 'femto)

(setq dm-buffer "*macro*")
(setq dm-max-ops 30)
(setq dm-ops 0)
(setq dm-recording nil)

(defun dm-record-key(k)
  (setq dm-obuf (get-buffer-name))
  (cond
    ((eq k "\n")
     (insert-string "\n")
     (select-buffer dm-buffer)
     (insert-string "\n (insert-string \"\\n\")")
     (select-buffer dm-obuf)
     (update-display))
    (t
     (insert-string k)
     (select-buffer dm-buffer)
     (insert-string "\n (insert-string \"")
     (insert-string k)
     (insert-string "\")")
     (select-buffer dm-obuf)
     (update-display))))

(defun dm-record-action(k)
  (setq dm-obuf (get-buffer-name))
  (execute-key)
  (select-buffer dm-buffer)
  (insert-string "\n ")
  (insert-string k)
  (select-buffer dm-obuf)
  (update-display))

(defun dm-init()
  (setq dm-ops 0)
  (setq dm-recording t)
  (setq dm-obuf (get-buffer-name))
  (kill-buffer dm-buffer)
  (select-buffer dm-buffer)
  (insert-string "(defun mymacro()")
  (select-buffer dm-obuf))

(defun dm-start-macro()
  (dm-init)
  (message "c-x ( macro recording started")
  (update-display)
  (dm-get-key))

(defun dm-end-macro()
  (cond
    (dm-recording
     (setq dm-recording nil)
     (setq dm-obuf (get-buffer-name))
     (select-buffer dm-buffer)
     (insert-string ")\n")
     (setq dm-point (get-point))
     (set-point 0)
     (set-mark)
     (set-point dm-point)
     (eval-block)
     (select-buffer dm-obuf)
     (setq dm-ops (+ 1 dm-max-ops))
     (message "c-x ) macro recording completed"))
    (t (message "macro recording is not in progress")) ))

;; prompt for string and return response, handle backspace, cr and c-g
(defun dm-get-key()
  (setq key (get-key))
  (cond
    ((> (setq dm-ops (+ 1 dm-ops)) dm-max-ops) (dm-end-macro))
    ((eq key "")
     (cond
       ((eq "(dm-end-macro)" (get-key-funcname)) (dm-end-macro))
       ((is_ctl_g key))
       (t (dm-record-action (get-key-funcname)) (dm-get-key))))
    (t
     (dm-record-key key)
     (dm-get-key))))

;;
;; define the key bindings
;;

(set-key "c-x (" "dm-start-macro")
(set-key "c-x )" "dm-end-macro")
(set-key "c-x e" "mymacro")
(set-key "esc-e" "mymacro")

(provide 'defmacro)
