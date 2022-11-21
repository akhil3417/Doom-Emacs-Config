;;; lisp/switch-window-patches.el -*- lexical-binding: t; -*-

(eval-when-compile (require 'switch-window nil t))

;; Make `switch-window' always respect `switch-window-threshold'
(defun +switch-window--then-other-window (prompt function)
  "PROMPT a question and let use select or create a window to run FUNCTION."
  (let ((f (switch-window--get-preferred-function function)))
    (switch-window--then
     prompt
     (lambda ()
       (select-window
        (if (one-window-p)
            (split-window-right)
          (next-window)))
       (call-interactively f))
     (lambda () (call-interactively f))
     nil
     switch-window-threshold)))
(advice-add #'switch-window--then-other-window :override #'+switch-window--then-other-window)

;; Increase `switch-window-threshold' for just `C-x o'.
(defun +switch-window-with-threshold-as-2-a (oldfun &rest args)
  (let ((switch-window-threshold 2))
    (apply oldfun args)))
(advice-add #'switch-window :around #'+switch-window-with-threshold-as-2-a)

(provide 'switch-window-patches)
