;;; lisp/better-registers.el -*- lexical-binding: t; -*-

(defun better-registers-decrement-register (number register)
  "Subtract NUMBER from the contents of register REGISTER.
Interactively, NUMBER is the prefix arg."
  (interactive "p\ncDecrement register: ")
  (increment-register (- number) register))

(define-key ctl-x-r-map (kbd "-") #'better-registers-decrement-register)

(provide 'better-registers)
