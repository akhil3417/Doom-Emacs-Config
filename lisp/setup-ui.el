;; [[file:../config.org::*setup.ui][setup.ui:1]]
;; -*- lexical-binding: t -*-
(setq inhibit-default-init t
      initial-scratch-message ";;Hello Sir! how you doing  eh?")
(fset #'display-startup-echo-area-message #'ignore)

;; (auto-image-file-mode t)

;; Get rid of the annoying system beep.
;; (setq ring-bell-function 'ignore)

;; Show me what I type, immediately.
(setq echo-keystrokes 0.01)

;; Mouse available in terminal
;; (add-hook 'tty-setup-hook #'xterm-mouse-mode)


;; Scrolling
(setq scroll-margin 0
      scroll-preserve-screen-position t
      next-screen-context-lines 2)
;;; mouse
;;; (setq mouse-wheel-scroll-amount '(t ((shift) . 2))
;;;       mouse-wheel-progressive-speed t)

;;; (setq-hook! '(eshell-mode-hook term-mode-hook) hscroll-margin 0)

;; Reduce the clutter in the fringes; we'd like to reserve that space for more
;; useful information, like git-gutter and flycheck.
(setq indicate-buffer-boundaries t)

;;; remove continuation arrow on right fringe
;;; (delq! 'continuation fringe-indicator-alist 'assq)

;;; ;;;###package pos-tip
;;; (setq pos-tip-internal-border-width 6
;;;       pos-tip-border-width 1)
;;; ;; Better fontification of number literals in code

;;; (use-package! highlight-numbers
;;;   :hook ((prog-mode conf-mode) . highlight-numbers-mode)
;;;   :config (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

;;; ;;;###package hide-mode-line-mode
;;; (add-hook! '(completion-list-mode-hook Man-mode-hook)
;;;            #'hide-mode-line-mode)

(setq-default
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      truncate-string-ellipsis "…"                ; Unicode ellispis are nicer than "...", and also save /precious/ space
      password-cache-expiry nil                   ; I can trust my computers ... can't I?
      scroll-preserve-screen-position 'always     ; Don't have `point' jump around
      scroll-margin 2)                            ; It's nice to maintain a little margin

;; (display-time-mode 1)                             ; Enable time in the mode-line

;; (unless (string-match-p "^Power N/A" (battery))   ; On laptops...
  ;; (display-battery-mode 1)                       ; it's nice to know how much power you have but i am running a desktop so doesnt make sense

(global-subword-mode 1)                           ; Iterate through CamelCase words

;; Underline looks a bit better when drawn lower
(setq x-underline-at-descent-line t)



;  (setq centaur-tabs-style "wave")
;  (setq centaur-tabs-set-bar 'under)
;; Note: If you're not using Spacmeacs, in order for the underline to display
;; correctly you must add the following line:
;(setq x-underline-at-descent-line t)
;
(provide 'setup-ui)
;; setup-ui ends here
;; setup.ui:1 ends here
