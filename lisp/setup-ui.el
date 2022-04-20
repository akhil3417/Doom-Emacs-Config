;; [[file:../config.org::*setup.ui][setup.ui:1]]
;; -*- lexical-binding: t -*-
(setq inhibit-startup-message t
      inhibit-splash-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)
(fset #'display-startup-echo-area-message #'ignore)

;; Stop the cursor from blinking.
(blink-cursor-mode 0)
;; No fat cursors.
(setq x-stretch-cursor nil)

;; Turn off the menu, tool bars and the scroll bar
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
;; Turn on image viewing
(auto-image-file-mode t)

;; Get rid of the annoying system beep.
(setq ring-bell-function 'ignore)

;; Show me what I type, immediately.
(setq echo-keystrokes 0.01)

;; Middle-click paste at point, not at cursor.
(setq mouse-yank-at-point t)
;; Mouse available in terminal
(add-hook 'tty-setup-hook #'xterm-mouse-mode)


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
(setq indicate-buffer-boundaries t
      indicate-empty-lines nil
      scroll-conservatively 101
      auto-window-vscroll nil)

;;; remove continuation arrow on right fringe
;;; (delq! 'continuation fringe-indicator-alist 'assq)

;; Don't resize emacs in steps.
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

;; The native border "consumes" a pixel of the fringe on righter-most splits,
;; `window-divider' does not. Available since Emacs 25.1.
(setq window-divider-default-places t
      window-divider-default-bottom-width 2
      window-divider-default-right-width 2)
(add-hook 'after-init-hook #'window-divider-mode)

;; No popup dialogs
(setq use-dialog-box nil)

;; native linux tooltips are ugly
(when IS-LINUX
  (setq x-gtk-use-system-tooltips t))

;; Window splitting
;; Set horizontal splits as the default
;; #+begin_src emacs-lisp
;; (setq split-width-threshold 120
;;       split-height-threshold 80)
;; #+end_src
;; or favor vertical splits over horizontal ones?
(setq split-width-threshold 180
      split-height-threshold 80)

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
 delete-by-moving-to-trash t                      ; Delete files to trash
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…"                ; Unicode ellispis are nicer than "...", and also save /precious/ space
      password-cache-expiry nil                   ; I can trust my computers ... can't I?
      ;; scroll-preserve-screen-position 'always     ; Don't have `point' jump around
      scroll-margin 2)                            ; It's nice to maintain a little margin

(display-time-mode 1)                             ; Enable time in the mode-line

;; (unless (string-match-p "^Power N/A" (battery))   ; On laptops...
  (display-battery-mode 1)                       ; it's nice to know how much power you have

(global-subword-mode 1)                           ; Iterate through CamelCase words

;; Underline looks a bit better when drawn lower
(setq x-underline-at-descent-line t)

(if (eq initial-window-system 'x)                 ; if started by emacs command or desktop file
    (toggle-frame-maximized)
  (toggle-frame-fullscreen))

;; (setq default-frame-alist
;;       (append (list
;; 	       ;; '(font . "Roboto Mono Emacs Regular:size=14")
;; 	       '(min-height . 1)  '(height     . 45)
;; 	       '(min-width  . 1) '(width      . 81)
;;                '(vertical-scroll-bars . nil)
;;                '(internal-border-width . 40)
;;                '(left-fringe    . 0)
;;                '(right-fringe   . 0)
;;                '(tool-bar-lines . 0)
;;                '(menu-bar-lines . 0))))

;  (setq centaur-tabs-style "wave")
;  (setq centaur-tabs-set-bar 'under)
;; Note: If you're not using Spacmeacs, in order for the underline to display
;; correctly you must add the following line:
;(setq x-underline-at-descent-line t)
;
(provide 'setup-ui)
;; setup-ui ends here
;; setup.ui:1 ends here
