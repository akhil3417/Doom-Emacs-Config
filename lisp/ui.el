;;; lisp/new/ui.el -*- lexical-binding: t; -*-
;;;

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
;; scroll-margin 0 ;; already in doom
(setq scroll-preserve-screen-position t
      next-screen-context-lines 5)
;;; mouse
;;; (setq mouse-wheel-scroll-amount '(t ((shift) . 2))
;;;       mouse-wheel-progressive-speed t)

;; (setq-hook! '(eshell-mode-hook term-mode-hook) hscroll-margin 0);; implemented in doom

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

(setq
 version-control t
 delete-old-versions t
 kept-new-versions 10
 kept-old-versions 5)


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

(setq pixel-scroll-positon-large-scroll-height 40)

(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 50) '(100 . 100)))))
(global-set-key (kbd "C-c T") 'toggle-transparency)
(setq pixel-scroll-positon-large-scroll-height 40)

(setq doom-font (font-spec :family "Fira Code" :size 18 :weight 'light :slant 'italic)
      doom-variable-pitch-font (font-spec :family "Overpass" :size 20)
      doom-big-font (font-spec :family "JetBrains Mono" :size 22 :weight 'light)
      doom-serif-font (font-spec :family "IBM Plex Mono" :size 18 :weight 'light)
      doom-unicode-font (font-spec :name "Noto Color Emoji"))
;; doom-unicode-font (font-spec :family "DejaVu Sans Mono" :size 18))

;; (setq doom-font (font-spec :family "JetBrains Mono" :size 18)
;;       doom-big-font (font-spec :family "JetBrains Mono" :size 27)
;;       doom-variable-pitch-font (font-spec :family "Overpass" :size 20)
;;       doom-unicode-font (font-spec :family "JuliaMono")
;; doom-unicode-font (font-spec :name "Noto Color Emoji"
;;       doom-serif-font (font-spec :family "IBM Plex Mono" :size 17 :weight 'light))
;;
;; Thin grey line separating windows
(set-face-background 'vertical-border "black")
(set-face-foreground 'vertical-border (face-background 'vertical-border))


;; (after! org
;;   ;; Custom regex fontifications
;;   (font-lock-add-keywords 'org-mode
;;                           '(("^\\(?:[  ]*\\)\\(?:[-+]\\|[ ]+\\*\\|\\(?:[0-9]+\\|[A-Za-z]\\)[.)]\\)?[ ]+"
;;                              . 'fixed-pitch)))
;;   (font-lock-add-keywords 'org-mode '(("(\\?)" . 'error)))

;;   ;; Highlight first letter of a paragraph
;;   ;; (font-lock-add-keywords 'org-mode '(("^\\(?:\n\\)\\([[:digit:][:upper:][:lower:]]\\)" . 'org-warning)))
;;   )

(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-themes-treemacs-enable-variable-pitch nil))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(use-package! mixed-pitch
  :hook (org-mode . mixed-pitch-mode)
  :config
  (setq mixed-pitch-face 'variable-pitch))

(defvar mixed-pitch-modes '(org-mode LaTeX-mode markdown-mode gfm-mode Info-mode)
  "Modes that `mixed-pitch-mode' should be enabled in, but only after UI initialisation.")
(defun init-mixed-pitch-h ()
  "Hook `mixed-pitch-mode' into each mode in `mixed-pitch-modes'.
Also immediately enables `mixed-pitch-modes' if currently in one of the modes."
  (when (memq major-mode mixed-pitch-modes)
    (mixed-pitch-mode 1))
  (dolist (hook mixed-pitch-modes)
    (add-hook (intern (concat (symbol-name hook) "-hook")) #'mixed-pitch-mode)))
(add-hook 'doom-init-ui-hook #'init-mixed-pitch-h)

(autoload #'mixed-pitch-serif-mode "mixed-pitch"
  "Change the default face of the current buffer to a serifed variable pitch, while keeping some faces fixed pitch." t)

(setq! variable-pitch-serif-font (font-spec :family "Alegreya" :size 27))

(after! mixed-pitch
  (setq mixed-pitch-set-height t)
  (set-face-attribute 'variable-pitch-serif nil :font variable-pitch-serif-font)
  (defun mixed-pitch-serif-mode (&optional arg)
    "Change the default face of the current buffer to a serifed variable pitch, while keeping some faces fixed pitch."
    (interactive)
    (let ((mixed-pitch-face 'variable-pitch-serif))
      (mixed-pitch-mode (or arg 'toggle)))))

(autoload #'mixed-pitch-serif-mode "mixed-pitch"
  "Change the default face of the current buffer to a serifed variable pitch, while keeping some faces fixed pitch." t)

(setq! variable-pitch-serif-font (font-spec :family "Alegreya" :size 27))

(after! mixed-pitch
  (setq mixed-pitch-set-height t)
  (set-face-attribute 'variable-pitch-serif nil :font variable-pitch-serif-font)
  (defun mixed-pitch-serif-mode (&optional arg)
    "Change the default face of the current buffer to a serifed variable pitch, while keeping some faces fixed pitch."
    (interactive)
    (let ((mixed-pitch-face 'variable-pitch-serif))
      (mixed-pitch-mode (or arg 'toggle)))))

(set-char-table-range composition-function-table ?f '(["\\(?:ff?[fijlt]\\)" 0 font-shape-gstring]))
(set-char-table-range composition-function-table ?T '(["\\(?:Th\\)" 0 font-shape-gstring]))

(defface variable-pitch-serif
  '((t (:family "serif")))
  "A variable-pitch face with serifs."
  :group 'basic-faces)

(defcustom variable-pitch-serif-font (font-spec :family "serif")
  "The font face used for `variable-pitch-serif'."
  :group 'basic-faces
  :set (lambda (symbol value)
         (set-face-attribute 'variable-pitch-serif nil :font value)
         (set-default-toplevel-value symbol value)))

(use-package! doom-themes
  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))
(doom-themes-visual-bell-config)
(map! :leader
      :desc "Load new theme" "h t" #'load-theme)


(set-face-attribute 'mode-line nil :font "Ubuntu Mono-13")

(setq display-time-format "%l:%M %p %b %y"
      display-time-default-load-average nil)

(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange"))

(use-package! uniquify
  :defer 5
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-strip-common-suffix t)
  (setq uniquify-after-kill-buffer-p t))

;;; Diff-mode (and prot-diff.el extensions)
(use-package! diff-mode
  :config
  (setq diff-default-read-only t)
  (setq diff-advance-after-apply-hunk t)
  (setq diff-update-on-the-fly t)
  ;; The following are from Emacs 27.1
  (setq diff-refine nil)                ; I do it on demand
  (setq diff-font-lock-prettify nil)    ; better for patches
  ;; The following is further controlled by
  ;; `prot-diff-modus-themes-diffs'
  (setq diff-font-lock-syntax 'hunk-also))

(after! diff-hl
  (setq global-diff-hl-mode +1)
  (setq diff-hl-show-hunk-function 'diff-hl-show-hunk-inline-popup))

;; (load "~/.config/doom/lisp/prot-diff.el")
;; (use-package! prot-diff
;;   :config
;;   ;; (prot-diff-modus-themes-diffs)
;;   (add-hook 'modus-themes-after-load-theme-hook #'prot-diff-modus-themes-diffs)

;;   (prot-diff-extra-keywords 1)

;;   ;; `prot-diff-buffer-dwim' replaces the default for `vc-diff' (which I
;;   ;; bind to another key---see VC section).
;;   (define-key global-map (kbd "C-x v =") #'prot-diff-buffer-dwim)
;;   (let ((map diff-mode-map))
;;     (define-key map (kbd "C-c C-b") #'prot-diff-refine-cycle) ; replace `diff-refine-hunk'
;;     (define-key map (kbd "C-c C-n") #'prot-diff-narrow-dwim)))
;; Diff-mode (and prot-diff.el extensions):1 ends here

(setq emojify-emoji-set "twemoji-v2")

(defvar emojify-disabled-emojis
  '(;; Org
    "◼" "☑" "☸" "⚙" "⏩" "⏪" "⬆" "⬇" "❓" "↔"
    ;; Terminal powerline
    "✔"
    ;; Box drawing
    "▶" "◀"
    ;; I just want to see this as text
    "©" "™")
  "Characters that should never be affected by `emojify-mode'.")

(defadvice! emojify-delete-from-data ()
  "Ensure `emojify-disabled-emojis' don't appear in `emojify-emojis'."
  :after #'emojify-set-emoji-data
  (dolist (emoji emojify-disabled-emojis)
    (remhash emoji emojify-emojis)))

(defun emojify--replace-text-with-emoji (orig-fn emoji text buffer start end &optional target)
  "Modify `emojify--propertize-text-for-emoji' to replace ascii/github emoticons with unicode emojis, on the fly."
  (if (or (not emoticon-to-emoji) (= 1 (length text)))
      (funcall orig-fn emoji text buffer start end target)
    (delete-region start end)
    (insert (ht-get emoji "unicode"))))

(define-minor-mode emoticon-to-emoji
  "Write ascii/gh emojis, and have them converted to unicode live."
  :global nil
  :init-value nil
  (if emoticon-to-emoji
      (progn
        (setq-local emojify-emoji-styles '(ascii github unicode))
        (advice-add 'emojify--propertize-text-for-emoji :around #'emojify--replace-text-with-emoji)
        (unless emojify-mode
          (emojify-turn-on-emojify-mode)))
    (setq-local emojify-emoji-styles (default-value 'emojify-emoji-styles))
    (advice-remove 'emojify--propertize-text-for-emoji #'emojify--replace-text-with-emoji)))

(add-hook! '(mu4e-compose-mode org-msg-edit-mode circe-channel-mode) (emoticon-to-emoji 1))

(use-package! gif-screencast
  :commands gif-screencast-mode
  :config
  (map! :map gif-screencast-mode-map
        :g "<f8>" #'gif-screencast-toggle-pause
        :g "<f9>" #'gif-screencast-stop)
  (setq gif-screencast-program "maim"
        gif-screencast-args `("--quality" "3" "-i" ,(string-trim-right
                                                     (shell-command-to-string
                                                      "xdotool getactivewindow")))
        gif-screencast-optimize-args '("--batch" "--optimize=3" "--usecolormap=/tmp/doom-color-theme"))
  (defun gif-screencast-write-colormap ()
    (f-write-text
     (replace-regexp-in-string
      "\n+" "\n"
      (mapconcat (lambda (c) (if (listp (cdr c))
                                 (cadr c))) doom-themes--colors "\n"))
     'utf-8
     "/tmp/doom-color-theme" ))
  (gif-screencast-write-colormap)
  (add-hook 'doom-load-theme-hook #'gif-screencast-write-colormap))

(use-package! diff-hl
  :config
  (custom-set-faces!
    `((diff-hl-change)
      :foreground ,(doom-blend (doom-color 'bg) (doom-color 'blue) 0.5))
    `((diff-hl-insert)
      :foreground ,(doom-blend (doom-color 'bg) (doom-color 'green) 0.5)))
  )

;; (use-package! evil-goggles
;;   :init
;;   (setq evil-goggles-enable-change t
;;         evil-goggles-enable-delete t
;;         evil-goggles-pulse         t
;;         evil-goggles-duration      0.25)
;;   :config
;;   (custom-set-faces!
;;     `((evil-goggles-yank-face evil-goggles-surround-face)
;;       :background ,(doom-blend (doom-color 'blue) (doom-color 'bg-alt) 0.5)
;;       :extend t)
;;     `(evil-goggles-paste-face
;;       :background ,(doom-blend (doom-color 'green) (doom-color 'bg-alt) 0.5)
;;       :extend t)
;;     `(evil-goggles-delete-face
;;       :background ,(doom-blend (doom-color 'red) (doom-color 'bg-alt) 0.5)
;;       :extend t)
;;     `(evil-goggles-change-face
;;       :background ,(doom-blend (doom-color 'orange) (doom-color 'bg-alt) 0.5)
;;       :extend t)
;;     `(evil-goggles-commentary-face
;;       :background ,(doom-blend (doom-color 'grey) (doom-color 'bg-alt) 0.5)
;;       :extend t)
;;     `((evil-goggles-indent-face evil-goggles-join-face evil-goggles-shift-face)
;;       :background ,(doom-blend (doom-color 'yellow) (doom-color 'bg-alt) 0.25)
;;       :extend t)
;;     ))

