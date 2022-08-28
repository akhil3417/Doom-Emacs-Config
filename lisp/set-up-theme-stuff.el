;;; lisp/set-up-theme-stuff.el -*- lexical-binding: t; -*-

;; Set basic theme variables
(defvar nyan-enabled-p t)
(defvar modus-summertime-p t)
(setq! doom-theme 'modus-vivendi)

(defun my-set-face-bg-fg-inverse-video (face bg fg)
  (set-face-attribute face nil :inverse-video t)
  (set-face-foreground face bg)
  (set-face-background face fg))

(add-hook 'enable-theme-functions #'my-customize-faces)

(defun my-customize-faces (theme)
  (after! org
    (set-face-foreground 'org-todo (face-foreground 'outline-1 nil t))
    (after! org-modern
      (my-set-face-bg-fg-inverse-video 'org-modern-done
                                       (face-foreground 'org-headline-done nil t)
                                       (alist-get 'background-color (frame-parameters)))
      (set-face-attribute 'org-modern-done nil :weight 'semibold)
      (my-set-face-bg-fg-inverse-video 'org-modern-time-inactive
                                       "gray50"
                                       (if (< (color-distance (alist-get 'background-color (frame-parameters))
                                                              "gray95")
                                              face-near-same-color-threshold)
                                           "gray95"
                                         "gray5"))
      (my-set-face-bg-fg-inverse-video 'org-modern-tag
                                       (face-foreground 'org-level-1 nil t)
                                       (alist-get 'background-color (frame-parameters)))
      (cl-loop for buf being the buffers do
               (when (buffer-local-value 'org-modern-mode buf)
                 (with-current-buffer buf
                   (org-modern--update-label-face)))))
    (set-face-attribute 'org-ellipsis nil
                        :inherit '(shadow default)
                        :weight 'normal)
    (set-face-attribute 'org-document-title nil
                        :height 1.2)))

(when (and (memq doom-theme '(modus-vivendi modus-operandi))
           modus-summertime-p)
  (require 'modus-themes-summertime)
  (modus-themes-summertime +1))

(after! modus-themes
  (let ((modus-themes-inhibit-reload t))
    (setq! modus-themes-italic-constructs t
            modus-themes-slanted-constructs t
            modus-themes-bold-constructs nil
            modus-themes-lang-checkers '(faint)
            modus-themes-org-blocks 'gray-background
            modus-themes-mode-line '(3d accented)
            modus-themes-syntax '(alt-syntax green-strings yellow-comments)
            modus-themes-headings '((1 . (1.25 extrabold rainbow))
                                    (2 . (1.15 semibold rainbow))
                                    (3 . (1.12 semibold rainbow))
                                    (4 . (1.09 medium rainbow))
                                    (5 . (1.06 medium rainbow))
                                    (6 . (1.03 medium rainbow))
                                    (7 . (medium rainbow))
                                    (8 . (medium rainbow))
                                    (t . (rainbow)))
            modus-themes-box-buttons '(accented semibold 0.9 faint all-buttons)
            modus-themes-fringes 'intense
            modus-themes-hl-line '()
            modus-themes-intense-mouseovers '(neutral-underline background)
            modus-themes-markup '(intense italic)
            modus-themes-paren-match '(intense)
            modus-themes-prompts '(intense bold)
            modus-themes-region '(accented bg-only)
            modus-themes-subtle-line-numbers nil
            modus-themes-tabs-accented t
            modus-themes-lang-checkers '(straight-underline intense text-also)
            modus-themes-links '(bold)
            modus-themes-completions nil
            modus-themes-mail-citations 'intense
            modus-themes-deuteranopia nil))
  (setq! modus-themes-inhibit-reload nil))

;; Set theme based on time
(when (display-graphic-p)
  (advice-add #'doom-init-theme-h :override #'ignore)
  (use-package! circadian
    :config
    (cond ((memq doom-theme '(modus-operandi modus-vivendi))
           (setq! circadian-themes '(("7:15" . modus-operandi)
                                      ("19:30" . modus-vivendi))))
          (t (setq! circadian-themes `(("0:00" . ,doom-theme)))))
    (circadian-setup)
    (after! exwm-randr
      (add-hook 'doom-load-theme-hook #'exwm-randr-refresh))))

;; (after! outline
;;   ;; Fontify outline headings like Org mode headings.
;;   (setq! outline-minor-mode-highlight t)
;;   (add-hook 'outline-minor-mode-hook
;;             (defun enable-outline-minor-faces-mode ()
;;               (when (and (derived-mode-p
;;                           'lisp-data-mode 'lisp-mode 'emacs-lisp-mode
;;                           'clojure-mode 'scheme-mode 'python-mode)
;;                          ;; Avoid an error with Org Babel source blocks.
;;                          (not (doom-temp-buffer-p (current-buffer))))
;;                 (outline-minor-faces-mode +1)))))

(provide 'set-up-theme-stuff)
