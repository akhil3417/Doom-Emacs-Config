;; [[file:../config.org::*marganalia][marganalia:1]]
;; -*- lexical-binding: t -*-
;; Prefer richer, more heavy, annotations over the lighter default variant.
;; E.g. M-x will show the documentation string additional to the keybinding. By
;; default only the keybinding is shown as annotation. Note that there is the
;; command `marginalia-cycle-annotators` to switch between the annotators.

(use-package marginalia
  :ensure t
  :after setup-minibuffer
  :init (marginalia-mode 1)
  ;; :bind (:map vertico-map
  ;;        ("M-]" . marginalia-cycle))
  :config
  (add-to-list 'marginalia-prompt-categories '("\\burl\\b" . url))
  (setq marginalia-max-relative-age 0)  ; time is absolute here!
  (setq marginalia-annotators
        '(marginalia-annotators-heavy marginalia-annotators-light)))

(provide 'setup-marginalia)
;; setup-marginalia.el ends here
;; marganalia:1 ends here
