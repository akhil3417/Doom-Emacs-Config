;; -*- lexical-binding: t -*-
;; Vertico
(use-package! vertico
  :hook (doom-first-input . vertico-mode)
  :after minibuffer
  :commands vertico-mode
  :init (vertico-mode 1)
  :bind (:map vertico-map
              ("M-s"     . nil)
              ("M-i"     . vertico-insert)
              ("C-M-n"   . vertico-next-group)
              ("C-M-p"   . vertico-previous-group)
              ("C-j"     . (lambda () (interactive)
	        	     (if minibuffer--require-match
	        	         (minibuffer-complete-and-exit)
	        	       (exit-minibuffer))))
              ("C->"     . embark-become)
              ("C-b"     . embark-become)
              (">"       . embark-become)
              ;; ("<backtab>"   . embark-act-with-completing-read)
              ("C-a"   . embark-act-with-completing-read)
              ("C-S-o"     . embark-minimal-act)
              ("C-M-o"   . embark-minimal-act-noexit)
              ("M-s o"   . embark-export)
              ("C-c C-o" . embark-export))
  :config
  (setq vertico-resize t
        vertico-count 10
        vertico-cycle t
        completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (map! :map vertico-map [backspace] #'vertico-directory-delete-char)
  (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions)
  (advice-add #'ffap-menu-ask :around
              (lambda (&rest args)
                (cl-letf (((symbol-function #'minibuffer-completion-help)
                           #'ignore))
                  (apply args)))))

(define-key vertico-map "\M-V" #'vertico-multiform-vertical)
(define-key vertico-map "\M-G" #'vertico-multiform-grid)
(define-key vertico-map "\M-F" #'vertico-multiform-flat)
(define-key vertico-map "\M-R" #'vertico-multiform-reverse)
(define-key vertico-map "\M-U" #'vertico-multiform-unobtrusive)
(use-package! vertico-multiform
  :commands vertico-multiform-mode
  :after vertico-flat
  :bind (:map vertico-map
              ("M-q" . vertico-multiform-grid)
              ("C-l" . vertico-multiform-reverse)
              ("C-M-l" . embark-export))
  ;; :init (vertico-multiform-mode 1)
  :init (vertico-reverse-mode 1)
  :config
  (setq vertico-multiform-categories
        '((file my/vertico-grid-mode reverse)
          (jinx grid (vertico-grid-annotate . 20))
          (project-file my/vertico-grid-mode reverse)
          (imenu buffer)
          (consult-location buffer)
          (consult-grep buffer)
          (notmuch-result reverse)
          (minor-mode reverse)
          (reftex-label (:not unobtrusive))
          (embark-keybinding grid)
          (citar-reference reverse)
          (xref-location reverse)
          (history reverse)
          (url reverse)
          (consult-info buffer)
          (kill-ring reverse)
          (consult-compile-error reverse)
          (buffer flat (vertico-cycle . t))
          (t flat)))
  (setq vertico-multiform-commands
        '((jinx-correct reverse)
          (tab-bookmark-open reverse)
          (dired-goto-file unobtrusive)
          (load-theme my/vertico-grid-mode reverse)
          (my/toggle-theme my/vertico-grid-mode reverse)
          (org-refile reverse)
          (org-agenda-refile reverse)
          (org-capture-refile reverse)
          (affe-find reverse)
          (execute-extended-command unobtrusive)
          (dired-goto-file flat)
          (consult-project-buffer flat)
          (consult-dir-maybe reverse)
          (consult-dir reverse)
          (consult-flymake reverse)
          (consult-history reverse)
          (consult-completion-in-region reverse)
          (consult-recoll buffer)
          (citar-insert-citation reverse)
          (completion-at-point reverse)
          (org-roam-node-find reverse)
          ;; (embark-completing-read-prompter reverse)
          ;; (embark-act-with-completing-read reverse)
          ;; (embark-bindings reverse)
          (consult-org-heading reverse)
          (consult-dff unobtrusive)
          (embark-find-definition reverse)
          (xref-find-definitions reverse)
          (my/eshell-previous-matching-input reverse)
          (tmm-menubar reverse)))

  (defun vertico-multiform-unobtrusive ()
    "Toggle the quiet display."
    (interactive)
    (vertico-multiform--define-display-toggle 'vertico-unobtrusive-mode)
    (if vertico-unobtrusive-mode
        (vertico-multiform--temporary-mode 'vertico-reverse-mode -1)
      (vertico-multiform--temporary-mode 'vertico-reverse-mode 1))))

(use-package! vertico-unobtrusive
  :after vertico-flat)

(use-package! vertico-grid
  :after vertico
  ;; :bind (:map vertico-map ("M-q" . vertico-grid-mode))
  :config
  (defvar my/vertico-count-orig vertico-count)
  (define-minor-mode my/vertico-grid-mode
    "Vertico-grid display with modified row count."
    :global t :group 'vertico
    (cond
     (my/vertico-grid-mode
      (setq my/vertico-count-orig vertico-count)
      (setq vertico-count 4)
      (vertico-grid-mode 1))
     (t (vertico-grid-mode 0)
        (setq vertico-count my/vertico-count-orig))))
  (setq vertico-grid-separator "    ")
  (setq vertico-grid-lookahead 50))

(use-package! vertico-quick
  :after vertico
  :bind (:map vertico-map
              ("M-i" . vertico-quick-insert)
              ("C-'" . vertico-quick-exit)
              ("C-o" . vertico-quick-embark))
  :config
  (defun vertico-quick-embark (&optional arg)
    "Embark on candidate using quick keys."
    (interactive)
    (when (vertico-quick-jump)
      (embark-act arg))))

(use-package! vertico-directory
  ;; :hook (rfn-eshadow-update-overlay vertico-directory-tidy)
  :after vertico
  :bind (:map vertico-map
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)
              ("C-w"   . vertico-directory-delete-word)
              ("RET"   . vertico-directory-enter)))

(use-package! vertico-repeat
  :after vertico
  :bind (("C-x ." . vertico-repeat)))   ;bound to SPC-' in doom emacs

(use-package! vertico-reverse
  ;; :disabled
  :after vertico)

(use-package! vertico-flat
  ;; :bind (:map vertico-map
  ;;             ("M-q" . vertico-flat-mode))
  :after vertico)

(use-package! vertico-buffer
  :after vertico
  ;; :hook (vertico-buffer-mode . vertico-buffer-setup)
  :config
  (setq vertico-buffer-display-action 'display-buffer-reuse-window))

;; ;; vertico-posframe config
;; (require 'vertico-posframe)
;; (vertico-posframe-mode 1)
;; (setq vertico-multiform-commands
;;       '((consult-line
;;          posframe
;;          (vertico-posframe-poshandler . posframe-poshandler-frame-top-center)
;;          (vertico-posframe-border-width . 10)
;;          ;; NOTE: This is useful when emacs is used in both in X and
;;          ;; terminal, for posframe do not work well in terminal, so
;;          ;; vertico-buffer-mode will be used as fallback at the
;;          ;; moment.
;;          (vertico-posframe-fallback-mode . vertico-buffer-mode))
;;         (t posframe)))
;; (vertico-multiform-mode 1)
;; (setq vertico-multiform-commands
;;       '((consult-line (:not posframe))
;;         ;; (consult-dir (:not posframe))
;;         ;; (find-file (:not posframe))
;;         (execute-extended-command (:not posframe))
;;         (t posframe)))
;; (setq vertico-posframe-parameters
;;       '((left-fringe . 15)
;;         (right-fringe . 1)))


(provide 'setup-vertico)
;; setup-vertico.el ends here
;; vertico:1 ends here
