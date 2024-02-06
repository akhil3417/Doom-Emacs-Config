;;; lisp/new/coding.el -*- lexical-binding: t; -*-

(setq-default tab-width 4)
;; magit
(after! magit
    (setq git-commit-summary-max-length 100))

;; This function first checks if there are any modified files using =magit-anything-modified-p=. If there are no modified files, it displays a message. Otherwise, it stages the modified files using =magit-stage-modified=, prompts you for a commit message, creates a commit using =magit-commit-create=, and finally pushes the changes to the "origin" remote using =magit-push-current-to-upstream=.
(defun +magit-stage-commit-push ()
  "Stage, commit, and push all modified files to 'origin'."
  (interactive)
  (if (not (magit-anything-modified-p))
      (message "No modified files to commit.")
    (magit-stage-modified)
    (let* ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S"))
      (commit-msg (concat "Update notes " timestamp)))
      (magit-commit-create (list "-m" commit-msg))
      (magit-push-current-to-upstream "p"))))
;; (map! :leader :desc "Magit quick push" "g p" #+magit-stage-commit-push)

(setq eros-eval-result-prefix "⟹ ") ; default =>

;; (after! treemacs
;;   (defvar treemacs-file-ignore-extensions '()
;;     "File extension which `treemacs-ignore-filter' will ensure are ignored")
;;   (defvar treemacs-file-ignore-globs '()
;;     "Globs which will are transformed to `treemacs-file-ignore-regexps' which `treemacs-ignore-filter' will ensure are ignored")
;;   (defvar treemacs-file-ignore-regexps '()
;;     "RegExps to be tested to ignore files, generated from `treeemacs-file-ignore-globs'")
;;   (defun treemacs-file-ignore-generate-regexps ()
;;     "Generate `treemacs-file-ignore-regexps' from `treemacs-file-ignore-globs'"
;;     (setq treemacs-file-ignore-regexps (mapcar 'dired-glob-regexp treemacs-file-ignore-globs)))
;;   (if (equal treemacs-file-ignore-globs '()) nil (treemacs-file-ignore-generate-regexps))
;;   (defun treemacs-ignore-filter (file full-path)
;;     "Ignore files specified by `treemacs-file-ignore-extensions', and `treemacs-file-ignore-regexps'"
;;     (or (member (file-name-extension file) treemacs-file-ignore-extensions)
;;         (let ((ignore-file nil))
;;           (dolist (regexp treemacs-file-ignore-regexps ignore-file)
;;             (setq ignore-file (or ignore-file (if (string-match-p regexp full-path) t nil)))))))
;;   (add-to-list 'treemacs-ignored-file-predicates #'treemacs-ignore-filter))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-magit
  :defer t
  :after (treemacs magit))

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :config (treemacs-set-scope-type 'Perspectives))



;; (setq treemacs-file-ignore-extensions
;;       '(;; LaTeX
;;         "aux"
;;         "ptc"
;;         "fdb_latexmk"
;;         "fls"
;;         "synctex.gz"
;;         "toc"
;;         ;; LaTeX - glossary
;;         "glg"
;;         "glo"
;;         "gls"
;;         "glsdefs"
;;         "ist"
;;         "acn"
;;         "acr"
;;         "alg"
;;         ;; LaTeX - pgfplots
;;         "mw"
;;         ;; LaTeX - pdfx
;;         "pdfa.xmpi"
;;         ))
;; (setq treemacs-file-ignore-globs
;;       '(;; LaTeX
;;         "*/_minted-*"
;;         ;; AucTeX
;;         "*/.auctex-auto"
;;         "*/_region_.log"
;;         "*/_region_.tex"))

;;; Configure 'electric' behaviour
(use-package! electric
  :config
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  (setq electric-pair-preserve-balance t)
  (setq electric-pair-pairs
        '((8216 . 8217)
          (8220 . 8221)
          (171 . 187)))
  (setq electric-pair-skip-self 'electric-pair-default-skip-self)
  (setq electric-pair-skip-whitespace nil)
  (setq electric-pair-skip-whitespace-chars '(9 10 32))
  (setq electric-quote-context-sensitive t)
  (setq electric-quote-paragraph t)
  (setq electric-quote-string nil)
  (setq electric-quote-replace-double t)
  (electric-pair-mode -1)
  (electric-quote-mode -1)
  ;; I don't like auto indents in Org and related.  They are okay for
  ;; programming.
  (electric-indent-mode -1)
  (add-hook 'prog-mode-hook #'electric-indent-local-mode))

(use-package! paren
  :ensure nil
  :config
  (setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-in-periphery nil)
  (setq show-paren-when-point-inside-paren nil)
  (setq show-paren-context-when-offscreen 'child-frame) ; Emacs 29
  (add-hook 'after-init-hook #'show-paren-mode))


(map! :leader
      (:prefix ("e". "evaluate/EWW")
       :desc "Evaluate elisp in buffer" "b" #'eval-buffer
       :desc "Evaluate defun" "d" #'eval-defun
       :desc "Evaluate elisp expression" "e" #'eval-expression
       :desc "Evaluate last sexpression" "l" #'eval-last-sexp
       :desc "Evaluate elisp in region" "r" #'eval-region))

(use-package! visual-regexp
  :config
  (map! :map 'doom-leader-regular-map
        (:prefix ("v" . "visual regex")
         :desc "Replace regexp" "r"#'vr/replace)))

(use-package! visual-regexp-steroids
  :after 'visual-regexp)

(use-package! devdocs
  :after lsp
  :config
  (add-hook! 'devdocs-mode-hook
    (face-remap-add-relative 'variable-pitch '(:family "Noto Sans"))))
(global-set-key (kbd "C-h D") 'devdocs-lookup)

(defadvice! +lookup/dictionary-definition-lexic (identifier &optional arg)
  "Look up the definition of the word at point (or selection) using `lexic-search'."
  :override #'+lookup/dictionary-definition
  (interactive
   (list (or (doom-thing-at-point-or-region 'word)
             (read-string "Look up in dictionary: "))
         current-prefix-arg))
  (lexic-search identifier nil nil t))

;; (add-hook! 'after-init-hook
;;            (progn
;;   (setq-hook! 'typescript-mode-hook +format-with :nil)
;;   (add-hook! 'typescript-mode-hook 'prettier-mode)
;;   (setq-hook! 'rjsx-mode-hook +format-with :nil)
;;   (add-hook! 'rjsx-mode-hook 'prettier-mode)
;;   (setq-hook! 'js2-mode-hook +format-with :nil)
;;   (add-hook! 'js2-mode-hook 'prettier-mode)
;;   (setq-hook! 'typescript-tsx-mode-hook +format-with :nil)
;;   (add-hook! 'typescript-tsx-mode-hook 'prettier-mode)
;;   ))

(use-package! lsp-ui
  :config
  (setq lsp-ui-doc-delay 2
        lsp-ui-doc-max-width 80)
  (setq lsp-signature-function 'lsp-signature-posframe))

(setq lsp-idle-delay 0.0
      company-idle-delay 0.0
      company-tooltip-idle-delay 0.0
      gc-cons-threshold (* 100 1024 1024)
      company-minimum-prefix-length 0
      company-tooltip-limit 10
      company-tooltip-minimum-width 100
      company-tooltip-maximum-width 200
      lsp-ui-doc-max-width 150
      lsp-ui-doc-max-height 100
      lsp-ui-doc-include-signature nil
      lsp-ui-doc-enable nil
      lsp-signature-render-documentation nil
      lsp-signature-auto-activate nil
      lsp-enable-snippet nil
      company-lsp-enable-snippet nil
      lsp-enable-symbol-highlighting nil
      lsp-enable-links nil
      lsp-restart nil ;; 'auto-restart
      lsp-intelephense-completion-trigger-parameter-hints nil
      lsp-signature-function 'lsp-signature-posframe)
;; (company-box-mode -1)

(after! company
  (map! :map company-active-map "<tab>" #'company-complete-selection)
  (map! "M-[" #'+company/complete)
  (map! :map lsp-mode-map "<tab>" #'company-indent-or-complete-common))

;; [[file:lang.org::*LAAS][LAAS:3]]
(use-package! laas
  :hook (LaTeX-mode . laas-mode)
  :config
  (aas-set-snippets 'laas-mode
                    ;; set condition!
                    :cond #'texmathp ; expand only while in math
                    "supp" "\\supp"
                    "On" "O(n)"
                    "O1" "O(1)"
                    "Olog" "O(\\log n)"
                    "Olon" "O(n \\log n)"
                    ;; bind to functions!
                    "Sum" (lambda () (interactive)
                            (yas-expand-snippet "\\sum_{$1}^{$2} $0"))
                    "Span" (lambda () (interactive)
                             (yas-expand-snippet "\\Span($1)$0"))
                    ;; add accent snippets
                    :cond #'laas-object-on-left-condition
                    "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt")))
  (defun laas-tex-fold-maybe ()
    (unless (equal "/" aas-transient-snippet-key)
      (+latex-fold-last-macro-a)))
  (add-hook 'aas-post-snippet-expand-hook #'laas-tex-fold-maybe))
;; LAAS:3 ends here

;; [[file:lang.org::*SyncTeX][SyncTeX:1]]
(after! tex
  (add-to-list 'TeX-view-program-list '("Evince" "evince %o"))
  (add-to-list 'TeX-view-program-selection '(output-pdf "Evince")))
;; SyncTeX:1 ends here

;; [[file:lang.org::*Fixes][Fixes:1]]
(when EMACS28+
  (add-hook 'latex-mode-hook #'TeX-latex-mode))
;; Fixes:1 ends here
;; [[file:lang.org::*Python][Python:1]]
;; (setq lsp-python-ms-executable "~/gitclones/emacsrepos/mspyls/python-language-server/output/bin/Release/linux-x64/publish/Microsoft.Python.LanguageServer")

;; (after! lsp-python-ms
;;   (set-lsp-priority! 'mspyls 1))

(use-package! python
  :config
  (set-popup-rules!
    '(("^\\*Python:*\\*$" :side right :size 0.5 :ttl nil))))
(add-hook 'python-mode-hook
          (lambda () (setq-local devdocs-current-docs '("python~3.12"))))

(setq dap-python-debugger 'debugpy)
(setq python-prettify-symbols-alist 'nil) ;defaults are bad , may customise later

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

;; (after! lsp-mode
;;   (set-lsp-priority! 'clangd 1))  ; ccls has priority 0
;; (setq lsp-enable-file-watchers nil)
;; :hook (lsp-mode . efs/lsp-mode-setup)
;; Python:1 ends here

;; [[file:lang.org::*Markdown][Markdown:1]]
(add-hook! (gfm-mode markdown-mode) #'visual-line-mode #'turn-off-auto-fill)
;; Markdown:1 ends here

;; [[file:lang.org::*Markdown][Markdown:2]]
(add-to-list 'auto-mode-alist '("\\.mdx\\'" . markdown-mode))

(custom-set-faces!
  '(markdown-header-face-1 :height 1.25 :weight extra-bold :inherit markdown-header-face)
  '(markdown-header-face-2 :height 1.15 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-3 :height 1.08 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-4 :height 1.00 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-5 :height 0.90 :weight bold       :inherit markdown-header-face)
  '(markdown-header-face-6 :height 0.75 :weight extra-bold :inherit markdown-header-face))
;; Markdown:2 ends here

;; [[file:lang.org::*Julia][Julia:1]]
(add-hook 'julia-mode-hook #'rainbow-delimiters-mode-enable)
(add-hook! 'julia-mode-hook
  (setq-local lsp-enable-folding t
              lsp-folding-range-limit 100))
;; Julia:1 ends here

;; (use-package lsp-treemacs :defer t)

;; (setq lsp-headerline-breadcrumb-enable t
;;       lsp-headerline-breadcrumb-segments '(symbols)
;;       lsp-headerline-breadcrumb-icons-enable t
;;       lsp-headerline-breadcrumb-enable-diagnostics nil)

;; (map! "M-G" 'lsp-ui-peek-find-references)
;; (map! "M-M" 'consult-lsp-symbols)
;; (map! "M-[" '+company/complete)

;; (defun me/consult-imenu-maybe-lsp ()
;;   (interactive)
;;   (if (bound-and-true-p lsp-mode)
;;       (consult-lsp-file-symbols t)
;;     (consult-imenu)))

;; (map! "M-m" 'me/consult-imenu-maybe-lsp)
;; (map! "C-f" 'lsp-format-region)

;; ;; lsp doc show
;; (map! "M-h" 'lsp-ui-doc-show)

;; ;; lsp signature show
;; (map! "M-H" 'lsp-signature-toggle-full-docs)


(use-package! yasnippet
  :config
  ;; It will test whether it can expand, if yes, change cursor color
  (defun hp/change-cursor-color-if-yasnippet-can-fire (&optional field)
    (interactive)
    (setq yas--condition-cache-timestamp (current-time))
    (let (templates-and-pos)
      (unless (and yas-expand-only-for-last-commands
                   (not (member last-command yas-expand-only-for-last-commands)))
        (setq templates-and-pos (if field
                                    (save-restriction
                                      (narrow-to-region (yas--field-start field)
                                                        (yas--field-end field))
                                      (yas--templates-for-key-at-point))
                                  (yas--templates-for-key-at-point))))
      (set-cursor-color (if (and templates-and-pos (first templates-and-pos)
                                 (eq evil-state 'insert))
                            (doom-color 'red)
                          (face-attribute 'default :foreground)))))
  :hook (post-command . hp/change-cursor-color-if-yasnippet-can-fire))

;; [[file:lang.org::*Editor Visuals][Editor Visuals:1]]
(after! ess-r-mode
  (appendq! +ligatures-extra-symbols
            '(:assign "⟵"
              :multiply "×"))
  (set-ligatures! 'ess-r-mode
    ;; Functional
    :def "function"
    ;; Types
    :null "NULL"
    :true "TRUE"
    :false "FALSE"
    :int "int"
    :floar "float"
    :bool "bool"
    ;; Flow
    :not "!"
    :and "&&" :or "||"
    :for "for"
    :in "%in%"
    :return "return"
    ;; Other
    :assign "<-"
    :multiply "%*%"))
;; Editor Visuals:1 ends here
