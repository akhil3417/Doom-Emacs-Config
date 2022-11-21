(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method 'aggressive)
 '(custom-safe-themes
   '("cf922a7a5c514fad79c483048257c5d8f242b21987af0db813d3f0b138dfaf53" "4f1d2476c290eaa5d9ab9d13b60f2c0f1c8fa7703596fa91b235db7f99a9441b" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "e87f48ec4aebdca07bb865b90088eb28ae4b286ee8473aadb39213d361d0c45f" "046e442b73846ae114d575a51be9edb081a1ef29c05ae5e237d5769ecfd70c2e" "7397cc72938446348521d8061d3f2e288165f65a2dbb6366bb666224de2629bb" "e2c926ced58e48afc87f4415af9b7f7b58e62ec792659fcb626e8cba674d2065" "9f1d0627e756e58e0263fe3f00b16d8f7b2aca0882faacdc20ddd56a95acb7c2" default))
 '(elfeed-feeds
   '("https://www.youtube.com/feeds/videos.xml?channel_id=UCq-Fj5jknLsUf-MWSy4_brA"))
 '(hacker-typer-remove-comments t)
 '(hacker-typer-type-rate 1)
 '(ignored-local-variable-values
   '((eval progn
      (setq-local dired-omit-files "\\`[.]?#\\|\\`[.][.]?\\'"))
     (eval progn
      (when
          (and
           (derived-mode-p 'emacs-lisp-mode)
           (fboundp '+compile-this-elisp-file))
        (add-hook 'after-save-hook #'+compile-this-elisp-file nil t))
      (when
          (require 'git-auto-commit-mode nil t)
        (git-auto-commit-mode 1)
        (setq-local gac-automatically-push-p t)))))
 '(org-agenda-files
   '("~/org/org-agenda/daily" "~/org/org-agenda/incubate.org" "~/org/org-agenda/openquestions.org" "~/org/org-agenda/todo.org" "~/org/org-agenda/agenda.org" "~/org/org-capture/todo.org" "~/org/org-capture/webnotes.org" "~/org/org-roam2/daily/" "~/org/org-roam2/todo/todo.org"))
 '(package-selected-packages
   '(mentor pacmacs w3m org-noter-pdftools visual-fill-column calfw elfeed-tube-mpv elfeed-tube dash miniedit pdf-tools all-the-icons-completion all-the-icons-dired recursion-indicator edwina trashed rcirc-notify emojify all-the-icons olivetti gameoflife cape exwm))
 '(rmh-elfeed-org-files '("~/.config/doom/elfeed.org"))
 '(warning-suppress-types '((org-element-cache) (comp) (defvaralias))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-buffer-modified ((t (:foreground "orange"))))
 '(font-lock-comment-face ((t (:slant italic))))
 '(font-lock-keyword-face ((t (:slant italic))))
 '(keycast-command ((t (:inherit doom-modeline-debug :height 0.9))))
 '(keycast-key ((t (:inherit custom-modified :height 1.1 :weight bold))))
 '(markdown-header-face-1 ((t (:height 1.25 :weight extra-bold :inherit markdown-header-face))))
 '(markdown-header-face-2 ((t (:height 1.15 :weight bold :inherit markdown-header-face))))
 '(markdown-header-face-3 ((t (:height 1.08 :weight bold :inherit markdown-header-face))))
 '(markdown-header-face-4 ((t (:height 1.0 :weight bold :inherit markdown-header-face))))
 '(markdown-header-face-5 ((t (:height 0.9 :weight bold :inherit markdown-header-face))))
 '(markdown-header-face-6 ((t (:height 0.75 :weight extra-bold :inherit markdown-header-face))))
 '(mode-line ((t (:height 0.9))))
 '(mode-line-inactive ((t (:height 0.9))))
 '(org-block (nil :foreground nil :inherit 'fixed-pitch))
 '(org-checkbox (nil :inherit 'fixed-pitch))
 '(org-code (nil :inherit '(shadow fixed-pitch)))
 '(org-document-title ((t (:height 1.2))))
 '(org-level-1 ((t (:inherit outline-1 :weight extra-bold :height 1.35))))
 '(org-level-2 ((t (:inherit outline-2 :weight bold :height 1.25))))
 '(org-level-3 ((t (:inherit outline-3 :weight bold :height 1.22))))
 '(org-level-4 ((t (:inherit outline-4 :weight bold :height 1.19))))
 '(org-level-5 ((t (:inherit outline-5 :weight semi-bold :height 1.16))))
 '(org-level-6 ((t (:inherit outline-6 :weight semi-bold :height 1.13))))
 '(org-level-7 ((t (:inherit outline-7 :weight semi-bold))))
 '(org-level-8 ((t (:inherit outline-8 :weight semi-bold))))
 '(org-meta-line (nil :inherit '(font-lock-comment-face fixed-pitch)))
 '(org-special-keyword (nil :inherit '(font-lock-comment-face fixed-pitch)))
 '(org-table (nil :inherit '(shadow fixed-pitch)))
 '(org-verbatim (nil :inherit '(shadow fixed-pitch))))
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'customize-group 'disabled nil)
;; (setq telega-tdlib-max-version "1.8.5")
;; (setq telega-server-libs-prefix "/home/shiva/gitclones/td/tdlib/")
(setq telega-server-libs-prefix "/usr/local/")


(fset 'background\ commit
   (kmacro-lambda-form [?V escape ?v ?$ ?y ?s ?\C-w ?\C-w ?a ?g ?i ?t ?  ?c ?o ?m ?m ?i ?t ?  ?- ?m ?  ?\" ?a ?d ?d ?  escape ?i ?  escape ?h ?p ?b ?b ?b ?h ?x ?A return escape ?\C-w 67108927 ?m ?a ?g ?i ?t ?  ?r ?e return] 0 "%d"))
