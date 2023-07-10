(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method 'aggressive)
 '(custom-safe-themes
   '("cf922a7a5c514fad79c483048257c5d8f242b21987af0db813d3f0b138dfaf53" "4f1d2476c290eaa5d9ab9d13b60f2c0f1c8fa7703596fa91b235db7f99a9441b" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "e87f48ec4aebdca07bb865b90088eb28ae4b286ee8473aadb39213d361d0c45f" "046e442b73846ae114d575a51be9edb081a1ef29c05ae5e237d5769ecfd70c2e" "7397cc72938446348521d8061d3f2e288165f65a2dbb6366bb666224de2629bb" "e2c926ced58e48afc87f4415af9b7f7b58e62ec792659fcb626e8cba674d2065" "9f1d0627e756e58e0263fe3f00b16d8f7b2aca0882faacdc20ddd56a95acb7c2" default))
 '(elfeed-feeds
   '("https://www.youtube.com/feeds/videos.xml?channel_id=UCCC4-ZHzMHUKNyDENY7Pk6Q" "https://www.youtube.com/feeds/videos.xml?channel_id=UCwuyodzTl_KdEKNuJmeo99A" "https://www.youtube.com/feeds/videos.xml?channel_id=UCngn7SVujlvskHRvRKc1cTw" "https://www.youtube.com/feeds/videos.xml?channel_id=UCmFeOdJI3IXgTBDzqBLD8qg" "https://www.youtube.com/feeds/videos.xml?channel_id=UCj1VqrHhDte54oLgPG4xpuQ" "https://www.youtube.com/feeds/videos.xml?channel_id=UCX6OQ3DkcsbYNE6H8uQQuVA" "https://www.youtube.com/feeds/videos.xml?channel_id=UCVls1GmFKf6WlTraIb_IaJg" "https://www.youtube.com/feeds/videos.xml?channel_id=UCqZQJ4600a9wIfMPbYc60OQ" "https://www.youtube.com/feeds/videos.xml?channel_id=UCUMZ7gohGI9HcU9VNsr2FJQ" "https://www.youtube.com/feeds/videos.xml?channel_id=UCR1IuLEqb6UEA_zQ81kwXfg" "https://www.youtube.com/feeds/videos.xml?channel_id=UC9-y-6csu5WGm29I7JiwpnA" "https://www.youtube.com/feeds/videos.xml?channel_id=UCZg4PvX48mgXQVySgIulX-Q" "https://www.youtube.com/feeds/videos.xml?channel_id=UCbsfyGlrjrKQC0gbzK0-EiA" "https://www.youtube.com/feeds/videos.xml?channel_id=UC7YOGHUfC1Tb6E4pudI9STA" "https://www.youtube.com/feeds/videos.xml?channel_id=UCkf4VIqu3Acnfzuk3kRIFwA" "https://www.youtube.com/feeds/videos.xml?channel_id=UCjr2bPAyPV7t35MvcgT3W8Q" "https://www.youtube.com/feeds/videos.xml?channel_id=UCfH_Hz868iwzA7g36NyBX1A"))
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
 '(org-agenda-files '("~/org/org-agenda/" "~/org/org-capture/todo.org"))
 '(package-selected-packages
   '(ytdl exwm-mff epc mentor pacmacs org-noter-pdftools visual-fill-column calfw elfeed-tube-mpv dash miniedit pdf-tools all-the-icons-completion all-the-icons-dired recursion-indicator edwina trashed rcirc-notify emojify all-the-icons olivetti gameoflife cape exwm))
 '(rmh-elfeed-org-files '("~/.config/doom/elfeed.org") t)
 '(warning-suppress-log-types '((emacs) (org-element-cache) (comp) (defvaralias)))
 '(warning-suppress-types '((org-element-cache) (comp) (defvaralias))))
;; (setq telega-tdlib-max-version "1.8.5")
;; (setq telega-server-libs-prefix "/home/shiva/gitclones/td/tdlib/")
(setq telega-server-libs-prefix "/usr/local/")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-buffer-modified ((t (:foreground "orange"))))
 '(font-lock-comment-face ((t (:slant italic))))
 '(font-lock-keyword-face ((t (:slant italic))))
 '(markdown-header-face-1 ((t (:height 1.25 :weight extra-bold :inherit markdown-header-face))))
 '(markdown-header-face-2 ((t (:height 1.15 :weight bold :inherit markdown-header-face))))
 '(markdown-header-face-3 ((t (:height 1.08 :weight bold :inherit markdown-header-face))))
 '(markdown-header-face-4 ((t (:height 1.0 :weight bold :inherit markdown-header-face))))
 '(markdown-header-face-5 ((t (:height 0.9 :weight bold :inherit markdown-header-face))))
 '(markdown-header-face-6 ((t (:height 0.75 :weight extra-bold :inherit markdown-header-face))))
 '(mode-line ((t (:height 1.0))))
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
 '(org-modern-statistics ((t (:inherit org-checkbox-statistics-todo))))
 '(org-special-keyword (nil :inherit '(font-lock-comment-face fixed-pitch)))
 '(org-table (nil :inherit '(shadow fixed-pitch)))
 '(org-verbatim (nil :inherit '(shadow fixed-pitch)))
 '(ts-fold-replacement-face ((t (:foreground nil :box nil :inherit font-lock-comment-face :weight light)))))

(fset 'test\
   (kmacro-lambda-form [?i ?- ?  ?  escape ?  ?n ?r ?i ?# ?a ?i up up return ?o ?\C-? ?- ?  backspace ?\C-? escape] 0 "%d"))

(fset 'background\ commit
   (kmacro-lambda-form [?V escape ?v ?$ ?y ?s ?\C-w ?\C-w ?a ?g ?i ?t ?  ?c ?o ?m ?m ?i ?t ?  ?- ?m ?  ?\" ?a ?d ?d ?  escape ?i ?  escape ?h ?p ?b ?b ?b ?h ?x ?A return escape ?\C-w ?m ?a ?g ?i ?t ?  ?r ?e return] 0 "%d"))

(fset 'title\ to\ roam
   (kmacro-lambda-form [?0 ?w ?v ?\C-s ?. return ?h ?h ?y ?  ?n ?r ?f ?\C-v ?\C-j ?n ?c ?o ?m ?m ?a ?n ?d ?s return ?o escape ?o escape ?\C-a return ?\C-s ?* return ?V ?G ?G ?y C-escape ?2 return ?p ?  ?f ?s ?k ?k ?k ?A backspace backspace backspace backspace backspace backspace backspace backspace backspace escape ?  ?f ?s] 0 "%d"))

(fset 'ytplaylist\ to\ org-links
   (kmacro-lambda-form [?\C-s ?u ?r ?l ?\C-s escape ?l ?l ?y ?i ?q ?\C-w ?i ?h ?t ?t ?p ?s ?: ?/ ?/ ?w ?w ?w ?. ?y ?o ?u ?t ?u ?b ?e ?. ?c ?o ?m ?/ ?w ?a ?t ?c ?h ?? ?v ?= escape ?p ?0 ?i ?\[ ?\[ escape ?A ?\] ?\[ escape ?h ?\C-w ?\C-s ?t ?i ?t escape ?l ?l ?l ?l ?y ?i ?q ?\C-w ?p ?a ?\] escape ?o ?\C-w ?\C-s ?\{] 0 "%d"))
(put 'downcase-region 'disabled nil)

(fset 'org\ emphasis\ highlight
   (kmacro-lambda-form [?\C-c ?\C-x ?\C-f ?=] 0 "%d"))

(fset 'add\ horizontal\ break\ upwards\ table
   (kmacro-lambda-form [?  ?u ?  ?m ?b ?-] 0 "%d"))

(fset 'add\ keybind\ and\ desc\ on\ top\ of\ table
   (kmacro-lambda-form [?O ?a ?d ?d backspace backspace backspace ?K ?e ?y ?b ?i ?n ?d ?s tab ?B ?i ?n ?d ?i ?n ?g ?  ?P ?u ?r ?p ?o ?s ?e escape ?  ?u ?  ?m ?b ?-] 0 "%d"))
