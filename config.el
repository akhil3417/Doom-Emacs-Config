;;; lisp/config.el -*- lexical-binding: t; -*-
;;; config.el -*- lexical-binding: t; -*-
(setenv "LC_ALL" "C.UTF-8")

;; (setq emms-source-playlist-ask-before-overwrite nil) ;; for favs
;; (load "~/.config/doom/exwmmain.el")
;; (setq package-native-compile t)
;; Personal Information:2 ends here

(load "~/.config/doom/lisp/setup-theme-magic.el")
(load "~/.config/doom/lisp/setup-avy.el")
;; (load "~/.config/doom/lisp/splash.el")
;; (load "~/.config/doom/lisp/setup-minibuffer.el")
(load "~/.config/doom/lisp/better-buffers.el");;essential
;; (load "~/.config/doom/lisp/setup-orderless.el");;configured in doom already but...
;; (load "~/.config/doom/lisp/setup-embark.el")
(load "~/.config/doom/lisp/utilities.el");;essential
(load "~/.config/doom/lisp/setup-isearch")
;; ytel provides an elfeed-like interface to search invidious instances for
;; youtube videos. Phew. The churn rate of Invidious urls is quite high, which
;; makes this flaky, but anything's better than the browser interface to
;; Youtube.
;; (load "~/.config/doom/lisp/setup-ytel.el");; youttuuube
(load "~/.config/doom/lisp/ytdl-downloader.el")
(load "~/.config/doom/lisp/setup-shell.el")
;; (load "~/.config/doom/lisp/exwm-paste.el")
;; (load "~/.config/doom/lisp/lock-screen.el")
(load "~/.config/doom/lisp/correct-previous-word-and-create-abbrev")
;; (load "~/.config/doom/lisp/lookup-on-github")
;; (load "~/.config/doom/lisp/switch-window-patches")
;; (load "~/.config/doom/lisp/pulseaudio.el")
(load "~/.config/doom/lisp/auto-scroll.el")
(load "~/.config/doom/lisp/prot-common.el")
(load "~/.config/doom/lisp/prot-comment.el")
(load "~/.config/doom/lisp/prot-bookmark.el")

(load "~/.config/doom/lisp/setup-avy.el")
;; (load "~/.config/doom/lisp/setup-keychord.el")
(load "~/.config/doom/lisp/setup-webshare.el")

(load "~/.config/doom/lisp/yt-org.el")

(load "~/.config/doom/lisp/ui.el")
(load "~/.config/doom/lisp/general.el")
(load "~/.config/doom/lisp/helper-funcs.el")
(load "~/.config/doom/lisp/coding.el")
(load "~/.config/doom/lisp/editing.el")
(load "~/.config/doom/lisp/binds.el")
(load "~/.config/doom/lisp/setup-tmr.el")
(load "~/.config/doom/lisp/tex.el")
;; launch telegram
;; Launch Telega in workspace 0 if we've logged in before
;; (when (file-exists-p "~/.telega/db.sqlite")
;;   (load "~/.config/doom/lisp/setup-telega.el")
;;   (telega nil)
;;   (setq telega-notifications-mode t))
;; telega:2 ends here
;;


;; (when (featurep! :completion corfu)
;;   (map! :map corfu-map
;;         :desc "insert separator" "C-SPC" #'corfu-insert-separator)
;;   ;; (setq corfu-quit-no-match t)
;;   ;; (setq corfu-on-exact-match nil)
;;   ;; Automatic documentation popup while autocompleting is nice, but let’s reduce
;;   ;; the font size a little bit so that it doesn’t cover the screen too much and
;;   ;; makes it easier to skim for information:
;;   (custom-set-faces! '((corfu-popupinfo) :height 0.9)))

;; [[file:config.org::*Eshell][Eshell:1]]

;;; URLs
(require 'url-util)
(use-package! goto-addr
  :hook ((compilation-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)
         (magit-mode . goto-address-mode)
         (yaml-mode . goto-address-prog-mode)
         (mu4e-view-mode . goto-address-mode))
  :commands (goto-address-prog-mode
             goto-address-mode))
;; url util:1 ends here

;; [[file:config.org::*Wolfram alpha][Wolfram alpha:1]]
;; wolfram alpha queries (M-x wolfram-alpha)
(use-package! wolfram
  :defer t
  :config
  (setq wolfram-alpha-app-id "KTKV36-2LRW2LELV8"))

;; [[file:config.org::*launch firefox][launch firefox:1]]
;; launch firefox:1 ends here

;; [[file:config.org::*ERC][ERC:1]]
(map! :leader
      (:prefix ("e". "evaluate/ERC/EWW")
       :desc "Launch ERC with TLS connection" "E" #'erc-tls))

(setq erc-prompt (lambda () (concat "[" (buffer-name) "]"))
      erc-server "irc.libera.chat"
      erc-nick "akhil3417"
      erc-user-full-name "Akhil Pratap Singh"
      erc-track-shorten-start 24
      erc-autojoin-channels-alist '(("irc.libera.chat" "#unixtube" "#emacs"))
      erc-kill-buffer-on-part t
      erc-fill-column 100
      erc-fill-function 'erc-fill-static
      erc-fill-static-center 20
      ;; erc-auto-query 'bury
      )
;; ERC:1 ends here

(use-package simple-httpd
  :defer t)
;; simple httpd:1 ends here

;; [[file:config.org::*org roam qutebrowser][org roam qutebrowser:1]]
(require 'org-roam-protocol)
(setq org-roam-capture-ref-templates
      '(("r" "ref" plain (function org-roam-capture--get-point)
         "%?"
         :file-name "${slug}"
         :head "#+TITLE: ${title}
#+ROAM_KEY: ${ref}"
         :unnarrowed t)))
;; org roam qutebrowser:1 ends here

;; [[file:config.org::*Mini-buffer editing more space][Mini-buffer editing more space:2]]
(use-package miniedit
  :commands minibuffer-edit
  :init (miniedit-install))
;; Mini-buffer editing more space:2 ends here
;; load org mode
(load "~/.config/doom/lisp/setup-org.el")
(load "~/.config/doom/lisp/setup-org-capture.el")
(load "~/.config/doom/lisp/setup-org-roam.el")
(load "~/.config/doom/lisp/setup-org-agenda.el")
;; [[file:lang.org::*Compilation][Compilation:1]]

;; [[file:lang.org::*MuPDF][MuPDF:2]]
;; (use-package paper
;;   ;; :mode ("\\.pdf\\'"  . paper-mode)
;;   ;; :mode ("\\.epub\\'"  . paper-mode)
;;   :config
;;   (require 'evil-collection-paper)
;;   (evil-collection-paper-setup))
;; MuPDF:2 ends here

;; [[file:lang.org::*Terminal viewing][Terminal viewing:2]]
(use-package! pdftotext
  :init
  (unless (display-graphic-p)
    (add-to-list 'auto-mode-alist '("\\.[pP][dD][fF]\\'" . pdftotext-mode))
    (add-to-list 'magic-mode-alist '("%PDF" . pdftotext-mode)))
  :config
  (unless (display-graphic-p) (after! pdf-tools (pdftotext-install)))
  ;; For prettyness
  (add-hook 'pdftotext-mode-hook #'spell-fu-mode-disable)
  (add-hook 'pdftotext-mode-hook (lambda () (page-break-lines-mode 1)))
  ;; I have no idea why this is needed
  (map! :map pdftotext-mode-map
        "<mouse-4>" (cmd! (scroll-down mouse-wheel-scroll-amount-horizontal))
        "<mouse-5>" (cmd! (scroll-up mouse-wheel-scroll-amount-horizontal))))
;; Terminal viewing:2 ends here

;; [[file:lang.org::*Graph viz][Graph viz:2]]
(use-package! graphviz-dot-mode
  :commands graphviz-dot-mode
  :mode ("\\.dot\\'" . graphviz-dot-mode)
  :init
  (after! org
    (setcdr (assoc "dot" org-src-lang-modes)
            'graphviz-dot)))

(use-package! company-graphviz-dot
  :after graphviz-dot-mode)
;; Graph viz:2 ends here

;; ;; [[file:lang.org::*Org-roam-server][Org-roam-server:1]]
;; (use-package org-roam-server
;;   :config
;;   (setq org-roam-server-host "127.0.0.1"
;;         org-roam-server-port 8080
;;         org-roam-server-authenticate nil
;;         org-roam-server-export-inline-images t
;;         org-roam-server-serve-files nil
;;         org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
;;         org-roam-server-network-poll t
;;         org-roam-server-network-arrows nil
;;         org-roam-server-network-label-truncate t
;;         org-roam-server-network-label-truncate-length 60
;;         org-roam-server-network-label-wrap-length 20)
;;   (defun org-roam-server-open ()
;;     "Ensure the server is active, then open the roam graph."
;;     (interactive)
;;     (org-roam-server-mode 1)
;;     (browse-url-xdg-open (format "http://localhost:%d" org-roam-server-port))))
;; ;; Org-roam-server:1 ends here

;; (use-package super-save)
;; (super-save-mode +1)
;; (setq super-save-auto-save-when-idle t)
;; (add-to-list 'super-save-hook-triggers 'find-file-hook)

;; ;; Super-save/lsp-mode were causing issues while typing. When a function call
;; ;; was typed (e.g. =call-to-func(=), lsp-mode would show the signature in the
;; ;; echo area, this will in turn cause super-save to save the file - as it lost
;; ;; focus - and switch to normal mode via the ~after-save-hook~ configured in the
;; ;; General section.
;; ;;
;; (defun me/super-save-disable-advice (orig-fun &rest args)
;;   "Dont auto-save under these conditions."
;;   (unless (equal (car args) " *LV*")
;; 	(apply orig-fun args)))
;; (advice-add 'super-save-command-advice :around #'me/super-save-disable-advice)

;; (use-package keyfreq
;;   :config
;;     (keyfreq-mode 1)
;;     (keyfreq-autosave-mode 1))

(setq gptel-directives (+gptel-build-directives(concat "~/myrepos/AIPIHKAL/system-prompts/")))
;; (setq gptel-directives (+gptel-build-directives(concat "~/tmpexpdir/aitest/pre/")))
(load-file "~/myrepos/hugging-chat-api-emacs/hugging-chat.el")
(load "~/.config/doom/lisp/html-export.el")
