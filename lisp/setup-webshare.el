;;; lisp/setup-webshare.el -*- lexical-binding: t; -*-


(use-package! webpaste
  ;; :ensure t
  :bind (("C-c C-p C-b" . webpaste-paste-buffer)
         ("C-c C-p C-r" . webpaste-paste-region)
         ("C-c C-p C-p" . webpaste-paste-buffer-or-region))
  :config
  (progn
    ;; (setq webpaste-provider-priority '("ix.io" "dpaste.org"))))
;; Choosing githup gist only
(setq webpaste-provider-priority '("gist.github.com"))

;; Choosing ix.io as first provider and dpaste.org as second
(setq webpaste-provider-priority '("ix.io" "dpaste.org"))

;; Choosing 1) ix.io, 2) dpaste.org, 3) dpaste.com
(setq webpaste-provider-priority '("ix.io" "dpaste.org" "dpaste.com"))))

;; You can always append this list as much as you like, and which providers
;; that exists is documented below in the readme.
;; Require confirmation before doing paste
(setq webpaste-paste-confirmation t)
;; Do maximum 13 retries instead of standard 10
(setq webpaste-max-retries 13)
(setq webpaste-add-to-killring nil);; Simple hook to just message the URL, this is more or less the default
;; already. But if you disable the default and still want a message, this
;; would work fine.
(add-hook 'webpaste-return-url-hook 'message)

;; To build your own send-to-browser hook, you could do like this:
(add-hook 'webpaste-return-url-hook
          (lambda (url)
            (message "Opened URL in browser: %S" url)
            (browse-url-generic url)))

;; Simple hook to replicate the `webpaste-copy-to-clipboard' option
(add-hook 'webpaste-return-url-hook 'simpleclip-set-contents)
;; Webpaste:2 ends here

;; [[file:config.org::*0x0][0x0:1]]
(use-package! 0x0
  :ensure
  :commands (0x0-upload 0x0-dwim)
  :bind ("C-x U" . 0x0-dwim))
;; 0x0:1 ends here
