;;; lisp/setup-eww.el -*- lexical-binding: t; -*-


;; To open .onion links from eww, run ‘torsocks emacs’ from the commandline. From then, you should be able to load .onion addresses from within ERC and eww.
;; (map! :leader
;;       :desc "Search web for text between BEG/END"
;;       "s w" #'eww-search-words
;;       (:prefix ("e" . "evaluate/EWW")
;;        :desc "Eww web browser" "w" #'eww
;;        :desc "Eww reload page" "R" #'eww-reload))


;;; Simple HTML Renderer (shr), Emacs Web Wowser (eww), Elpher, and prot-eww.el
(use-package! browse-url
  :defer t
  :config
  (setq browse-url-browser-function 'eww-browse-url)
  (setq engine/browser-function 'eww-browse-url);;  browse-url-default-browser open firefox
  ;; (setq engine/browser-function 'browse-url-default-browser);;  browse-url-default-browser open firefox
  ;; (setq browse-url-secondary-browser-function 'browse-url-default-browser))

 ;; (setq browse-url-browser-function 'w3m-browse-url)
;;Open a url embedded in any buffer
 ;; (setq browse-url-browser-function  'w3m-goto-url-new-session)
 ;; (setq browse-url-browser-function  'w3m-new-buffer)
 ;; (autoload 'w3m-browse-url  "w3m"  "Ask a WWW browser to show a URL." t)

  ;; (setq engine/browser-function 'w3m-browse-url);;  browse-url-default-browser open firefox
  ;; (setq engine/browser-function 'eww-browse-url);;  browse-url-default-browser open firefox

  (setq browse-url-secondary-browser-function 'browse-url-generic
      browse-url-generic-program "qutebrowser"))

;; (setq browse-url-handlers
;;       '(("https:\\/\\/www\\.youtu\\.*be." . browse-url-mpv)
;;         ("." . browse-url-generic)))
;; (defun browse-url-mpv (url &optional single)
;;   (start-process "mpv" nil "mpv" (shell-quote-argument url)))




(use-package! eww
  :config
  (setq eww-download-directory (expand-file-name "~/Documents/eww-downloads"))
  (setq eww-suggest-uris
        '(eww-links-at-point
          thing-at-point-url-at-point))
  ;; (setq eww-bookmarks-directory (locate-user-emacs-file "eww-bookmarks/"))
  (setq eww-bookmarks-directory (expand-file-name "~/.config/eww-bookmarks/"))
  (setq bookmark-file (expand-file-name "~/.config/eww-bookmarks/emacs-bookmarks"))
  (setq eww-use-external-browser-for-content-type
        "\\`\\(video/\\|audio\\)") ; On GNU/Linux check your mimeapps.list
   ;; "\\`\\(video/\\|audio/\\|application/ogg\\|application/pdf\\)"))
   ;;
  ;; NOTE `eww-retrieve-command' is for Emacs28.  I tried the following
  ;; two values.  The first would not render properly some plain text
  ;; pages, such as by messing up the spacing between paragraphs.  The
  ;; second is more reliable but feels slower.  So I just use the
  ;; default (nil), though I find wget to be a bit faster.  In that case
  ;; one could live with the occasional errors by using `eww-download'
  ;; on the offending page, but I prefer consistency.
  ;;
  ;; '("wget" "--quiet" "--output-document=-")
  ;; '("chromium" "--headless" "--dump-dom")



;; list buffers gt

  (define-key eww-link-keymap (kbd "v") nil) ; stop overriding `eww-view-source'
  (after! dired
  (define-key dired-mode-map (kbd "E") #'eww-open-file)) ; to render local HTML files
  (define-key eww-buffers-mode-map (kbd "d") #'eww-buffer-kill)   ; it actually deletes
  (define-key eww-bookmark-mode-map (kbd "d") #'eww-bookmark-kill)) ; same

(use-package! elpher)    ; NOTE 2021-07-24: work-in-progress

(load "~/.config/doom/lisp/prot-eww.el")
(use-package! prot-eww
  :config
  (setq prot-eww-save-history-file
        (locate-user-emacs-file "prot-eww-visited-history"))
  (setq prot-eww-save-visited-history t)
  (setq prot-eww-bookmark-link nil)

  (add-hook 'prot-eww-history-mode-hook #'hl-line-mode)

  (define-prefix-command 'prot-eww-map)
  ;; (define-key global-map (kbd "C-c w") 'prot-eww-map)
  (define-key global-map (kbd "s-B") 'prot-eww-map)
  (let ((map prot-eww-map))
    (define-key map (kbd "b") #'prot-eww-visit-bookmark)
    (define-key map (kbd "e") #'prot-eww-browse-dwim)
    (define-key map (kbd "s") #'prot-eww-search-engine)
    (define-key map (kbd "h") #'prot-eww-list-history)))
;;keybinding are almost same to qutebrowser for consistency
(map! :leader
      :desc "Eww" "@ " #'prot-eww-map)
(evil-define-key 'normal eww-mode-map        (kbd "C-F") #'prot-eww-find-feed
                                              (kbd ".") #'prot-eww-list-history ;key fine

                                               (kbd "t") #'prot-eww-search-engine
                                               (kbd "C-r") #'prot-eww-readable
                                               (kbd "Q") #'prot-eww-quit)


(evil-define-key 'normal prot-eww-history-mode-map  (kbd "RET") #'prot-eww-history-browse
                                               (kbd "o") #'prot-eww-open-in-other-window)




