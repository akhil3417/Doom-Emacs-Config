;;; lisp/new/general.el -*- lexical-binding: t; -*-

(setq org-directory "~/org/")
(setq user-full-name "Akhil Pratap Singh"
      user-mail-address "akhilpratapsingh3417@gmail.com")
;; Personal Information:2 ends here
(setq auth-sources '("~/.authinfo")
      auth-source-cache-expiry nil)
(use-package! auth-source-pass
  :init (auth-source-pass-enable))

; [[file:config.org::*Auto-customisations][Auto-customisations:1]]
(setq-default custom-file (expand-file-name ".custom.el" doom-user-dir))
(when (file-exists-p custom-file)
  (load custom-file))

(add-hook 'doom-first-buffer-hook
          (defun +abbrev-file-name ()
            (setq-default abbrev-mode t)
            (setq abbrev-file-name (expand-file-name "abbrev_defs" doom-user-dir))))


(setq eww-download-directory "~/Downloads/")
(setq eww-bookmarks-directory (expand-file-name "~/.config/eww-bookmarks/"))
(setq bookmark-file (expand-file-name "~/.config/eww-bookmarks/emacs-bookmarks"))

;; (after! projectile (setq projectile-project-root-files-bottom-up (remove ".git"
;;           projectile-project-root-files-bottom-up)))

(use-package! emacs
  :config
  (defun dir-concat (dir file)
    "join path DIR with filename FILE correctly"
    (concat (file-name-as-directory dir) file))

  ;; ;; Adds ~/.emacs.d to the load-path
  ;; (push (dir-concat user-emacs-directory "plugins/") load-path)
  ;; (push (dir-concat user-emacs-directory "lisp/") load-path)

  ;; cache directory
  (defvar user-cache-directory "~/.cache/emacs/"
    "Location where files created by emacs are placed."))

(setq evil-vsplit-window-right t
      evil-split-window-below t)

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))


(setq shell-file-name "/bin/bash"
      vterm-max-scrollback 5000)
(setq eshell-rc-script "~/.config/doom/eshell/profile"
      eshell-aliases-file "~/.config/doom/eshell/aliases"
      eshell-history-size 50000
      eshell-buffer-maximum-lines 50000
      eshell-hist-ignoredups t
      eshell-scroll-to-bottom-on-input t
      eshell-destroy-buffer-when-process-dies t
      eshell-visual-commands'("bash" "fish" "htop" "ssh" "top" "zsh"))

(use-package! savehist
  :config
  (setq savehist-file (locate-user-emacs-file "savehist"))
  (setq history-length 10000)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (add-hook 'after-init-hook #'savehist-mode))

(defun +insert-todays-date (prefix)
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%A, %B %d, %Y")
                 ((equal prefix '(4)) "%m-%d-%Y")
                 ((equal prefix '(16)) "%Y-%m-%d"))))
    (insert (format-time-string format))))

(require 'calendar)
(defun +insert-any-date (date)
  "Insert DATE using the current locale."
  (interactive (list (calendar-read-date)))
  (insert (calendar-date-string date)))

(map! :leader
      (:prefix ("i d" . "Insert date")
       :desc "Insert any date" "a" #'+insert-any-date
       :desc "Insert todays date" "t" #'+insert-todays-date))

(setq projectile-ignored-projects '("~/" "/tmp" "~/.emacs.d/.local/straight/repos/"))
(defun projectile-ignored-project-function (filepath)
  "Return t if FILEPATH is within any of `projectile-ignored-projects'"
  (or (mapcar (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects)))


;;; Repeatable key chords (repeat-mode)
(use-package! repeat
  :config
  (setq repeat-on-final-keystroke t)
  (setq set-mark-command-repeat-pop t)

  (repeat-mode 1))

(setq ibuffer-saved-filter-groups
      '(("Home"
         ;; ("Modified" (predicate buffer-modified-p (current-buffer)))
         ("EXWM" (mode . exwm-mode))
         ("dired" (mode . dired-mode))
         ("Web Development" (or (mode . css-mode)
                                (mode . html-mode)
                                (mode . mhtml-mode)
                                (mode . web-mode)
                                (mode . less-mode)
                                (mode . scss-mode)
                                (mode . android-mode)))
         ("Clojure Development" (or (mode . clojure-mode)
                                    (mode . clojurescript-mode)))
         ("perl" (mode . cperl-mode))
         ("erc" (mode . erc-mode))
         ;; ("Dev" (or (filename . ".+\\\\.css\\\\'")
         ;;            (filename . ".+\\\\.html?\\\\'")
         ;;            (mode . android-mode)
         ;;            (mode . clojure-mode)))
         ("Mail" (or (mode . mu4e-headers-mode)
                     (mode . mu4e-view-mode)
                     (mode . mu4e-compose-mode)
                     (name . ".*Mail*.")
                     (name . ".*draft*.")
                     (filename . "/home/shiva/doom.org")))
         ("planner" (or
                     (name . "^\\*Calendar\\*$")
                     (name . "^diary$")
                     ;; (filename . "(.*agenda.*|.*todo.*)") ;; should'nt it work?
                     (filename . ".*agenda.*")
                     (filename . ".*todo.*")
                     (mode . muse-mode)))

         ("org-roam" (filename . ".*2022.*\.org$"))
         ("org" (mode . org-mode))

         ("Feeds" (or (mode . elfeed-search-mode)
                      (mode . elfeed-show-mode)
                      (name . "*elfeed-log*")))
         ("svg" (name . "\\.svg")) ; group by file extension
         ("emacs" (or
                   (name . "^\\*scratch\\*$")
                   ;; (filename . "~/.config/doom/config.org")
                   (name . "^\\*Messages\\*$")))
         ("Telegram" (or (mode . telega-root-mode)
                         (mode . telega-chat-mode)
                         (mode . telega-image-mode)))
         ("Special" (or (mode . special-mode)
                        (name . "^\\*.\\*$"))))))

;; Repeatable key chords (repeat-mode):1 ends here

(defvar resize-window-repeat-map
  (let ((map (make-sparse-keymap)))
    ;; Standard keys:
    (define-key map "^" 'enlarge-window)
    (define-key map "}" 'enlarge-window-horizontally)
    (define-key map "{" 'shrink-window-horizontally) ; prot note: those three are C-x KEY
    ;; Additional keys:
    (define-key map "v" 'shrink-window) ; prot note: this is not bound by default
    map)
  "Keymap to repeat window resizing commands.  Used in `repeat-mode'.")
(put 'enlarge-window 'repeat-map 'resize-window-repeat-map)
(put 'enlarge-window-horizontally 'repeat-map 'resize-window-repeat-map)
(put 'shrink-window-horizontally 'repeat-map 'resize-window-repeat-map)
(put 'shrink-window 'repeat-map 'resize-window-repeat-map)


(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))

(after! text-mode
  (add-hook! 'text-mode-hook
    (unless (derived-mode-p 'org-mode)
      ;; Apply ANSI color codes
      (with-silent-modifications
        (ansi-color-apply-on-region (point-min) (point-max) t)))))
;; Ansi colours:1 ends here

;; [[file:config.org::*Margin without line numbers][Margin without line numbers:1]]
(defvar +text-mode-left-margin-width 1
  "The `left-margin-width' to be used in `text-mode' buffers.")

(defun +setup-text-mode-left-margin ()
  (when (and (derived-mode-p 'text-mode)
             (eq (current-buffer) ; Check current buffer is active.
                 (window-buffer (frame-selected-window))))
    (setq left-margin-width (if display-line-numbers
                                0 +text-mode-left-margin-width))
    (set-window-buffer (get-buffer-window (current-buffer))
                       (current-buffer))))
;; Margin without line numbers:1 ends here

;; [[file:config.org::*Margin without line numbers][Margin without line numbers:2]]
(add-hook 'window-configuration-change-hook #'+setup-text-mode-left-margin)
(add-hook 'display-line-numbers-mode-hook #'+setup-text-mode-left-margin)
(add-hook 'text-mode-hook #'+setup-text-mode-left-margin)
;; Margin without line numbers:2 ends here

;; [[file:config.org::*Margin without line numbers][Margin without line numbers:3]]
(defadvice! +doom/toggle-line-numbers--call-hook-a ()
  :after #'doom/toggle-line-numbers
  (run-hooks 'display-line-numbers-mode-hook))
;; Margin without line numbers:3 ends here
;;
(remove-hook 'text-mode-hook #'display-line-numbers-mode)

(use-package beframe ; another package of mine (work-in-progress)
  :config
  (setq beframe-functions-in-frames '(project-prompt-project-dir))

  (beframe-mode 1)

  (let ((map global-map))
    (define-key map (kbd "C-x f") #'other-frame-prefix) ; override `set-fill-column'
    ;; Also see `beframe-switch-buffer-in-frame'.
    (define-key map (kbd "C-x B") #'beframe-switch-buffer)))

(let ((map global-map))
  (define-key map (kbd "C-x C-n") #'next-buffer)     ; override `set-goal-column'
  (define-key map (kbd "C-x C-p") #'previous-buffer)) ; override `mark-page'

