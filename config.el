;;; config.el -*- lexical-binding: t; -*-

;; (load "~/.config/doom/exwmmain.el")
;; (setq package-native-compile t)
(setq org-directory "~/org/")
(setq user-full-name "Akhil Pratap Singh"
      user-mail-address "akhilpratapsingh3417@gmail.com")
;; Personal Information:2 ends here

;; [[file:config.org::*Personal Information][Personal Information:3]]
(setq auth-sources '("~/.authinfo")
      auth-source-cache-expiry nil)
 ; default is 7200 (2h)
(use-package! auth-source-pass
  :init (auth-source-pass-enable))
;; Personal Information:3 ends here

(load "~/.config/doom/lisp/codeiumconfig.el")
(load "~/.config/doom/lisp/setup-avy.el")
;; (load "~/.config/doom/lisp/splash.el")
(load "~/.config/doom/lisp/setup-ui.el")
(load "~/.config/doom/lisp/setup-minibuffer.el")
(load "~/.config/doom/lisp/better-buffers.el");;essential
;; (load "~/.config/doom/lisp/setup-orderless.el");;configured in doom already but...
;; (load "~/.config/doom/lisp/setup-vertico.el")
;; (load "~/.config/doom/lisp/setup-embark.el")
(load "~/.config/doom/lisp/utilities.el");;essential
(load "~/.config/doom/lisp/setup-isearch")
;; ytel provides an elfeed-like interface to search invidious instances for
;; youtube videos. Phew. The churn rate of Invidious urls is quite high, which
;; makes this flaky, but anything's better than the browser interface to
;; Youtube.
;; (load "~/.config/doom/lisp/setup-ytel.el");; youttuuube
;; (load "~/.config/doom/lisp/ytdl-downloader.el")
(load "~/.config/doom/lisp/setup-shell.el")
;; (load "~/.config/doom/lisp/exwm-paste.el")
;; (load "~/.config/doom/lisp/lock-screen.el")
(load "~/.config/doom/lisp/correct-previous-word-and-create-abbrev")
;; (load "~/.config/doom/lisp/lookup-on-github")
(load "~/.config/doom/lisp/show-diffs-before-killing-buffers")
;; (load "~/.config/doom/lisp/switch-window-patches")
;; (load "~/.config/doom/lisp/pulseaudio.el")
(load "~/.config/doom/lisp/auto-scroll.el")
(load "~/.config/doom/lisp/prot-common.el")
(load "~/.config/doom/lisp/prot-comment.el")
(load "~/.config/doom/lisp/prot-bookmark.el")

;; [[file:config.org::*Auto-customisations][Auto-customisations:1]]
(setq-default custom-file (expand-file-name ".custom.el" doom-user-dir))
(when (file-exists-p custom-file)
  (load custom-file))
;; Auto-customisations:1 ends here

;; [[file:config.org::*Windows Split][Windows Split:1]]
(setq evil-vsplit-window-right t
      evil-split-window-below t)
;; Windows Split:1 ends here

;; [[file:config.org::*Windows Split][Windows Split:2]]
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))

(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

(setq-default tab-width 4)
(global-set-key (kbd "C-?") #'execute-extended-command)

(global-set-key (kbd "<C-escape>") 'consult-buffer)
(setq display-line-numbers-type nil)
(map! :leader
      :desc "Comment or uncomment lines" "TAB TAB" #'comment-line
      (:prefix ("t" . "toggle")
       :desc "Toggle line numbers" "l" #'doom/toggle-line-numbers
       :desc "Toggle line highlight in frame" "h" #'hl-line-mode
       :desc "Toggle line highlight globally" "H" #'global-hl-line-mode
       :desc "Toggle truncate lines" "t" #'toggle-truncate-lines))

(setq doom-font (font-spec :family "JetBrains Mono" :size 18)
      doom-big-font (font-spec :family "JetBrains Mono" :size 27)
      doom-variable-pitch-font (font-spec :family "Overpass" :size 20)
      doom-unicode-font (font-spec :family "JuliaMono")
      doom-serif-font (font-spec :family "IBM Plex Mono" :size 17 :weight 'light))

;; Thin grey line separating windows
(set-face-background 'vertical-border "black")
(set-face-foreground 'vertical-border (face-background 'vertical-border))


(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
   doom-themes-treemacs-enable-variable-pitch nil))
 (custom-set-faces!
   '(font-lock-comment-face :slant italic)
   '(font-lock-keyword-face :slant italic))

(use-package! doom-themes
  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))
  (doom-themes-visual-bell-config)
(map! :leader
      :desc "Load new theme" "h t" #'load-theme)


(set-face-attribute 'mode-line nil :font "Ubuntu Mono-13")

(setq display-time-format "%l:%M %p %b %y"
      display-time-default-load-average nil)

(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange"))

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

;; [[file:config.org::*Abbrev][Abbrev:1]]
(add-hook 'doom-first-buffer-hook
          (defun +abbrev-file-name ()
            (setq-default abbrev-mode t)
            (setq abbrev-file-name (expand-file-name "abbrev_defs" doom-user-dir))))
;; Abbrev:1 ends here

;; [[file:config.org::*Save Macros][Save Macros:1]]
  (defun save-macro (name)
    "save a macro. Take a name as argument
     and save the last defined macro under
     this name at the end of your .emacs"
     (interactive "SName of the macro: ")  ; ask for the name of the macro
     (kmacro-name-last-macro name)         ; use this name for the macro
     (find-file custom-file)            ; open ~/.emacs or other user init file
     (goto-char (point-max))               ; go to the end of the .emacs
     (newline)                             ; insert a newline
     (insert-kbd-macro name)               ; copy the macro
     (newline)                             ; insert a newline
     (save-buffer)
     (switch-to-buffer nil))               ; return to the initial buffer

  (define-key evil-normal-state-map (kbd "C-t") 'transpose-chars)
;; Paste in Visual Mode:1 ends here

;; [[file:config.org::*Unbind certain Emacs keybindings in =evil-mode=][Unbind certain Emacs keybindings in =evil-mode=:1]]
  (define-key evil-normal-state-map (kbd "C-?") #'execute-extended-command)
  (define-key evil-normal-state-map (kbd "C-.") #'embark-act)
  (define-key evil-normal-state-map (kbd "t") #'my/avy-goto-char-this-window)
  (define-key evil-normal-state-map (kbd "T") #'avy-goto-char-timer)
  (define-key evil-normal-state-map (kbd "C-n") nil)
  (define-key evil-normal-state-map (kbd "M-<right>") #'sp-backward-barf-sexp)
  (define-key evil-normal-state-map (kbd "M-<left>") #'sp-backward-slurp-sexp)
  (define-key evil-normal-state-map (kbd "C-<right>") #'sp-forward-slurp-sexp)
  (define-key evil-normal-state-map (kbd "C-<left>") #'sp-forward-barf-sexp)

  ;; (define-key org-mode-map (kbd "C-'") nil) ;; need that for embark act
  (evil-define-key 'normal org-mode-map (kbd "M-j") nil) ;; avvvy
;; for org-agenda-custom
  ;; (define-key org-super-agenda-header-map (kbd "j") nil) ;; its irrtating to be prompted when in custom org-agenda heading , can just use gd
  ;; (define-key org-super-agenda-header-map (kbd "k") nil) ;; just use SPC-X , for capture

  (define-key evil-normal-state-map (kbd "C-M-d") nil) ;;change default evil-multiedit-restore in favor of sp-down-sexp
;; Unbind certain Emacs keybindings in =evil-mode=:1 ends here

;; [[file:config.org::*Unbind certain Emacs keybindings in =evil-mode=][Unbind certain Emacs keybindings in =evil-mode=:2]]
  (define-key evil-normal-state-map (kbd "M-.") nil)
  (define-key evil-normal-state-map (kbd "M-,") nil)
;; Unbind certain Emacs keybindings in =evil-mode=:2 ends here

;; [[file:config.org::*Unbind certain Emacs keybindings in =evil-mode=][Unbind certain Emacs keybindings in =evil-mode=:5]]
  (define-key global-map (kbd "<tab>") nil)
  (define-key evil-insert-state-map (kbd "<tab>") nil)
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
;; Unbind certain Emacs keybindings in =evil-mode=:5 ends here

(map! :map corfu-map
      :desc "insert separator" "C-SPC" #'corfu-insert-separator)

(use-package! corfu
  :config
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer))
;; Automatic documentation popup while autocompleting is nice, but let’s reduce
;; the font size a little bit so that it doesn’t cover the screen too much and
;; makes it easier to skim for information:
(custom-set-faces! '((corfu-popupinfo) :height 0.9))


;; [[file:config.org::*EVIL][EVIL:1]]
(after! evil
  (setq evil-ex-substitute-global t     ; I like my s/../.. to by global by default
        evil-move-cursor-back nil       ; Don't move the block cursor when toggling insert mode
        evil-kill-on-visual-paste nil)) ; Don't put overwritten text in the kill ring

(use-package substitute
  :config
  ;; Set this to non-nil to highlight all occurences of the current
  ;; target.
  ;; (setopt substitute-highlight t)
  (setq substitute-highlight t)

  ;; Set this to t if you want to always treat the letter casing
  ;; literally.  Otherwise each command accepts a `C-u' prefix
  ;; argument to do this on-demand.
  (setq substitute-fixed-letter-case nil)

  ;; Produce a message after the substitution that reports on what
  ;; happened.  It is a single line, like "Substituted `TARGET' with
  ;; `SUBSTITUTE' N times across the buffer.
  (add-hook 'substitute-post-replace-hook #'substitute-report-operation)

  ;; The mnemonic for the prefix is that M-# (or M-S-3) is close to
  ;; M-% (or M-S-5).
  (let ((map global-map))
    (define-key map (kbd "M-# s") #'substitute-target-below-point) ; Forward motion like isearch (C-s)
    (define-key map (kbd "M-# r") #'substitute-target-above-point) ; Backward motion like isearch (C-r)
    (define-key map (kbd "M-# d") #'substitute-target-in-defun)    ; "defun" mnemonic
    (define-key map (kbd "M-# b") #'substitute-target-in-buffer))) ; "buffer" mnemonic
;; substitute:2 ends here

(use-package! evil-goggles
  :init
  (setq evil-goggles-enable-change t
        evil-goggles-enable-delete t
        evil-goggles-pulse         t
        evil-goggles-duration      0.25)
  :config
  (custom-set-faces!
    `((evil-goggles-yank-face evil-goggles-surround-face)
      :background ,(doom-blend (doom-color 'blue) (doom-color 'bg-alt) 0.5)
      :extend t)
    `(evil-goggles-paste-face
      :background ,(doom-blend (doom-color 'green) (doom-color 'bg-alt) 0.5)
      :extend t)
    `(evil-goggles-delete-face
      :background ,(doom-blend (doom-color 'red) (doom-color 'bg-alt) 0.5)
      :extend t)
    `(evil-goggles-change-face
      :background ,(doom-blend (doom-color 'orange) (doom-color 'bg-alt) 0.5)
      :extend t)
    `(evil-goggles-commentary-face
      :background ,(doom-blend (doom-color 'grey) (doom-color 'bg-alt) 0.5)
      :extend t)
    `((evil-goggles-indent-face evil-goggles-join-face evil-goggles-shift-face)
      :background ,(doom-blend (doom-color 'yellow) (doom-color 'bg-alt) 0.25)
      :extend t)
    ))


(setq ispell-dictionary "en-custom")
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(setq ispell-personal-dictionary (expand-file-name ".ispell_personal" doom-user-dir))
(setq ispell-hunspell-dictionary-alist "/usr/share/myspell/en-custom.dic")

;; [[file:config.org::*Eshell][Eshell:1]]
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
(map! :leader
      :desc "Eshell" "e s" #'eshell
      :desc "Eshell popup toggle" "e t" #'+eshell/toggle
      ;; :desc "Counsel eshell history" "e h" #'counsel-esh-history
      :desc "Counsel eshell history" "e h" #'+eshell/search-history
      :desc "Vterm popup toggle" "v t" #'+vterm/toggle)

(map! :leader
      (:prefix ("=" . "open file")
       :desc "Edit agenda file" "a" #'(lambda () (interactive) (find-file "~/org/org-agenda/agenda.org"))
       :desc "Edit doom config.org" "c" #'(lambda () (interactive) (find-file "~/.config/doom/config.org"))
       :desc "Edit doom init.el" "i" #'(lambda () (interactive) (find-file "~/.config/doom/init.el"))
       :desc "Edit doom packages.el" "p" #'(lambda () (interactive) (find-file "~/.config/doom/packages.el"))))
(map! :leader
      (:prefix ("= e" . "open eshell files")
       :desc "Edit eshell aliases" "a" #'(lambda () (interactive) (find-file "~/.config/doom/eshell/aliases"))
       :desc "Edit eshell profile" "p" #'(lambda () (interactive) (find-file "~/.config/doom/eshell/profile"))))

(defvar counsel-network-manager-history nil
  "Network manager history.")

(defun counsel-network-manager (&optional initial-input)
  "Connect to wifi network."
  (interactive)
  (shell-command "nmcli device wifi rescan")
  (let ((networks-list (s-split "\n" (shell-command-to-string "nmcli device wifi list"))))
    (ivy-read "Select network" networks-list
              :initial-input initial-input
              :require-match t
              :history counsel-network-manager-history
              :sort nil
              :caller 'counsel-network-manager
              :action (lambda (line)
                        (let ((network (car (s-split " " (s-trim (s-chop-prefix "*" line)) t))))
                          (message "Connecting to \"%s\".." network)
                          (async-shell-command
                           (format "nmcli device wifi connect %s" (shell-quote-argument network))))))))
;; Network Manager:1 ends here

;; [[file:config.org::*url util][url util:1]]
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

;; [[file:config.org::*launch firefox][launch firefox:1]]
    (defun my/launch-firefox-private (&optional arg)
      "Launch Firefox.
  With `\\[universal-argument]' prefix argument ARG, create private
  window."
      (interactive "P")
      (make-process
       :name "firefox"
       :command `("firefox" ,(if arg "--private-window" "--new-window"))))
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

;; [[file:config.org::*telega][telega:2]]

;; (setq telega-tdlib-max-version "1.8.5")
    ;; Launch Telega in workspace 0 if we've logged in before
(when (file-exists-p "~/.telega/db.sqlite")
  (telega nil)
  (setq telega-notifications-mode t)
  (load "~/.config/doom/lisp/setup-telega.el"))
;; telega:2 ends here

;; [[file:config.org::*Google Translate][Google Translate:2]]
     (use-package! google-translate
       :demand t
       :init
            (require 'google-translate)
       :functions (my-google-translate-at-point google-translate--search-tkk)
       :custom
       (google-translate-backend-method 'curl)
       :config
       (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))
       (defun my-google-translate-at-point()
         "reverse translate if prefix"
         (interactive)
         (if current-prefix-arg
             (google-translate-at-point)
           (google-translate-at-point-reverse)))
       :bind
       ("C-c t". my-google-translate-at-point))
(use-package! emacs
  :config
  (defvar google-search-history nil
    "List of queries to google-search-string.")
  (defun google-search-string (search-string)
    "Read SEARCH-STRING from the minibuffer and call the shell
command tuxi on it."
    (interactive (list (read-string "Google: " nil
                                    google-search-history
                                    (thing-at-point 'sexp))))
    (unless (executable-find "ls");; m dumb,
      (user-error "Cannot find shell command: tuxipy"))
    (let ((search-output (string-trim-right
                          (shell-command-to-string
                           (concat
                            "python3 -m tuxipy "
                            (shell-quote-argument search-string))))))
      (with-current-buffer (get-buffer-create "*Tuxi Output*")
        (goto-char (point-max))
        (unless (bobp) (insert "\n\n* * *\n"))
        (insert (capitalize search-string) ":\n\n")
        (push-mark)
        (insert search-output)
        (let ((lines (count-lines (or (mark) (point-min)) (point-max))))
          (if (<= lines 1)
              (message search-output)
            (let ((win (display-buffer (current-buffer))))
              (set-window-start win (mark))
              (set-window-parameter win 'window-height (min lines 10))
              (goto-address-mode 1)))))))
  (defun google-search-at-point (&optional beg end)
    "Call the shell command tuxi on the symbol at point. With an
active region use it instead."
    (interactive "r")
    (if-let ((search-string (if (use-region-p)
                                (buffer-substring-no-properties beg end)
                              (thing-at-point 'symbol))))
        (google-search-string search-string)
      ;; (message "No symbol to search for at point!")
      (call-interactively #'google-search-string)))
  :bind (:map help-map
              ("g" . google-search-string)
              ("C-=" . google-search-at-point)))
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
(setq bookmark-file (expand-file-name "~/.config/eww-bookmarks/emacs-bookmarks"))
(require 'consult)
(require 'eww)

;;; consult-buffer source
;; Taken with very minor modifications from the Consult wiki
(defvar consult--source-eww
  (list
   :name     "Eww"
   :narrow   ?e
   :category 'eww-bookmark
   :action   (lambda (bm)
               (eww-browse-url (get-text-property 0 'url bm)))
   :items    (lambda ()
               (eww-read-bookmarks)
               (mapcar (lambda (bm)
                         (propertize
                          (plist-get bm :title)
                          'url (plist-get bm :url)))
                       eww-bookmarks))))

;;; annotate with URL
(add-to-list 'consult-buffer-sources 'consult--source-eww 'append)

(defun annotate-eww-bookmark (bm)
  (concat
   (propertize " " 'display `(space :align-to (- right 50)))
   (propertize (get-text-property 0 'url bm) 'face 'completions-annotations)))

(defvar marginalia-annotator-registry)
(with-eval-after-load 'marginalia
  (add-to-list 'marginalia-annotator-registry
               '(eww-bookmark annotate-eww-bookmark builtin none)))

;;; Have Embark treat them as just URLs
(defun transform-eww-bookmark-to-url (target)
  (if (eq (car target) 'eww-bookmark)
      (cons 'url (get-text-property 0 'url (cdr target)))
    target))

(with-eval-after-load 'embark
  (advice-add 'embark--refine-multi-category
              :filter-return #'transform-eww-bookmark-to-url))

(provide 'consult-eww-source)
;; oantolin'eww:1 ends here

;; [[file:config.org::*eww load files][eww load files:1]]
;; EWW is the Emacs Web Wowser, the builtin browser in Emacs.  Below I set urls to open in a specific browser (eww) with browse-url-browser-function.  By default, Doom Emacs does not use 'SPC e' for anything, so I choose to use the format 'SPC e' plus 'key' for these (I also use 'SPC e' for 'eval' keybindings).  I chose to use 'SPC s w' for eww-search-words because Doom Emacs uses 'SPC s' for 'search' commands.

(load "~/.config/doom/lisp/setup-engine-mode.el")
(load "~/.config/doom/lisp/setup-eww.el")
(load "~/.config/doom/lisp/language-detection-eww.el")

(load "~/.config/doom/lisp/browse-url.el")
 ;; eww toggle  images
(load "~/.config/doom/lisp/eww-image-toggle.el")
;; eww load files:1 ends here

;; [[file:config.org::*simple httpd][simple httpd:1]]
(use-package simple-httpd
  :defer t)
;; simple httpd:1 ends here

(load "~/.config/doom/lisp/setup-avy.el")

(use-package! tmr
:config
  (setq tmr-sound-file
        "~/.config/doom/NotificationSound.opus")
  (setq tmr-notification-urgency 'normal)
  (setq tmr-descriptions-list (list "Boil water" "Prepare tea" "Bake bread"))
  (let ((map global-map))
    (define-key map (kbd "C-c n t") #'tmr)
    (define-key map (kbd "C-c n c") #'tmr-cancel)))


;; do some changes as I don't like the way notification is clutterd with usless stuff
(defun tmr-notification-notify (timer)
  "Dispatch a notification for TIMER.

Read: (info \"(elisp) Desktop Notifications\") for details."
  (if (featurep 'dbusbind)
      (let ((title (tmr--long-description-for-finished-timer timer))
            (body ""))
        (notifications-notify
         :title title
         :body body
         :app-name "GNU Emacs"
         :urgency tmr-notification-urgency
         :sound-file tmr-sound-file))
    (warn "Emacs has no DBUS support, TMR notifications unavailable")))

(defun tmr--long-description-for-finished-timer (timer)
  "Return a human-readable description of finished TIMER.
This includes the creation and completion dates as well as the
optional `tmr--timer-description'."
  (let ((start (tmr--format-creation-date timer))
        (end (tmr--format-end-date timer))
        (description (tmr--timer-description timer)))
    ;; For the TMR prefix, see comment in `tmr--long-description'.
    (format "Hey Akhil Time is up!\n%s%s %s\n%s %s"
            (if description (format "%s\n" description) "")
            (propertize "Started" 'face 'success)
            start
            (propertize "Ended" 'face 'error)
            end)))
;; tmr.el (TMR Must Recur):1 ends here
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
;;
;;
;; [[file:config.org::*Projectile][Projectile:1]]
;; [[file:config.org::*Projectile][Projectile:1]]
(setq projectile-ignored-projects '("~/" "/tmp" "~/.emacs.d/.local/straight/repos/"))
(defun projectile-ignored-project-function (filepath)
  "Return t if FILEPATH is within any of `projectile-ignored-projects'"
  (or (mapcar (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects)))
;; Projectile:1 ends here

;; [[file:config.org::*CALENDAR][CALENDAR:1]]
;; https://stackoverflow.com/questions/9547912/emacs-calendar-show-more-than-3-months
(defun dt/year-calendar (&optional year)
  (interactive)
  (require 'calendar)
  (let* (
      (current-year (number-to-string (nth 5 (decode-time (current-time)))))
      (month 0)
      (year (if year year (string-to-number (format-time-string "%Y" (current-time))))))
    (switch-to-buffer (get-buffer-create calendar-buffer))
    (when (not (eq major-mode 'calendar-mode))
      (calendar-mode))
    (setq displayed-month month)
    (setq displayed-year year)
    (setq buffer-read-only nil)
    (erase-buffer)
    ;; horizontal rows
    (dotimes (j 4)
      ;; vertical columns
      (dotimes (i 3)
        (calendar-generate-month
          (setq month (+ month 1))
          year
          ;; indentation / spacing between months
          (+ 5 (* 25 i))))
      (goto-char (point-max))
      (insert (make-string (- 10 (count-lines (point-min) (point-max))) ?\n))
      (widen)
      (goto-char (point-max))
      (narrow-to-region (point-max) (point-max)))
    (widen)
    (goto-char (point-min))
    (setq buffer-read-only t)))

(defun dt/scroll-year-calendar-forward (&optional arg event)
  "Scroll the yearly calendar by year in a forward direction."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (unless arg (setq arg 0))
  (save-selected-window
    (if (setq event (event-start event)) (select-window (posn-window event)))
    (unless (zerop arg)
      (let* (
              (year (+ displayed-year arg)))
        (dt/year-calendar year)))
    (goto-char (point-min))
    (run-hooks 'calendar-move-hook)))

(defun dt/scroll-year-calendar-backward (&optional arg event)
  "Scroll the yearly calendar by year in a backward direction."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (dt/scroll-year-calendar-forward (- (or arg 1)) event))

(map! :leader
      :desc "Scroll year calendar backward" "<left>" #'dt/scroll-year-calendar-backward
      :desc "Scroll year calendar forward" "<right>" #'dt/scroll-year-calendar-forward)

(defalias 'year-calendar 'dt/year-calendar)
;; CALENDAR:1 ends here
(setq projectile-ignored-projects '("~/" "/tmp" "~/.emacs.d/.local/straight/repos/"))
(defun projectile-ignored-project-function (filepath)
  "Return t if FILEPATH is within any of `projectile-ignored-projects'"
  (or (mapcar (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects)))
;; Projectile:1 ends here

;; [[file:config.org::*CALENDAR][CALENDAR:1]]
;; https://stackoverflow.com/questions/9547912/emacs-calendar-show-more-than-3-months
(defun dt/year-calendar (&optional year)
  (interactive)
  (require 'calendar)
  (let* (
      (current-year (number-to-string (nth 5 (decode-time (current-time)))))
      (month 0)
      (year (if year year (string-to-number (format-time-string "%Y" (current-time))))))
    (switch-to-buffer (get-buffer-create calendar-buffer))
    (when (not (eq major-mode 'calendar-mode))
      (calendar-mode))
    (setq displayed-month month)
    (setq displayed-year year)
    (setq buffer-read-only nil)
    (erase-buffer)
    ;; horizontal rows
    (dotimes (j 4)
      ;; vertical columns
      (dotimes (i 3)
        (calendar-generate-month
          (setq month (+ month 1))
          year
          ;; indentation / spacing between months
          (+ 5 (* 25 i))))
      (goto-char (point-max))
      (insert (make-string (- 10 (count-lines (point-min) (point-max))) ?\n))
      (widen)
      (goto-char (point-max))
      (narrow-to-region (point-max) (point-max)))
    (widen)
    (goto-char (point-min))
    (setq buffer-read-only t)))

(defun dt/scroll-year-calendar-forward (&optional arg event)
  "Scroll the yearly calendar by year in a forward direction."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (unless arg (setq arg 0))
  (save-selected-window
    (if (setq event (event-start event)) (select-window (posn-window event)))
    (unless (zerop arg)
      (let* (
              (year (+ displayed-year arg)))
        (dt/year-calendar year)))
    (goto-char (point-min))
    (run-hooks 'calendar-move-hook)))

(defun dt/scroll-year-calendar-backward (&optional arg event)
  "Scroll the yearly calendar by year in a backward direction."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (dt/scroll-year-calendar-forward (- (or arg 1)) event))

(map! :leader
      :desc "Scroll year calendar backward" "<left>" #'dt/scroll-year-calendar-backward
      :desc "Scroll year calendar forward" "<right>" #'dt/scroll-year-calendar-forward)

(defalias 'year-calendar 'dt/year-calendar)
;; CALENDAR:1 ends here

;; [[file:config.org::*Mini-buffer editing more space][Mini-buffer editing more space:2]]
(use-package miniedit
  :commands minibuffer-edit
  :init (miniedit-install))
;; Mini-buffer editing more space:2 ends here

;; ibuffer

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
;; [[file:config.org::*Copy filename to clipboard][Copy filename to clipboard:1]]
(defun my/copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))
;; Copy filename to clipboard:1 ends here

(use-package! uniquify
  :defer 5
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-strip-common-suffix t)
  (setq uniquify-after-kill-buffer-p t))

;; [[file:config.org::*open current file with external program][open current file with external program:1]]
(defun my/open-with (arg)
  "Open visited file in default external program.

      With a prefix ARG always prompt for command to use."
  (interactive "P")
  (when buffer-file-name
    (shell-command (concat
                    (cond
                     ((and (not arg) (eq system-type 'darwin)) "open")
                     ((and (not arg) (member system-type '(gnu gnu/linux gnu/kfreebsd))) "xdg-open")
                     (t (read-shell-command "Open current file with: ")))
                    " "
                    (shell-quote-argument buffer-file-name)))))
;; open current file with external program:1 ends here

;; [[file:config.org::*Org Mode: Insert YouTube video with separate caption][Org Mode: Insert YouTube video with separate caption:2]]
(load "~/.config/doom/lisp/yt-org.el")
;; Org Mode: Insert YouTube video with separate caption:2 ends here

;; [[file:config.org::*CLIPPY][CLIPPY:1]]
(map! :leader
      (:prefix ("c h" . "Help info from Clippy")
       :desc "Clippy describes function under point" "f" #'clippy-describe-function
       :desc "Clippy describes variable under point" "v" #'clippy-describe-variable))
;; CLIPPY:1 ends here
;;
;;
(use-package! lexic
  :commands lexic-search lexic-list-dictionary
  :config
  (map! :map lexic-mode-map
        :n "q" #'lexic-return-from-lexic
        :nv "RET" #'lexic-search-word-at-point
        :n "a" #'outline-show-all
        :n "h" (cmd! (outline-hide-sublevels 3))
        :n "o" #'lexic-toggle-entry
        :n "n" #'lexic-next-entry
        :n "N" (cmd! (lexic-next-entry t))
        :n "p" #'lexic-previous-entry
        :n "P" (cmd! (lexic-previous-entry t))
        :n "E" (cmd! (lexic-return-from-lexic) ; expand
                     (switch-to-buffer (lexic-get-buffer)))
        :n "M" (cmd! (lexic-return-from-lexic) ; minimise
                     (lexic-goto-lexic))
        :n "C-p" #'lexic-search-history-backwards
        :n "C-n" #'lexic-search-history-forwards
        :n "/" (cmd! (call-interactively #'lexic-search))))
;; Dictionary:2 ends here

;; [[file:config.org::*Dictionary][Dictionary:3]]
(defadvice! +lookup/dictionary-definition-lexic (identifier &optional arg)
  "Look up the definition of the word at point (or selection) using `lexic-search'."
  :override #'+lookup/dictionary-definition
  (interactive
   (list (or (doom-thing-at-point-or-region 'word)
             (read-string "Look up in dictionary: "))
         current-prefix-arg))
  (lexic-search identifier nil nil t))
;; Dictionary:3 ends here

;; [[file:config.org::*Diff-mode (and prot-diff.el extensions)][Diff-mode (and prot-diff.el extensions):1]]
;;; Diff-mode (and prot-diff.el extensions)
(use-package! diff-mode
  :config
  (setq diff-default-read-only t)
  (setq diff-advance-after-apply-hunk t)
  (setq diff-update-on-the-fly t)
  ;; The following are from Emacs 27.1
  (setq diff-refine nil)                ; I do it on demand
  (setq diff-font-lock-prettify nil)    ; better for patches
  ;; The following is further controlled by
  ;; `prot-diff-modus-themes-diffs'
  (setq diff-font-lock-syntax 'hunk-also))

(load "~/.config/doom/lisp/prot-diff.el")
 (use-package! prot-diff
   :config
   ;; (prot-diff-modus-themes-diffs)
   (add-hook 'modus-themes-after-load-theme-hook #'prot-diff-modus-themes-diffs)

   (prot-diff-extra-keywords 1)

   ;; `prot-diff-buffer-dwim' replaces the default for `vc-diff' (which I
   ;; bind to another key---see VC section).
   (define-key global-map (kbd "C-x v =") #'prot-diff-buffer-dwim)
   (let ((map diff-mode-map))
     (define-key map (kbd "C-c C-b") #'prot-diff-refine-cycle) ; replace `diff-refine-hunk'
     (define-key map (kbd "C-c C-n") #'prot-diff-narrow-dwim)))
;; Diff-mode (and prot-diff.el extensions):1 ends here

;; [[file:config.org::*Helper function to measure the running time of a function][Helper function to measure the running time of a function:1]]
  (defmacro measure-time (&rest body)
    "Measure the time it takes to evaluate BODY."
    `(let ((time (current-time)))
       ,@body
       (message "%.06f" (float-time (time-since time)))))
;; Helper function to measure the running time of a function:1 ends here

;; [[file:config.org::*Repeatable key chords (repeat-mode)][Repeatable key chords (repeat-mode):1]]
;;; Repeatable key chords (repeat-mode)
(use-package! repeat
  :config
  (setq repeat-on-final-keystroke t)
  (setq set-mark-command-repeat-pop t)

  (repeat-mode 1))
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
;; [[file:config.org::*Eros][Eros:1]]
(setq eros-eval-result-prefix "⟹ ") ; default =>
;; Eros:1 ends here

;; [[file:config.org::*generate password][generate password:1]]
  (defun generate-password-non-interactive ()
     (string-trim (shell-command-to-string "pwgen -A 24")))

  (defun generate-password ()
    "Generates and inserts a new password"
    (interactive)
    (insert
     (shell-command-to-string
      (concat "pwgen -A " (read-string "Length: " "24") " 1"))))
;; generate password:1 ends here

;; [[file:config.org::*Info pages][Info pages:2]]
(use-package! info-colors
  :commands (info-colors-fontify-node))

(add-hook 'Info-selection-hook 'info-colors-fontify-node)
;; Info pages:2 ends here
(use-package! man
   :ensure nil
  :config
  (let ((map Man-mode-map))
    (define-key map (kbd "i") #'Man-goto-section)
    (define-key map (kbd "g") #'Man-update-manpage)))
(evil-define-key 'normal Info-mode-map (kbd "n") 'Info-next)
(evil-define-key 'normal Info-mode-map (kbd "p") 'Info-prev)
(evil-define-key 'normal Info-mode-map (kbd "H") 'Info-up)
(evil-define-key 'normal Info-mode-map (kbd "f") 'Info-goto-node)
(evil-define-key 'normal Info-mode-map (kbd "C-f") 'Info-menu)

;; Tools for manual pages (manpages):1 ends here

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

;; [[file:config.org::*Org][Org:1]]
(after! org

)
;; Org:1 ends here


(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil)
  ;; for proper first-time setup, `org-appear--set-elements'
  ;; needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))

 ;; (add-to-list 'org-emphasis-alist
 ;;              '("=" (:foreground "red")
 ;;                ))

(use-package! org-ol-tree
  :commands org-ol-tree
  :config
  (setq org-ol-tree-ui-icon-set
        (if (and (display-graphic-p)
                 (fboundp 'all-the-icons-material))
            'all-the-icons
          'unicode))
  (org-ol-tree-ui--update-icon-set))

;; (map! :map org-mode-map
;;       :after org
;;       :localleader
;;       :desc "Outline" "O" #'org-ol-tree)

(defun +org-tree-to-indirect-buffer-options (option)
    (let* ((old-value org-indirect-buffer-display))
          (progn
            (setq org-indirect-buffer-display option)
          (org-tree-to-indirect-buffer)
          (setq org-indirect-buffer-display old-value))))

(defun +org-tree-to-indirect-other-window ()
  (interactive)
  (+org-tree-to-indirect-buffer-options 'other-window))

(defun +org-tree-to-indirect-current-window ()
  (interactive)
  (+org-tree-to-indirect-buffer-options 'current-window))

(defun +org-tree-to-indirect-dedicated-frame ()
  (interactive)
  (+org-tree-to-indirect-buffer-options 'dedicated-frame))

(use-package! org-modern
  ;; :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
        org-modern-table-vertical 1
        org-modern-table-horizontal 0.2
        org-modern-list '((43 . "➤")
                          (45 . "–")
                          (42 . "•"))
        org-modern-todo-faces
        '(("TODO" :inverse-video t :inherit org-todo)
          ("PROJ" :inverse-video t :inherit +org-todo-project)
          ("STRT" :inverse-video t :inherit +org-todo-active)
          ("[-]"  :inverse-video t :inherit +org-todo-active)
          ("HOLD" :inverse-video t :inherit +org-todo-onhold)
          ("WAIT" :inverse-video t :inherit +org-todo-onhold)
          ("[?]"  :inverse-video t :inherit +org-todo-onhold)
          ("KILL" :inverse-video t :inherit +org-todo-cancel)
          ("NO"   :inverse-video t :inherit +org-todo-cancel))
        org-modern-footnote
        (cons nil (cadr org-script-display))
        org-modern-block-fringe nil
        org-modern-block-name
        '((t . t)
          ("src" "»" "«")
          ("example" "»–" "–«")
          ("quote" "❝" "❞")
          ("export" "⏩" "⏪"))
        org-modern-progress nil
        org-modern-priority nil
        org-modern-horizontal-rule (make-string 36 ?─)
        org-modern-keyword
        '((t . t)
          ("title" . "𝙏")
          ("subtitle" . "𝙩")
          ("author" . "𝘼")
          ("email" . #("" 0 1 (display (raise -0.14))))
          ("date" . "𝘿")
          ("property" . "☸")
          ("options" . "⌥")
          ("startup" . "⏻")
          ("macro" . "𝓜")
          ("bind" . #("" 0 1 (display (raise -0.1))))
          ("bibliography" . "")
          ("print_bibliography" . #("" 0 1 (display (raise -0.1))))
          ("cite_export" . "⮭")
          ("print_glossary" . #("ᴬᶻ" 0 1 (display (raise -0.1))))
          ("glossary_sources" . #("" 0 1 (display (raise -0.14))))
          ("include" . "⇤")
          ("setupfile" . "⇚")
          ("html_head" . "🅷")
          ("html" . "🅗")
          ("latex_class" . "🄻")
          ("latex_class_options" . #("🄻" 1 2 (display (raise -0.14))))
          ("latex_header" . "🅻")
          ("latex_header_extra" . "🅻⁺")
          ("latex" . "🅛")
          ("beamer_theme" . "🄱")
          ("beamer_color_theme" . #("🄱" 1 2 (display (raise -0.12))))
          ("beamer_font_theme" . "🄱𝐀")
          ("beamer_header" . "🅱")
          ("beamer" . "🅑")
          ("attr_latex" . "🄛")
          ("attr_html" . "🄗")
          ("attr_org" . "⒪")
          ("call" . #("" 0 1 (display (raise -0.15))))
          ("name" . "⁍")
          ("header" . "›")
          ("caption" . "☰")
          ("results" . "🠶")))
  (custom-set-faces! '(org-modern-statistics :inherit org-checkbox-statistics-todo)))

(after! spell-fu
  (cl-pushnew 'org-modern-tag (alist-get 'org-mode +spell-excluded-faces-alist)))
(setq org-directory "~/org"                      ; let's put files here
      org-use-property-inheritance t       ; It's convenient to have properties inherited.
      org-log-done 'time                   ; Having the time a item is done sounds convenient.
      org-list-allow-alphabetical t        ; Have a. A. a) A) list bullets.
      org-catch-invisible-edits 'smart     ; Try not to accidently do weird stuff in invisible regions.
      org-export-with-sub-superscripts '{} ; Don't treat lone _ / ^ as sub/superscripts, require _{} / ^{}.
      org-export-allow-bind-keywords t     ; Bind keywords can be handy
      org-image-actual-width '(0.9))       ; Make the in-buffer display closer to the exported result..
;; (setq org-image-actual-width nil)


      ;; org-export-in-background t                  ; run export processes in external emacs process ; weird enough this gave error :Debugger entered--Lisp error: (void-variable doom-version)


(setq org-use-tag-inheritance nil)
(setq org-startup-indented t)
(setq org-hide-leading-stars t)
(setq org-treat-S-cursor-todo-selection-as-state-change t)
(setq org-hide-emphasis-markers t)
(setq org-support-shift-select t)
(require 'org-inlinetask)
(setq org-time-stamp-custom-formats '("<%A, %B %d, %Y>" . "<%m/%d/%y %a %H:%M>"))
(setq org-archive-location (concat "org/archive-"
                                   (format-time-string "%Y%m" (current-time))
                                   ".org_archive::"))
(with-eval-after-load 'ox
  (require 'ox-pandoc))
(setq org-pandoc-options-for-beamer-pdf '((pdf-engine . "xelatex")))
(setq org-export-with-smart-quotes t)

(setq org-babel-default-header-args
      '((:session . "none")
        (:results . "replace")
        (:exports . "code")
        (:cache . "no")
        (:noweb . "no")
        (:hlines . "no")
        (:tangle . "no")
        (:comments . "link")))

(remove-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'text-mode-hook #'auto-fill-mode)

;; (map! :map evil-org-mode-map
;;       :after evil-org
;;       :n "g <up>" #'org-backward-heading-same-level
;;       :n "g <down>" #'org-forward-heading-same-level
;;       :n "g <left>" #'org-up-element
;;       :n "g <right>" #'org-down-element)

(evil-define-command evil-buffer-org-new (count file)
  "Creates a new ORG buffer replacing the current window, optionally
   editing a certain FILE"
  :repeat nil
  (interactive "P<f>")
  (if file
      (evil-edit file)
    (let ((buffer (generate-new-buffer "*new org*")))
      (set-window-buffer nil buffer)
      (with-current-buffer buffer
        (org-mode)))))
(map! :leader
      (:prefix "b"
       :desc "New empty ORG buffer" "o" #'evil-buffer-org-new))

(map! :leader
      (:prefix "i"
      :nie "z" (cmd! (insert "\u200B"))))

(defun +org-export-remove-zero-width-space (text _backend _info)
  "Remove zero width spaces from TEXT."
  (unless (org-export-derived-backend-p 'org)
    (replace-regexp-in-string "\u200B" "" text)))

(after! ox
  (add-to-list 'org-export-filter-final-output-functions #'+org-export-remove-zero-width-space t))

(setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a.")))

;; (add-hook 'org-mode-hook 'turn-on-flyspell)

;; (cl-defmacro lsp-org-babel-enable (lang)
;;   "Support LANG in org source code block."
;;   (setq centaur-lsp 'lsp-mode)
;;   (cl-check-type lang stringp)
;;   (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
;;          (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
;;     `(progn
;;        (defun ,intern-pre (info)
;;          (let ((file-name (->> info caddr (alist-get :file))))
;;            (unless file-name
;;              (setq file-name (make-temp-file "babel-lsp-")))
;;            (setq buffer-file-name file-name)
;;            (lsp-deferred)))
;;        (put ',intern-pre 'function-documentation
;;             (format "Enable lsp-mode in the buffer of org source block (%s)."
;;                     (upcase ,lang)))
;;        (if (fboundp ',edit-pre)
;;            (advice-add ',edit-pre :after ',intern-pre)
;;          (progn
;;            (defun ,edit-pre (info)
;;              (,intern-pre info))
;;            (put ',edit-pre 'function-documentation
;;                 (format "Prepare local buffer environment for org source block (%s)."
;;                         (upcase ,lang))))))))
;; (defvar org-babel-lang-list
;;   '("go" "python" "ipython" "bash" "sh"))
;; (dolist (lang org-babel-lang-list)
;;   (eval `(lsp-org-babel-enable ,lang)))

(use-package! org-pandoc-import
  :after org)

(use-package! orgdiff
  :defer t
  :config
  (defun +orgdiff-nicer-change-colours ()
    (goto-char (point-min))
    ;; Set red/blue based on whether chameleon is being used
    (if (search-forward "%% make document follow Emacs theme" nil t)
        (setq red  (substring (doom-blend 'red 'fg 0.8) 1)
              blue (substring (doom-blend 'blue 'teal 0.6) 1))
      (setq red  "c82829"
            blue "00618a"))
    (when (and (search-forward "%DIF PREAMBLE EXTENSION ADDED BY LATEXDIFF" nil t)
               (search-forward "\\RequirePackage{color}" nil t))
      (when (re-search-forward "definecolor{red}{rgb}{1,0,0}" (cdr (bounds-of-thing-at-point 'line)) t)
        (replace-match (format "definecolor{red}{HTML}{%s}" red)))
      (when (re-search-forward "definecolor{blue}{rgb}{0,0,1}" (cdr (bounds-of-thing-at-point 'line)) t)
        (replace-match (format "definecolor{blue}{HTML}{%s}" blue)))))
  (add-to-list 'orgdiff-latexdiff-postprocess-hooks #'+orgdiff-nicer-change-colours))

(setq org-todo-keywords
      '((sequence "TODO(t)" "PROJ(p)" "LOOP(r)" "STRT(s)" "WAIT(w@/!)" "HOLD(h)" "IDEA(i)" "FIXME(f@/!)" "REVIEW(v!)" "NEXT(N)" "FOUND(F@/!)" "|" "DONE(d)" "KILL(k@)")
        (sequence "BACKLOG(b)" "PLAN(P)" "READY(r)"   "|" "COMPLETED(c)" "CANC(C@)")
        (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
        (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")))

;; (setq org-todo-keywords-for-agenda '("TODO(t)" "PROJ(p)" "LOOP(r)" "STRT(s)" "WAIT(w@/!)" "HOLD(h)" "IDEA(i)" "FIXME(f@/!)" "DONE(d)" "KILL(k)" "BLOCKED(B@) BACKLOG(b)" "SOMEDAY(s)" "PLAN(P)" "READY(r)" "FOUND(F@/!)" "ACTIVE(a@)" "REVIEW(v!)" "COMPLETED(c)" "CANC(C@)" "[ ](T)" "[-](S)" "[?](W)" "[X](D)"  "OKAY(o)" "YES(y)" "NO(n)"))

(setq org-todo-keywords-for-agenda '("TODO(t)" "PROJ(p)" "LOOP(r)" "STRT(s)" "WAIT(w@/!)" "HOLD(h)" "IDEA(i)" "FIXME(f@/!)" "[ ](T)" "[-](S)" "[?](W)" "[X](D)" "OKAY(o)" "YES(y)" "NO(n)"))

(map! :map org-mode-map
      :localleader
      :desc "View exported file" "v" #'org-view-output-file)

(defun org-view-output-file (&optional org-file-path)
  "Visit buffer open on the first output file (if any) found, using `org-view-output-file-extensions'"
  (interactive)
  (let* ((org-file-path (or org-file-path (buffer-file-name) ""))
         (dir (file-name-directory org-file-path))
         (basename (file-name-base org-file-path))
         (output-file nil))
    (dolist (ext org-view-output-file-extensions)
      (unless output-file
        (when (file-exists-p
               (concat dir basename "." ext))
          (setq output-file (concat dir basename "." ext)))))
    (if output-file
        (if (member (file-name-extension output-file) org-view-external-file-extensions)
            (browse-url-xdg-open output-file)
          (pop-to-buffer (or (find-buffer-visiting output-file)
                             (find-file-noselect output-file))))
      (message "No exported file found"))))

(defvar org-view-output-file-extensions '("pdf" "md" "rst" "txt" "tex" "html")
  "Search for output files with these extensions, in order, viewing the first that matches")
(defvar org-view-external-file-extensions '("html")
  "File formats that should be opened externally.")

(defun +org-insert-file-link ()
  "Insert a file link.  At the prompt, enter the filename."
  (interactive)
  (insert (format "[[%s]]" (org-link-complete-file))))

(map! :after org
      :map org-mode-map
      :localleader
      "l f" #'+org-insert-file-link)

(use-package! doct
  :commands doct)
(load "~/.config/doom/lisp/org-protocol-capture-html.el")
(require 'org-protocol-capture-html)

(after! org-capture
  (defun org-capture-select-template-prettier (&optional keys)
    "Select a capture template, in a prettier way than default
  Lisp programs can force the template by setting KEYS to a string."
    (let ((org-capture-templates
           (or (org-contextualize-keys
                (org-capture-upgrade-templates org-capture-templates)
                org-capture-templates-contexts)
               '(("t" "Task" entry (file+headline "" "Tasks")
                  "* TODO %?\n  %u\n  %a")))))
      (if keys
          (or (assoc keys org-capture-templates)
              (error "No capture template referred to by \"%s\" keys" keys))
        (org-mks org-capture-templates
                 "Select a capture template\n━━━━━━━━━━━━━━━━━━━━━━━━━"
                 "Template key: "
                 `(("q" ,(concat (all-the-icons-octicon "stop" :face 'all-the-icons-red :v-adjust 0.01) "\tAbort")))))))
  (advice-add 'org-capture-select-template :override #'org-capture-select-template-prettier)

  (defun org-mks-pretty (table title &optional prompt specials)
    "Select a member of an alist with multiple keys. Prettified.

  TABLE is the alist which should contain entries where the car is a string.
  There should be two types of entries.

  1. prefix descriptions like (\"a\" \"Description\")
     This indicates that `a' is a prefix key for multi-letter selection, and
     that there are entries following with keys like \"ab\", \"ax\"…

  2. Select-able members must have more than two elements, with the first
     being the string of keys that lead to selecting it, and the second a
     short description string of the item.

  The command will then make a temporary buffer listing all entries
  that can be selected with a single key, and all the single key
  prefixes.  When you press the key for a single-letter entry, it is selected.
  When you press a prefix key, the commands (and maybe further prefixes)
  under this key will be shown and offered for selection.

  TITLE will be placed over the selection in the temporary buffer,
  PROMPT will be used when prompting for a key.  SPECIALS is an
  alist with (\"key\" \"description\") entries.  When one of these
  is selected, only the bare key is returned."
    (save-window-excursion
      (let ((inhibit-quit t)
            (buffer (org-switch-to-buffer-other-window "*Org Select*"))
            (prompt (or prompt "Select: "))
            case-fold-search
            current)
        (unwind-protect
            (catch 'exit
              (while t
                (setq-local evil-normal-state-cursor (list nil))
                (erase-buffer)
                (insert title "\n\n")
                (let ((des-keys nil)
                      (allowed-keys '("\C-g"))
                      (tab-alternatives '("\s" "\t" "\r"))
                      (cursor-type nil))
                  ;; Populate allowed keys and descriptions keys
                  ;; available with CURRENT selector.
                  (let ((re (format "\\`%s\\(.\\)\\'"
                                    (if current (regexp-quote current) "")))
                        (prefix (if current (concat current " ") "")))
                    (dolist (entry table)
                      (pcase entry
                        ;; Description.
                        (`(,(and key (pred (string-match re))) ,desc)
                         (let ((k (match-string 1 key)))
                           (push k des-keys)
                           ;; Keys ending in tab, space or RET are equivalent.
                           (if (member k tab-alternatives)
                               (push "\t" allowed-keys)
                             (push k allowed-keys))
                           (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) (propertize "›" 'face 'font-lock-comment-face) "  " desc "…" "\n")))
                        ;; Usable entry.
                        (`(,(and key (pred (string-match re))) ,desc . ,_)
                         (let ((k (match-string 1 key)))
                           (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) "   " desc "\n")
                           (push k allowed-keys)))
                        (_ nil))))
                  ;; Insert special entries, if any.
                  (when specials
                    (insert "─────────────────────────\n")
                    (pcase-dolist (`(,key ,description) specials)
                      (insert (format "%s   %s\n" (propertize key 'face '(bold all-the-icons-red)) description))
                      (push key allowed-keys)))
                  ;; Display UI and let user select an entry or
                  ;; a sub-level prefix.
                  (goto-char (point-min))
                  (unless (pos-visible-in-window-p (point-max))
                    (org-fit-window-to-buffer))
                  (let ((pressed (org--mks-read-key allowed-keys
                                                    prompt
                                                    (not (pos-visible-in-window-p (1- (point-max)))))))
                    (setq current (concat current pressed))
                    (cond
                     ((equal pressed "\C-g") (user-error "Abort"))
                     ;; Selection is a prefix: open a new menu.
                     ((member pressed des-keys))
                     ;; Selection matches an association: return it.
                     ((let ((entry (assoc current table)))
                        (and entry (throw 'exit entry))))
                     ;; Selection matches a special entry: return the
                     ;; selection prefix.
                     ((assoc current specials) (throw 'exit current))
                     (t (error "No entry available")))))))
          (when buffer (kill-buffer buffer))))))
  (advice-add 'org-mks :override #'org-mks-pretty)

  (defun +doct-icon-declaration-to-icon (declaration)
    "Convert :icon declaration to icon"
    (let ((name (pop declaration))
          (set  (intern (concat "all-the-icons-" (plist-get declaration :set))))
          (face (intern (concat "all-the-icons-" (plist-get declaration :color))))
          (v-adjust (or (plist-get declaration :v-adjust) 0.01)))
      (apply set `(,name :face ,face :v-adjust ,v-adjust))))

  (defun +doct-iconify-capture-templates (groups)
    "Add declaration's :icon to each template group in GROUPS."
    (let ((templates (doct-flatten-lists-in groups)))
      (setq doct-templates (mapcar (lambda (template)
                                     (when-let* ((props (nthcdr (if (= (length template) 4) 2 5) template))
                                                 (spec (plist-get (plist-get props :doct) :icon)))
                                       (setf (nth 1 template) (concat (+doct-icon-declaration-to-icon spec)
                                                                      "\t"
                                                                      (nth 1 template))))
                                     template)
                                   templates))))

  (setq doct-after-conversion-functions '(+doct-iconify-capture-templates))

  (defvar +org-capture-recipies  "~/org/org-capture/recipies.org")
  (setq +org-capture-todo-file  "~/org/org-capture/todo.org")

  (defun set-org-capture-templates ()
    (setq org-capture-templates
          (doct `(("Personal todo" :keys "t"
                   :icon ("checklist" :set "octicon" :color "green")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* TODO %?"
                              "%i %a"))
                  ("Personal note" :keys "N"
                   :icon ("sticky-note-o" :set "faicon" :color "green")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* %?"
                              "%i %a"))

                  ("Web site" :keys "w"
                   :icon ("globe" :set "faicon" :color "blue")
                   :file "~/org/org-capture/webnotes.org"
                   :prepend t
                   :headline ""
                   :type entry
                   :template ("* %?\n%c\n%:initial"))

                  ("Email" :keys "e"
                   :icon ("envelope" :set "faicon" :color "blue")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* TODO %^{type|reply to|contact} %\\3 %? :email:"
                              "Send an email %^{urgancy|soon|ASAP|anon|at some point|eventually} to %^{recipiant}"
                              "about %^{topic}"
                              "%U %i %a"))

                  ("Appointment" :keys "a"
                   :icon ("eye" :set "faicon" :color "lcyan")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Appointment"
                   :type entry
                   :template ("* %? \n:PROPERTIES:\n:calendar-id: akhilpratapsingh3417@gmail.com\n:LOCATION:\n:END:\n\n:org-gcal:\n%^T\n:END:\nLink: %a"))

                  ("Interesting" :keys "i"
                   :icon ("eye" :set "faicon" :color "lcyan")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Interesting"
                   :type entry
                   :template ("* [ ] %{desc}%? :%{i-type}:"
                              "%i %a")
                   :children (("Webpage" :keys "w"
                               :icon ("globe" :set "faicon" :color "green")
                               :desc "%(org-cliplink-capture) "
                               :i-type "read:web")
                              ("Article" :keys "a"
                               :icon ("file-text" :set "octicon" :color "yellow")
                               :desc ""
                               :i-type "read:reaserch")
                              ("\tRecipie" :keys "r"
                               :icon ("spoon" :set "faicon" :color "dorange")
                               :file +org-capture-recipies
                               :headline "Unsorted"
                               :template "%(org-chef-get-recipe-from-url)")
                              ("Information" :keys "i"
                               :icon ("info-circle" :set "faicon" :color "blue")
                               :desc ""
                               :i-type "read:info")
                              ("Idea" :keys "I"
                               :icon ("bubble_chart" :set "material" :color "silver")
                               :desc ""
                               :i-type "idea")))

                  ("Tasks" :keys "k"
                   :icon ("inbox" :set "octicon" :color "yellow")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Tasks"
                   :type entry
                   :template ("* TODO %? %^G%{extra}"
                              "%i %a")
                   :children (("General Task" :keys "k"
                               :icon ("inbox" :set "octicon" :color "yellow")
                               :extra "")
                              ("Task with deadline" :keys "d"
                               :icon ("timer" :set "material" :color "orange" :v-adjust -0.1)
                               :extra "\nDEADLINE: %^{Deadline:}t")
                              ("Scheduled Task" :keys "s"
                               :icon ("calendar" :set "octicon" :color "orange")
                               :extra "\nSCHEDULED: %^{Start time:}t")))

                  ("Project" :keys "p"
                   :icon ("repo" :set "octicon" :color "silver")
                   :prepend t
                   :type entry
                   :headline "Inbox"
                   :template ("* %{time-or-todo} %?"
                              "%i"
                              "%a")
                   :file ""
                   :custom (:time-or-todo "")
                   :children (("Project-local todo" :keys "t"
                               :icon ("checklist" :set "octicon" :color "green")
                               :time-or-todo "TODO"
                               :file +org-capture-project-todo-file)
                              ("Project-local note" :keys "n"
                               :icon ("sticky-note" :set "faicon" :color "yellow")
                               :time-or-todo "%U"
                               :file +org-capture-project-notes-file)
                              ("Project-local changelog" :keys "c"
                               :icon ("list" :set "faicon" :color "blue")
                               :time-or-todo "%U"
                               :heading "Unreleased"
                               :file +org-capture-project-changelog-file)))

                  ("\tCentralised project templates"
                   :keys "o"
                   :type entry
                   :prepend t
                   :template ("* %{time-or-todo} %?"
                              "%i"
                              "%a")
                   :children (("Project todo"
                               :keys "t"
                               :prepend nil
                               :time-or-todo "TODO"
                               :heading "Tasks"
                               :file +org-capture-central-project-todo-file)
                              ("Project note"
                               :keys "n"
                               :time-or-todo "%U"
                               :heading "Notes"
                               :file +org-capture-central-project-notes-file)
                              ("Project changelog"
                               :keys "c"
                               :time-or-todo "%U"
                               :heading "Unreleased"
                               :file +org-capture-central-project-changelog-file)))))))

  (set-org-capture-templates)
  (unless (display-graphic-p)
    (add-hook 'server-after-make-frame-hook
              (defun org-capture-reinitialise-hook ()
                (when (display-graphic-p)
                  (set-org-capture-templates)
                  (remove-hook 'server-after-make-frame-hook
                               #'org-capture-reinitialise-hook))))))
;; Org Capture:3 ends here

;; [[file:config.org::prettify-capture][prettify-capture]]
(defun org-capture-select-template-prettier (&optional keys)
  "Select a capture template, in a prettier way than default
Lisp programs can force the template by setting KEYS to a string."
  (let ((org-capture-templates
         (or (org-contextualize-keys
              (org-capture-upgrade-templates org-capture-templates)
              org-capture-templates-contexts)
             '(("t" "Task" entry (file+headline "" "Tasks")
                "* TODO %?\n  %u\n  %a")))))
    (if keys
        (or (assoc keys org-capture-templates)
            (error "No capture template referred to by \"%s\" keys" keys))
      (org-mks org-capture-templates
               "Select a capture template\n━━━━━━━━━━━━━━━━━━━━━━━━━"
               "Template key: "
               `(("q" ,(concat (all-the-icons-octicon "stop" :face 'all-the-icons-red :v-adjust 0.01) "\tAbort")))))))
(advice-add 'org-capture-select-template :override #'org-capture-select-template-prettier)

(defun org-mks-pretty (table title &optional prompt specials)
  "Select a member of an alist with multiple keys. Prettified.

TABLE is the alist which should contain entries where the car is a string.
There should be two types of entries.

1. prefix descriptions like (\"a\" \"Description\")
   This indicates that `a' is a prefix key for multi-letter selection, and
   that there are entries following with keys like \"ab\", \"ax\"…

2. Select-able members must have more than two elements, with the first
   being the string of keys that lead to selecting it, and the second a
   short description string of the item.

The command will then make a temporary buffer listing all entries
that can be selected with a single key, and all the single key
prefixes.  When you press the key for a single-letter entry, it is selected.
When you press a prefix key, the commands (and maybe further prefixes)
under this key will be shown and offered for selection.

TITLE will be placed over the selection in the temporary buffer,
PROMPT will be used when prompting for a key.  SPECIALS is an
alist with (\"key\" \"description\") entries.  When one of these
is selected, only the bare key is returned."
  (save-window-excursion
    (let ((inhibit-quit t)
          (buffer (org-switch-to-buffer-other-window "*Org Select*"))
          (prompt (or prompt "Select: "))
          case-fold-search
          current)
      (unwind-protect
          (catch 'exit
            (while t
              (setq-local evil-normal-state-cursor (list nil))
              (erase-buffer)
              (insert title "\n\n")
              (let ((des-keys nil)
                    (allowed-keys '("\C-g"))
                    (tab-alternatives '("\s" "\t" "\r"))
                    (cursor-type nil))
                ;; Populate allowed keys and descriptions keys
                ;; available with CURRENT selector.
                (let ((re (format "\\`%s\\(.\\)\\'"
                                  (if current (regexp-quote current) "")))
                      (prefix (if current (concat current " ") "")))
                  (dolist (entry table)
                    (pcase entry
                      ;; Description.
                      (`(,(and key (pred (string-match re))) ,desc)
                       (let ((k (match-string 1 key)))
                         (push k des-keys)
                         ;; Keys ending in tab, space or RET are equivalent.
                         (if (member k tab-alternatives)
                             (push "\t" allowed-keys)
                           (push k allowed-keys))
                         (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) (propertize "›" 'face 'font-lock-comment-face) "  " desc "…" "\n")))
                      ;; Usable entry.
                      (`(,(and key (pred (string-match re))) ,desc . ,_)
                       (let ((k (match-string 1 key)))
                         (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) "   " desc "\n")
                         (push k allowed-keys)))
                      (_ nil))))
                ;; Insert special entries, if any.
                (when specials
                  (insert "─────────────────────────\n")
                  (pcase-dolist (`(,key ,description) specials)
                    (insert (format "%s   %s\n" (propertize key 'face '(bold all-the-icons-red)) description))
                    (push key allowed-keys)))
                ;; Display UI and let user select an entry or
                ;; a sub-level prefix.
                (goto-char (point-min))
                (unless (pos-visible-in-window-p (point-max))
                  (org-fit-window-to-buffer))
                (let ((pressed (org--mks-read-key allowed-keys
                                                  prompt
                                                  (not (pos-visible-in-window-p (1- (point-max)))))))
                  (setq current (concat current pressed))
                  (cond
                   ((equal pressed "\C-g") (user-error "Abort"))
                   ;; Selection is a prefix: open a new menu.
                   ((member pressed des-keys))
                   ;; Selection matches an association: return it.
                   ((let ((entry (assoc current table)))
                      (and entry (throw 'exit entry))))
                   ;; Selection matches a special entry: return the
                   ;; selection prefix.
                   ((assoc current specials) (throw 'exit current))
                   (t (error "No entry available")))))))
        (when buffer (kill-buffer buffer))))))
(advice-add 'org-mks :override #'org-mks-pretty)
;; prettify-capture ends here

;; [[file:config.org::*Org Capture][Org Capture:5]]
(setf (alist-get 'height +org-capture-frame-parameters) 15)
;; (alist-get 'name +org-capture-frame-parameters) "❖ Capture") ;; ATM hardcoded in other places, so changing breaks stuff
(setq +org-capture-fn
      (lambda ()
        (interactive)
        (set-window-parameter nil 'mode-line-format 'none)
        (org-capture)))
;; Org Capture:5 ends here

;; [[file:config.org::*Habit Tracking][Habit Tracking:1]]
(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60)
;; Habit Tracking:1 ends here

;; [[file:config.org::*Super agenda][Super agenda:2]]
(use-package! org-super-agenda
  :commands org-super-agenda-mode)

;; (use-package! org-super-agenda
;;   :hook (org-agenda-mode . org-super-agenda-mode)
;; )
;; Super agenda:2 ends here

;; [[file:config.org::*Super agenda][Super agenda:3]]
(after! org
 ;; '(org-agenda-files
 ;;   '( "~/org/org-capture/todo.org" "~/org/org-capture/webnotes.org" "~/org/org-roam2/daily/" "~/org/org-roam2/todo/todo.org"))
 (setq org-agenda-files '("~/org/org-agenda/" "~/org/org-capture/todo.org")))
 (setq org-default-notes-file "~/org/notes/notes.org")
;; Super agenda:3 ends here

;; [[file:config.org::*Super agenda][Super agenda:5]]
;; (after! org-agenda
;; (org-super-agenda-mode)

(after! org-agenda
  (let ((inhibit-message t))
    (org-super-agenda-mode)))

(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-block-separator nil
      ;; org-agenda-tags-column 100 ;; from testing this seems to be a good value
      org-agenda-start-with-log-mode t
      calendar-latitude 34.034520
      calendar-longitude -84.456010
      calendar-location-name "Marietta, GA"
      org-agenda-compact-blocks t)
      ;; org-agenda-include-diary t) ;; TODO fix diary
;; org-agenda-start-with-log-mode t)

(setq org-agenda-custom-commands
      '(("o" "Overview"
         ((agenda "" ((org-agenda-span 'day)
                      (org-agenda-start-day nil)
                      (org-super-agenda-groups
                       '((:name "Today"
                          :time-grid t
                          :date today
                          :todo "TODAY"
                          :scheduled today
                          :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '((:name "Ongoing"
                           :tag "ongoing"
                           :todo "[-]"
                           :order 1)
                          (:name "Due Today"
                           :deadline today
                           :order 2)
                          (:name "Important"
                           :tag "Important"
                           :priority "A"
                           :order 3)
                          (:name "Next to do"
                           :todo "NEXT"
                           :order 4)
                          (:name "Due Soon"
                           :deadline future
                           :order 8)
                          (:name "Overdue"
                           :deadline past
                           :face error
                           :order 7)
                          (:name "Assignments"
                           :tag "Assignment"
                           :order 10)
                          (:name "Issues"
                           :tag "Issue"
                           :todo ("FIXME" "CLAR")
                           :order 12)
                          (:name "Emacs"
                           :tag "Emacs"
                           :order 13)
                          (:name "Projects"
                           :tag "Project"
                           :todo "PROJ"
                           :order 14)
                          (:name "Research"
                           :tag "Research"
                           :todo "FOUND"
                           :order 15)
                          (:name "To read & Learn"
                           :tag ("Read" "learn")
                           :order 30)
                          (:name "Waiting"
                           :todo "WAIT"
                           :tag "waiting"
                           :order 20)
                          (:name "University"
                           :tag "uni"
                           :order 32)
                          (:name "Binge Watch"
                           :tag ("tvshow" "movie" "documentry")
                           :order 90)
                          (:name "Trivial"
                           :priority<= "E"
                           :tag ("Trivial" "Unimportant")
                           :todo ("SOMEDAY" )
                           :order 36)
                          (:discard (:tag ("Chore" "Routine" "Daily")))))))))
        ("d" "Dashboard"
          ((agenda "" ((org-deadline-warning-days 7)))
           (todo "NEXT"
                 ((org-agenda-overriding-header "Next Tasks")))
           (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

         ("n" "Next Tasks"
          ((todo "NEXT"
                 ((org-agenda-overriding-header "Next Tasks")))))


         ("w" "Work Tasks" tags-todo "+work")

         ;; Low-effort next actions
         ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
          ((org-agenda-overriding-header "Low Effort Tasks")
           (org-agenda-max-todos 20)
           (org-agenda-files org-agenda-files)))

         ("W" "Workflow Status"
          ((todo "WAIT"
                 ((org-agenda-overriding-header "Waiting on External")
                  (org-agenda-files org-agenda-files)))
           (todo "REVIEW"
                 ((org-agenda-overriding-header "In Review")
                  (org-agenda-files org-agenda-files)))
           (todo "PLAN"
                 ((org-agenda-overriding-header "In Planning")
                  (org-agenda-todo-list-sublevels nil)
                  (org-agenda-files org-agenda-files)))
           (todo "BACKLOG"
                 ((org-agenda-overriding-header "Project Backlog")
                  (org-agenda-todo-list-sublevels nil)
                  (org-agenda-files org-agenda-files)))
           (todo "READY"
                 ((org-agenda-overriding-header "Ready for Work")
                  (org-agenda-files org-agenda-files)))
           (todo "ACTIVE"
                 ((org-agenda-overriding-header "Active Projects")
                  (org-agenda-files org-agenda-files)))
           (todo "COMPLETED"
                 ((org-agenda-overriding-header "Completed Projects")
                  (org-agenda-files org-agenda-files)))
           (todo "CANC"
                 ((org-agenda-overriding-header "Cancelled Projects")
                  (org-agenda-files org-agenda-files)))))))


(let ((map global-map))
  (define-key map (kbd "C-c a") #'org-agenda)
  (define-key map (kbd "C-c c") #'org-capture)
  (define-key map (kbd "C-c l") #'org-store-link)
  (define-key map (kbd "C-c L") #'org-insert-link-global)
  (define-key map (kbd "C-c O") #'org-open-at-point-global))
(let ((map org-mode-map))
  (define-key map (kbd "C-c M-l") #'org-insert-last-stored-link)
  (define-key map (kbd "C-c C-M-l") #'org-toggle-link-display))

(defun my-org-check-agenda ()
  "Peek at agenda."
  (interactive)
  (cond
   ((derived-mode-p 'org-agenda-mode)
    (if (window-parent) (delete-window) (bury-buffer)))
   ((get-buffer "*Org Agenda*")
    (switch-to-buffer-other-window "*Org Agenda*"))
   (t (org-agenda nil "a"))))
;; Super agenda:5 ends here

;; [[file:config.org::*Basic settings][Basic settings:1]]
;;(after! org-roam
(setq
      org-roam-directory "~/org/org-roam2/"
      org-roam-db-location (concat org-roam-directory "org-roam.db")
      org-roam-todo-file (concat org-roam-directory "todo/todo.org"))
(save-window-excursion
  (find-file org-roam-todo-file)
  (save-buffer))
;; Basic settings:1 ends here
;;
(defun org-roam-buffer-setup ()
  "Function to make org-roam-buffer more pretty."
  (progn
    (setq-local olivetti-body-width 44)
    (variable-pitch-mode 1)
    (olivetti-mode 1)
    ;; (centaur-tabs-local-mode -1)

  (set-face-background 'magit-section-highlight (face-background 'default))))

(after! org-roam
(add-hook! 'org-roam-mode-hook #'org-roam-buffer-setup))
;;

;; [[file:config.org::*Modeline file name][Modeline file name:1]]
(defadvice! doom-modeline--buffer-file-name-roam-aware-a (orig-fun)
  :around #'doom-modeline-buffer-file-name ; takes no args
  (if (s-contains-p org-roam-directory (or buffer-file-name ""))
      (replace-regexp-in-string
       "\\(?:^\\|.*/\\)\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[0-9]*-"
       "🢔(\\1-\\2-\\3) "
       (subst-char-in-string ?_ ?  buffer-file-name))
    (funcall orig-fun)))
;; Modeline file name:1 ends here

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :commands org-roam-ui-open
  :hook (org-roam . org-roam-ui-mode)
  :config
  (require 'org-roam) ; in case autoloaded
  (defun org-roam-ui-open ()
    "Ensure the server is active, then open the roam graph."
    (interactive)
    (unless org-roam-ui-mode (org-roam-ui-mode 1))
    (browse-url-xdg-open (format "http://localhost:%d" org-roam-ui-port))))

(after! org-roam
  (setq org-roam-capture-templates
        `(("s" "standard" plain "%?"
           :if-new
           (file+head "standard/%<%Y%m%d%H%M%S>--${slug}.org"
                      "#+title: ${title}\n#+date\n#+filetags: \n\n ")
           :unnarrowed t)

          ("d" "definition" plain
           "%?"
           :if-new
           (file+head "definition/${slug}.org" "#+title: ${title}\n#+filetags: definition \n\n* Definition\n\n\n* Examples\n")
           :unnarrowed t)
          ("r" "ref" plain "%?"
           :if-new
           (file+head "ref/${citekey}.org"
                      "#+title: ${slug}: ${title}\n
                      \n#+date : %<%Y%m%d%H%M%S>
                      \n#+filetags: reference ${keywords} \n
                      \n* ${title}\n\n
                      \n* Summary
                      \n\n\n* Rough note space\n")
           :unnarrowed t)
          ("P" "person" plain "%?"
           :if-new
           (file+head "${slug}.org" "%^{relation|some guy|family|friend|colleague}p %^{birthday}p %^{address}p
,#+title:${slug}\n#+filetags: :person: \n")
           :unnarrowed t)
          ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
           :if-new (file+head "Projects/%<%Y%m%d%H%M%S>--${slug}.org" "#+title: ${title}\n#+filetags: Project")
           :unnarrowed t)
          ("b" "book notes" plain
           "\n* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
           :if-new (file+head "BookNotes/%<%Y%m%d%H%M%S>--${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ;; templates can be used as well, which is pretty cool
          ;; ("b" "book notes" plain (file "~/org/org-roam2/Templates/BookNotesTemplate.org")
          ;;  :if-new (file+head "BookNotes/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
          ;;  :unnarrowed t)
          ("l" "programming language" plain
           "* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
           :if-new (file+head "ProgLangs/%<%Y%m%d%H%M%S>--${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)

          ("n" "notes" plain "%?"
           :if-new
           (file+head "Notes/%<%Y%m%d%H%M%S>--${title}.org" "#+title: ${title}\n#+STARTUP: content\n#+date : %<%Y-%m-%d>\n#+filetags: :%^{tags|article:note|learn|info|posix|web|intresting|emacs|gnu_linux|health|food|shopping|pentesting|tv_shows|elfeed|void_linux|internet} ")
           :immediate-finish t
           :unnarrowed t)

          ("a" "article" plain "%?"
           :if-new
           (file+head "Articles/%<%Y%m%d%H%M%S>--${title}.org" "#+title: ${title}\n#+STARTUP: content\n#+date : %<%Y-%m-%d>\n#+filetags: :%^{tags|article:read|learn|info|posix|web|intresting|emacs|gnu_linux|health|food|shopping|pentesting|tv_shows|elfeed|void_linux|internet} ")

           ;; (file+head "Articles/%<%Y%m%d%H%M%S>-${title}.org" "#+title: ${title}\n#+date : %<%Y%m%d%H%M%S>\n#+filetags: :article:%^{tags|read|learn|info}
            ;; \n\n%i %a")
           :immediate-finish t
           :unnarrowed t))))
(setq org-roam-dailies-capture-templates
      '(("d" "default" entry "* %<%I:%M %p>: %?"
         :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))

;; ;; Creating the property “type” on my nodes.
(cl-defmethod org-roam-node-type ((node org-roam-node))
  "Return the TYPE of NODE."
  (condition-case nil
      (file-name-nondirectory
       (directory-file-name
        (file-name-directory
         (file-relative-name (org-roam-node-file node) org-roam-directory))))
    (error "")))

;; Modifying the display template to show the node “type”

;; (setq org-roam-node-display-template
;;        (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
;; #("${doom-type:15} ${doom-hierarchy:*}  ${doom-tags:42}" 20 35
;;   (face font-lock-keyword-face)
;;   36 51
;;   (face org-tag)))

(defun my/org-roam--backlinks-list (file)
  (if (org-roam--org-roam-file-p file)
      (--reduce-from
       (concat acc (format "- [[file:%s][%s]]\n"
                           (file-relative-name (car it) org-roam-directory)
                           (org-roam--get-title-or-slug (car it))))
       "" (org-roam-sql [:select [from]
                         :from links
                         :where (= to $s1)
                         :and from :not :like $s2] file "%private%"))
    ""))

(defun my/org-export-preprocessor (_backend)
  (let ((links (my/org-roam--backlinks-list (buffer-file-name))))
    (unless (string= links "")
      (save-excursion
        (goto-char (point-max))
        (insert (concat "\n* Backlinks\n" links))))))

(defvar org-reference-contraction-max-words 3
  "Maximum number of words in a reference reference.")
(defvar org-reference-contraction-max-length 35
  "Maximum length of resulting reference reference, including joining characters.")
(defvar org-reference-contraction-stripped-words
  '("the" "on" "in" "off" "a" "for" "by" "of" "and" "is" "to")
  "Superfluous words to be removed from a reference.")
(defvar org-reference-contraction-joining-char "-"
  "Character used to join words in the reference reference.")

(defun org-reference-contraction-truncate-words (words)
  "Using `org-reference-contraction-max-length' as the total character 'budget' for the WORDS
and truncate individual words to conform to this budget.

To arrive at a budget that accounts for words undershooting their requisite average length,
the number of characters in the budget freed by short words is distributed among the words
exceeding the average length.  This adjusts the per-word budget to be the maximum feasable for
this particular situation, rather than the universal maximum average.

This budget-adjusted per-word maximum length is given by the mathematical expression below:

max length = \\floor{ \\frac{total length - chars for seperators - \\sum_{word \\leq average length} length(word) }{num(words) > average length} }"
  ;; trucate each word to a max word length determined by
  ;;
  (let* ((total-length-budget (- org-reference-contraction-max-length  ; how many non-separator chars we can use
                                 (1- (length words))))
         (word-length-budget (/ total-length-budget                      ; max length of each word to keep within budget
                                org-reference-contraction-max-words))
         (num-overlong (-count (lambda (word)                            ; how many words exceed that budget
                                 (> (length word) word-length-budget))
                               words))
         (total-short-length (-sum (mapcar (lambda (word)                ; total length of words under that budget
                                             (if (<= (length word) word-length-budget)
                                                 (length word) 0))
                                           words)))
         (max-length (/ (- total-length-budget total-short-length)       ; max(max-length) that we can have to fit within the budget
                        num-overlong)))
    (mapcar (lambda (word)
              (if (<= (length word) max-length)
                  word
                (substring word 0 max-length)))
            words)))

(defun org-reference-contraction (reference-string)
  "Give a contracted form of REFERENCE-STRING that is only contains alphanumeric characters.
Strips 'joining' words present in `org-reference-contraction-stripped-words',
and then limits the result to the first `org-reference-contraction-max-words' words.
If the total length is > `org-reference-contraction-max-length' then individual words are
truncated to fit within the limit using `org-reference-contraction-truncate-words'."
  (let ((reference-words
         (-filter (lambda (word)
                    (not (member word org-reference-contraction-stripped-words)))
                  (split-string
                   (->> reference-string
                        downcase
                        (replace-regexp-in-string "\\[\\[[^]]+\\]\\[\\([^]]+\\)\\]\\]" "\\1") ; get description from org-link
                        (replace-regexp-in-string "[-/ ]+" " ") ; replace seperator-type chars with space
                        puny-encode-string
                        (replace-regexp-in-string "^xn--\\(.*?\\) ?-?\\([a-z0-9]+\\)$" "\\2 \\1") ; rearrange punycode
                        (replace-regexp-in-string "[^A-Za-z0-9 ]" "") ; strip chars which need %-encoding in a uri
                        ) " +"))))
    (when (> (length reference-words)
             org-reference-contraction-max-words)
      (setq reference-words
            (cl-subseq reference-words 0 org-reference-contraction-max-words)))

    (when (> (apply #'+ (1- (length reference-words))
                    (mapcar #'length reference-words))
             org-reference-contraction-max-length)
      (setq reference-words (org-reference-contraction-truncate-words reference-words)))

    (string-join reference-words org-reference-contraction-joining-char)))

(define-minor-mode unpackaged/org-export-html-with-useful-ids-mode
  "Attempt to export Org as HTML with useful link IDs.
Instead of random IDs like \"#orga1b2c3\", use heading titles,
made unique when necessary."
  :global t
  (if unpackaged/org-export-html-with-useful-ids-mode
      (advice-add #'org-export-get-reference :override #'unpackaged/org-export-get-reference)
    (advice-remove #'org-export-get-reference #'unpackaged/org-export-get-reference)))
(unpackaged/org-export-html-with-useful-ids-mode 1) ; ensure enabled, and advice run

(defun unpackaged/org-export-get-reference (datum info)
  "Like `org-export-get-reference', except uses heading titles instead of random numbers."
  (let ((cache (plist-get info :internal-references)))
    (or (car (rassq datum cache))
        (let* ((crossrefs (plist-get info :crossrefs))
               (cells (org-export-search-cells datum))
               ;; Preserve any pre-existing association between
               ;; a search cell and a reference, i.e., when some
               ;; previously published document referenced a location
               ;; within current file (see
               ;; `org-publish-resolve-external-link').
               ;;
               ;; However, there is no guarantee that search cells are
               ;; unique, e.g., there might be duplicate custom ID or
               ;; two headings with the same title in the file.
               ;;
               ;; As a consequence, before re-using any reference to
               ;; an element or object, we check that it doesn't refer
               ;; to a previous element or object.
               (new (or (cl-some
                         (lambda (cell)
                           (let ((stored (cdr (assoc cell crossrefs))))
                             (when stored
                               (let ((old (org-export-format-reference stored)))
                                 (and (not (assoc old cache)) stored)))))
                         cells)
                        (when (org-element-property :raw-value datum)
                          ;; Heading with a title
                          (unpackaged/org-export-new-named-reference datum cache))
                        (when (member (car datum) '(src-block table example fixed-width property-drawer))
                          ;; Nameable elements
                          (unpackaged/org-export-new-named-reference datum cache))
                        ;; NOTE: This probably breaks some Org Export
                        ;; feature, but if it does what I need, fine.
                        (org-export-format-reference
                         (org-export-new-reference cache))))
               (reference-string new))
          ;; Cache contains both data already associated to
          ;; a reference and in-use internal references, so as to make
          ;; unique references.
          (dolist (cell cells) (push (cons cell new) cache))
          ;; Retain a direct association between reference string and
          ;; DATUM since (1) not every object or element can be given
          ;; a search cell (2) it permits quick lookup.
          (push (cons reference-string datum) cache)
          (plist-put info :internal-references cache)
          reference-string))))

(defun unpackaged/org-export-new-named-reference (datum cache)
  "Return new reference for DATUM that is unique in CACHE."
  (cl-macrolet ((inc-suffixf (place)
                             `(progn
                                (string-match (rx bos
                                                  (minimal-match (group (1+ anything)))
                                                  (optional "--" (group (1+ digit)))
                                                  eos)
                                              ,place)
                                ;; HACK: `s1' instead of a gensym.
                                (-let* (((s1 suffix) (list (match-string 1 ,place)
                                                           (match-string 2 ,place)))
                                        (suffix (if suffix
                                                    (string-to-number suffix)
                                                  0)))
                                  (setf ,place (format "%s--%s" s1 (cl-incf suffix)))))))
    (let* ((headline-p (eq (car datum) 'headline))
           (title (if headline-p
                      (org-element-property :raw-value datum)
                    (or (org-element-property :name datum)
                        (concat (org-element-property :raw-value
                                                      (org-element-property :parent
                                                                            (org-element-property :parent datum)))))))
           ;; get ascii-only form of title without needing percent-encoding
           (ref (concat (org-reference-contraction (substring-no-properties title))
                        (unless (or headline-p (org-element-property :name datum))
                          (concat ","
                                  (pcase (car datum)
                                    ('src-block "code")
                                    ('example "example")
                                    ('fixed-width "mono")
                                    ('property-drawer "properties")
                                    (_ (symbol-name (car datum))))
                                  "--1"))))
           (parent (when headline-p (org-element-property :parent datum))))
      (while (--any (equal ref (car it))
                    cache)
        ;; Title not unique: make it so.
        (if parent
            ;; Append ancestor title.
            (setf title (concat (org-element-property :raw-value parent)
                                "--" title)
                  ;; get ascii-only form of title without needing percent-encoding
                  ref (org-reference-contraction (substring-no-properties title))
                  parent (when headline-p (org-element-property :parent parent)))
          ;; No more ancestors: add and increment a number.
          (inc-suffixf ref)))
      ref)))

(add-hook 'org-load-hook #'unpackaged/org-export-html-with-useful-ids-mode)

(defadvice! org-export-format-reference-a (reference)
  "Format REFERENCE into a string.

REFERENCE is a either a number or a string representing a reference,
as returned by `org-export-new-reference'."
  :override #'org-export-format-reference
  (if (stringp reference) reference (format "org%07x" reference)))

(defun unpackaged/org-element-descendant-of (type element)
  "Return non-nil if ELEMENT is a descendant of TYPE.
TYPE should be an element type, like `item' or `paragraph'.
ELEMENT should be a list like that returned by `org-element-context'."
  ;; MAYBE: Use `org-element-lineage'.
  (when-let* ((parent (org-element-property :parent element)))
    (or (eq type (car parent))
        (unpackaged/org-element-descendant-of type parent))))

;;;###autoload
(defun unpackaged/org-return-dwim (&optional default)
  "A helpful replacement for `org-return-indent'.  With prefix, call `org-return-indent'.

On headings, move point to position after entry content.  In
lists, insert a new item or end the list, with checkbox if
appropriate.  In tables, insert a new row or end the table."
  ;; Inspired by John Kitchin: http://kitchingroup.cheme.cmu.edu/blog/2017/04/09/A-better-return-in-org-mode/
  (interactive "P")
  (if default
      (org-return t)
    (cond
     ;; Act depending on context around point.

     ;; NOTE: I prefer RET to not follow links, but by uncommenting this block, links will be
     ;; followed.

     ;; ((eq 'link (car (org-element-context)))
     ;;  ;; Link: Open it.
     ;;  (org-open-at-point-global))

     ((org-at-heading-p)
      ;; Heading: Move to position after entry content.
      ;; NOTE: This is probably the most interesting feature of this function.
      (let ((heading-start (org-entry-beginning-position)))
        (goto-char (org-entry-end-position))
        (cond ((and (org-at-heading-p)
                    (= heading-start (org-entry-beginning-position)))
               ;; Entry ends on its heading; add newline after
               (end-of-line)
               (insert "\n\n"))
              (t
               ;; Entry ends after its heading; back up
               (forward-line -1)
               (end-of-line)
               (when (org-at-heading-p)
                 ;; At the same heading
                 (forward-line)
                 (insert "\n")
                 (forward-line -1))
               (while (not (looking-back "\\(?:[[:blank:]]?\n\\)\\{3\\}" nil))
                 (insert "\n"))
               (forward-line -1)))))

     ((org-at-item-checkbox-p)
      ;; Checkbox: Insert new item with checkbox.
      (org-insert-todo-heading nil))

     ((org-in-item-p)
      ;; Plain list.  Yes, this gets a little complicated...
      (let ((context (org-element-context)))
        (if (or (eq 'plain-list (car context))  ; First item in list
                (and (eq 'item (car context))
                     (not (eq (org-element-property :contents-begin context)
                              (org-element-property :contents-end context))))
                (unpackaged/org-element-descendant-of 'item context))  ; Element in list item, e.g. a link
            ;; Non-empty item: Add new item.
            (org-insert-item)
          ;; Empty item: Close the list.
          ;; TODO: Do this with org functions rather than operating on the text. Can't seem to find the right function.
          (delete-region (line-beginning-position) (line-end-position))
          (insert "\n"))))

     ((when (fboundp 'org-inlinetask-in-task-p)
        (org-inlinetask-in-task-p))
      ;; Inline task: Don't insert a new heading.
      (org-return t))

     ((org-at-table-p)
      (cond ((save-excursion
               (beginning-of-line)
               ;; See `org-table-next-field'.
               (cl-loop with end = (line-end-position)
                        for cell = (org-element-table-cell-parser)
                        always (equal (org-element-property :contents-begin cell)
                                      (org-element-property :contents-end cell))
                        while (re-search-forward "|" end t)))
             ;; Empty row: end the table.
             (delete-region (line-beginning-position) (line-end-position))
             (org-return t))
            (t
             ;; Non-empty row: call `org-return-indent'.
             (org-return t))))
     (t
      ;; All other cases: call `org-return-indent'.
      (org-return t)))))

(map!
 :after evil-org
 :map evil-org-mode-map
 :i [return] #'unpackaged/org-return-dwim)

(defun +yas/org-src-header-p ()
  "Determine whether `point' is within a src-block header or header-args."
  (pcase (org-element-type (org-element-context))
    ('src-block (< (point) ; before code part of the src-block
                   (save-excursion (goto-char (org-element-property :begin (org-element-context)))
                                   (forward-line 1)
                                   (point))))
    ('inline-src-block (< (point) ; before code part of the inline-src-block
                          (save-excursion (goto-char (org-element-property :begin (org-element-context)))
                                          (search-forward "]{")
                                          (point))))
    ('keyword (string-match-p "^header-args" (org-element-property :value (org-element-context))))))

(defun org-syntax-convert-keyword-case-to-lower ()
  "Convert all #+KEYWORDS to #+keywords."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0)
          (case-fold-search nil))
      (while (re-search-forward "^[ \t]*#\\+[A-Z_]+" nil t)
        (unless (s-matches-p "RESULTS" (match-string 0))
          (replace-match (downcase (match-string 0)) t)
          (setq count (1+ count))))
      (message "Replaced %d occurances" count))))

(org-link-set-parameters "xkcd"
                         :image-data-fun #'+org-xkcd-image-fn
                         :follow #'+org-xkcd-open-fn
                         :export #'+org-xkcd-export
                         :complete #'+org-xkcd-complete)

(defun +org-xkcd-open-fn (link)
  (+org-xkcd-image-fn nil link nil))

(defun +org-xkcd-image-fn (protocol link description)
  "Get image data for xkcd num LINK"
  (let* ((xkcd-info (+xkcd-fetch-info (string-to-number link)))
         (img (plist-get xkcd-info :img))
         (alt (plist-get xkcd-info :alt)))
    (message alt)
    (+org-image-file-data-fn protocol (xkcd-download img (string-to-number link)) description)))

(defun +org-xkcd-export (num desc backend _com)
  "Convert xkcd to html/LaTeX form"
  (let* ((xkcd-info (+xkcd-fetch-info (string-to-number num)))
         (img (plist-get xkcd-info :img))
         (alt (plist-get xkcd-info :alt))
         (title (plist-get xkcd-info :title))
         (file (xkcd-download img (string-to-number num))))
    (cond ((org-export-derived-backend-p backend 'html)
           (format "<img class='invertible' src='%s' title=\"%s\" alt='%s'>" img (subst-char-in-string ?\" ?“ alt) title))
          ((org-export-derived-backend-p backend 'latex)
           (format "\\begin{figure}[!htb]
  \\centering
  \\includegraphics[scale=0.4]{%s}%s
\\end{figure}" file (if (equal desc (format "xkcd:%s" num)) ""
                      (format "\n  \\caption*{\\label{xkcd:%s} %s}"
                              num
                              (or desc
                                  (format "\\textbf{%s} %s" title alt))))))
          (t (format "https://xkcd.com/%s" num)))))

(defun +org-xkcd-complete (&optional arg)
  "Complete xkcd using `+xkcd-stored-info'"
  (format "xkcd:%d" (+xkcd-select)))

(org-link-set-parameters "yt" :export #'+org-export-yt)
(defun +org-export-yt (path desc backend _com)
  (cond ((org-export-derived-backend-p backend 'html)
         (format "<iframe width='440' \
height='335' \
src='https://www.youtube.com/embed/%s' \
frameborder='0' \
allowfullscreen>%s</iframe>" path (or "" desc)))
        ((org-export-derived-backend-p backend 'latex)
         (format "\\href{https://youtu.be/%s}{%s}" path (or desc "youtube")))
        (t (format "https://youtu.be/%s" path))))

(defadvice! shut-up-org-problematic-hooks (orig-fn &rest args)
  :around #'org-fancy-priorities-mode
  (ignore-errors (apply orig-fn args)))

(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))
(setq org-refile-use-outline-path 'file)
(setq org-refile-active-region-within-subtree t)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes (quote confirm))

(define-minor-mode org-vid-minor-mode
   "Toggle video minor mode for video note taking in org-mode"
   :lighter " Video"
   :keymap
   `(
     (,(kbd "<up>")    . (lambda () (interactive) (mpv-speed-increase 1)))
     (,(kbd "<down>")  . (lambda () (interactive) (mpv-speed-decrease 1)))
     (,(kbd "<right>") . (lambda () (interactive) (mpv-seek-forward 1)))
     (,(kbd "<left>")  . (lambda () (interactive) (mpv-seek-backward 1)))
     (,(kbd "M-p")     . mpv-pause)
     (,(kbd "M-SPC")   . mpv-pause)
     (,(kbd "M-k")     . mpv-kill)
     (,(kbd "M--")     . (lambda () (interactive) (mpv-insert-playback-position t)))
     (,(kbd "M-s")     . (lambda () mpv-seek))
     (,(kbd "M-0")     . (lambda () (interactive) (mpv-speed-set 1)))
     (,(kbd "M-S")     . (lambda () (interactive) (mpv-seek-to-position-at-point)))
     ))

(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 50) '(100 . 100)))))
(global-set-key (kbd "C-c T") 'toggle-transparency)



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

(defun op/buffer-to-side-window (place)
  "Place the current buffer in the side window at PLACE."
  (interactive (list (intern
                      (completing-read "Which side: "
                                       '(top left right bottom)))))
  (let ((buf (current-buffer)))
    (display-buffer-in-side-window
     buf `((window-height . 0.15)
           (side . ,place)
           (slot . -1)
           (window-parameters . ((no-delete-other-windows . t)
                                 (no-other-window t)))))
    (delete-window)))


(defun create-scratch-buffer nil
  "create a new scratch buffer to work in. (could be *scratch* - *scratchX*)"
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (emacs-lisp-mode)
    ))

(defun split-window-right-and-move-there-dammit ()
  (interactive)
  (split-window-right)
  (windmove-right))



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



(use-package! key-chord
  :config
  ;; (key-chord-mode 1)
  (setq key-chord-one-key-delay 0.10 ; same key (e.g. xx) 20 delay
        key-chord-two-keys-delay 0.05))

  (key-chord-define-global ",." 'end-of-buffer)
  ;; FIXME: accidentally triggered too often
  (key-chord-define-global "zx" 'beginning-of-buffer)

  (key-chord-define-global "qw" 'delete-window)
  (key-chord-define-global "qp" 'delete-other-windows)
  (key-chord-define-global ",," 'doom/open-scratch-buffer)

  (key-chord-define-global "fk" 'other-window)
  (key-chord-define-global "jd" 'rev-other-window)
(key-chord-define-global "JJ" 'previous-buffer)
  (key-chord-define-global "KK" 'next-buffer)


  (key-chord-define-global "hh" 'helpful-at-point)
  (key-chord-define-global "hk" 'helpful-key)
  (key-chord-define-global "hv" 'helpful-variable)

  ;; no bueno: e.g. "pathfinder", "highfidelity"
  ;; (key-chord-define-global "hf" 'helpful-function)

  (key-chord-define-global "vn" 'split-window-vertically-and-switch)
  (key-chord-define-global "vm" 'split-window-vertically-and-switch) ; ergodox
  (key-chord-define-global "hj" 'split-window-horizontally-and-switch)

  (key-chord-define-global "jm" 'my/duplicate-line-or-region)
  (key-chord-define-global "fv" 'comment-line)

  (key-chord-define-global "kl" 'er/expand-region)

  (key-chord-define-global "xx" 'execute-extended-command)
  (key-chord-define-global "xf" 'find-file)

  (key-chord-define-global "jp" 'my/insert-jupyter-python-block)

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

(add-hook 'org-mode-hook #'+org-pretty-mode)

  (custom-set-faces!
    '(org-level-1 :inherit outline-1 :weight extra-bold :height 1.35)
    '(org-level-2 :inherit outline-2 :weight bold :height 1.25)
    '(org-level-3 :inherit outline-3 :weight bold :height 1.22)
    '(org-level-4 :inherit outline-4 :weight bold :height 1.19)
    '(org-level-5 :inherit outline-5 :weight semi-bold :height 1.16)
    '(org-level-6 :inherit outline-6 :weight semi-bold :height 1.13)
    '(org-level-7 :inherit outline-7 :weight semi-bold)
    '(org-level-8 :inherit outline-8 :weight semi-bold)
    ;; Ensure that anything that should be fixed-pitch in org buffers appears that
    ;; way
    '(org-block nil :foreground nil :inherit 'fixed-pitch)
    '(org-code nil   :inherit '(shadow fixed-pitch))
    '(org-table nil   :inherit '(shadow fixed-pitch))
    '(org-verbatim nil :inherit '(shadow fixed-pitch))
    '(org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    '(org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    '(org-checkbox nil :inherit 'fixed-pitch))

(custom-set-faces!
  '(org-document-title :height 1.2))

(setq org-agenda-deadline-faces
      '((1.001 . error)
        (1.0 . org-warning)
        (0.5 . org-upcoming-deadline)
        (0.0 . org-upcoming-distant-deadline)))

(setq org-fontify-quote-and-verse-blocks t)

(defun locally-defer-font-lock ()
  "Set jit-lock defer and stealth, when buffer is over a certain size."
  (when (> (buffer-size) 50000)
    (setq-local jit-lock-defer-time 0.05
                jit-lock-stealth-time 1)))

(add-hook 'org-mode-hook #'locally-defer-font-lock)

(defadvice! +org-indent--reduced-text-prefixes ()
  :after #'org-indent--compute-prefixes
  (setq org-indent--text-line-prefixes
        (make-vector org-indent--deepest-level nil))
  (when (> org-indent-indentation-per-level 0)
    (dotimes (n org-indent--deepest-level)
      (aset org-indent--text-line-prefixes
            n
            (org-add-props
                (concat (make-string (* n (1- org-indent-indentation-per-level))
                                     ?\s)
                        (if (> n 0)
                             (char-to-string org-indent-boundary-char)
                          "\u200b"))
                nil 'face 'org-indent)))))

(setq org-inline-src-prettify-results '("⟨" . "⟩"))

(setq doom-themes-org-fontify-special-tags nil)
(setq org-highlight-latex-and-related '(native script entities))
(require 'org-src)
(add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))

(use-package! org-fragtog
  :hook (org-mode . org-fragtog-mode))

(setq org-format-latex-header "\\documentclass{article}
\\usepackage[usenames]{xcolor}

\\usepackage[T1]{fontenc}

\\usepackage{booktabs}

\\pagestyle{empty}             % do not remove
% The settings below are copied from fullpage.sty
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-3cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}
\\addtolength{\\topmargin}{-2.54cm}
% my custom stuff
\\usepackage[nofont,plaindd]{bmc-maths}
\\usepackage{arev}
")

(setq org-format-latex-options
      (plist-put org-format-latex-options :background "Transparent"))
;; cause issues with emacs server
;; (add-hook! 'doom-load-theme-hook
;;   (setq org-preview-latex-image-directory
;;         (concat doom-cache-dir "org-latex/" (symbol-name doom-theme) "/"))
;;   (dolist (buffer (doom-buffers-in-mode 'org-mode (buffer-list)))
;;     (with-current-buffer buffer
;;       (+org--toggle-inline-images-in-subtree (point-min) (point-max) 'refresh)
;;       (org-clear-latex-preview (point-min) (point-max))
;;       (org--latex-preview-region (point-min) (point-max)))))

(defun scimax-org-latex-fragment-justify (justification)
  "Justify the latex fragment at point with JUSTIFICATION.
JUSTIFICATION is a symbol for 'left, 'center or 'right."
  (interactive
   (list (intern-soft
          (completing-read "Justification (left): " '(left center right)
                           nil t nil nil 'left))))
  (let* ((ov (ov-at))
         (beg (ov-beg ov))
         (end (ov-end ov))
         (shift (- beg (line-beginning-position)))
         (img (overlay-get ov 'display))
         (img (and (and img (consp img) (eq (car img) 'image)
                        (image-type-available-p (plist-get (cdr img) :type)))
                   img))
         space-left offset)
    (when (and img
               ;; This means the equation is at the start of the line
               (= beg (line-beginning-position))
               (or
                (string= "" (s-trim (buffer-substring end (line-end-position))))
                (eq 'latex-environment (car (org-element-context)))))
      (setq space-left (- (window-max-chars-per-line) (car (image-size img)))
            offset (floor (cond
                           ((eq justification 'center)
                            (- (/ space-left 2) shift))
                           ((eq justification 'right)
                            (- space-left shift))
                           (t
                            0))))
      (when (>= offset 0)
        (overlay-put ov 'before-string (make-string offset ?\ ))))))

(defun scimax-org-latex-fragment-justify-advice (beg end image imagetype)
  "After advice function to justify fragments."
  (scimax-org-latex-fragment-justify (or (plist-get org-format-latex-options :justify) 'left)))


(defun scimax-toggle-latex-fragment-justification ()
  "Toggle if LaTeX fragment justification options can be used."
  (interactive)
  (if (not (get 'scimax-org-latex-fragment-justify-advice 'enabled))
      (progn
        (advice-add 'org--format-latex-make-overlay :after 'scimax-org-latex-fragment-justify-advice)
        (put 'scimax-org-latex-fragment-justify-advice 'enabled t)
        (message "Latex fragment justification enabled"))
    (advice-remove 'org--format-latex-make-overlay 'scimax-org-latex-fragment-justify-advice)
    (put 'scimax-org-latex-fragment-justify-advice 'enabled nil)
    (message "Latex fragment justification disabled")))

;; Numbered equations all have (1) as the number for fragments with vanilla
;; org-mode. This code injects the correct numbers into the previews so they
;; look good.
(defun scimax-org-renumber-environment (orig-func &rest args)
  "A function to inject numbers in LaTeX fragment previews."
  (let ((results '())
        (counter -1)
        (numberp))
    (setq results (cl-loop for (begin . env) in
                           (org-element-map (org-element-parse-buffer) 'latex-environment
                             (lambda (env)
                               (cons
                                (org-element-property :begin env)
                                (org-element-property :value env))))
                           collect
                           (cond
                            ((and (string-match "\\\\begin{equation}" env)
                                  (not (string-match "\\\\tag{" env)))
                             (cl-incf counter)
                             (cons begin counter))
                            ((string-match "\\\\begin{align}" env)
                             (prog2
                                 (cl-incf counter)
                                 (cons begin counter)
                               (with-temp-buffer
                                 (insert env)
                                 (goto-char (point-min))
                                 ;; \\ is used for a new line. Each one leads to a number
                                 (cl-incf counter (count-matches "\\\\$"))
                                 ;; unless there are nonumbers.
                                 (goto-char (point-min))
                                 (cl-decf counter (count-matches "\\nonumber")))))
                            (t
                             (cons begin nil)))))

    (when (setq numberp (cdr (assoc (point) results)))
      (setf (car args)
            (concat
             (format "\\setcounter{equation}{%s}\n" numberp)
             (car args)))))

  (apply orig-func args))


(defun scimax-toggle-latex-equation-numbering ()
  "Toggle whether LaTeX fragments are numbered."
  (interactive)
  (if (not (get 'scimax-org-renumber-environment 'enabled))
      (progn
        (advice-add 'org-create-formula-image :around #'scimax-org-renumber-environment)
        (put 'scimax-org-renumber-environment 'enabled t)
        (message "Latex numbering enabled"))
    (advice-remove 'org-create-formula-image #'scimax-org-renumber-environment)
    (put 'scimax-org-renumber-environment 'enabled nil)
    (message "Latex numbering disabled.")))

(advice-add 'org-create-formula-image :around #'scimax-org-renumber-environment)
(put 'scimax-org-renumber-environment 'enabled t)

(setq org-ellipsis " ▾ "
      org-hide-leading-stars t
      org-priority-highest ?A
      org-priority-lowest ?E
      org-priority-faces
      '((?A . 'all-the-icons-red)
        (?B . 'all-the-icons-orange)
        (?C . 'all-the-icons-yellow)
        (?D . 'all-the-icons-green)
        (?E . 'all-the-icons-blue)))

(appendq! +ligatures-extra-symbols
          (list :list_property "∷"
                :em_dash       "—"
                :ellipses      "…"
                :arrow_right   "→"
                :arrow_left    "←"
                :arrow_lr      "↔"
                :properties    "⚙"
                :end           "∎"
                :priority_a    #("⚑" 0 1 (face all-the-icons-red))
                :priority_b    #("⬆" 0 1 (face all-the-icons-orange))
                :priority_c    #("■" 0 1 (face all-the-icons-yellow))
                :priority_d    #("⬇" 0 1 (face all-the-icons-green))
                :priority_e    #("❓" 0 1 (face all-the-icons-blue))))

(defadvice! +org-init-appearance-h--no-ligatures-a ()
  :after #'+org-init-appearance-h
  (set-ligatures! 'org-mode nil)
  (set-ligatures! 'org-mode
    :list_property "::"
    :em_dash       "---"
    :ellipsis      "..."
    :arrow_right   "->"
    :arrow_left    "<-"
    :arrow_lr      "<->"
    :properties    ":PROPERTIES:"
    :end           ":END:"
    :priority_a    "[#A]"
    :priority_b    "[#B]"
    :priority_c    "[#C]"
    :priority_d    "[#D]"
    :priority_e    "[#E]"))

(defvar +org-plot-term-size '(1050 . 650)
  "The size of the GNUPlot terminal, in the form (WIDTH . HEIGHT).")

(after! org-plot
  (defun +org-plot-generate-theme (_type)
    "Use the current Doom theme colours to generate a GnuPlot preamble."
    (format "
fgt = \"textcolor rgb '%s'\" # foreground text
fgat = \"textcolor rgb '%s'\" # foreground alt text
fgl = \"linecolor rgb '%s'\" # foreground line
fgal = \"linecolor rgb '%s'\" # foreground alt line

# foreground colors
set border lc rgb '%s'
# change text colors of  tics
set xtics @fgt
set ytics @fgt
# change text colors of labels
set title @fgt
set xlabel @fgt
set ylabel @fgt
# change a text color of key
set key @fgt

# line styles
set linetype 1 lw 2 lc rgb '%s' # red
set linetype 2 lw 2 lc rgb '%s' # blue
set linetype 3 lw 2 lc rgb '%s' # green
set linetype 4 lw 2 lc rgb '%s' # magenta
set linetype 5 lw 2 lc rgb '%s' # orange
set linetype 6 lw 2 lc rgb '%s' # yellow
set linetype 7 lw 2 lc rgb '%s' # teal
set linetype 8 lw 2 lc rgb '%s' # violet

# border styles
set tics out nomirror
set border 3

# palette
set palette maxcolors 8
set palette defined ( 0 '%s',\
1 '%s',\
2 '%s',\
3 '%s',\
4 '%s',\
5 '%s',\
6 '%s',\
7 '%s' )
"
            (doom-color 'fg)
            (doom-color 'fg-alt)
            (doom-color 'fg)
            (doom-color 'fg-alt)
            (doom-color 'fg)
            ;; colours
            (doom-color 'red)
            (doom-color 'blue)
            (doom-color 'green)
            (doom-color 'magenta)
            (doom-color 'orange)
            (doom-color 'yellow)
            (doom-color 'teal)
            (doom-color 'violet)
            ;; duplicated
            (doom-color 'red)
            (doom-color 'blue)
            (doom-color 'green)
            (doom-color 'magenta)
            (doom-color 'orange)
            (doom-color 'yellow)
            (doom-color 'teal)
            (doom-color 'violet)))

  (defun +org-plot-gnuplot-term-properties (_type)
    (format "background rgb '%s' size %s,%s"
            (doom-color 'bg) (car +org-plot-term-size) (cdr +org-plot-term-size)))

  (setq org-plot/gnuplot-script-preamble #'+org-plot-generate-theme)
  (setq org-plot/gnuplot-term-extra #'+org-plot-gnuplot-term-properties))

(define-minor-mode prot/scroll-center-cursor-mode
  "Toggle centred cursor scrolling behavior"
  :init-value nil
  :lighter " S="
  :global nil
  (if prot/scroll-center-cursor-mode
      (setq-local scroll-margin (* (frame-height) 2)
                  scroll-conservatively 0
                  maximum-scroll-margin 0.5)
    (dolist (local '(scroll-preserve-screen-position
                     scroll-conservatively
                     maximum-scroll-margin
                     scroll-margin))
      (kill-local-variable `,local)))
  )

(define-minor-mode prot/variable-pitch-mode
  "Toggle 'mixed-pitch-modei, except for programming modes"
  :init-value nil
  :global nil
  (if prot/variable-pitch-mode
      (unless (derived-mode-p 'prog-mode)
        (variable-pitch-mode 1))
    (variable-pitch-mode -1)))

(define-minor-mode prot/display-line-number-mode
  "Disable line numbers, except for programming modes."
  :init-value nil
  :global nil
  (if prot/display-line-number-mode
      (unless (derived-mode-p 'prog-mode)
        (display-line-numbers-mode -1))
    (display-line-numbers-mode 1)))

(setq org-export-headline-levels 5) ; I like nesting
(require 'ox-extra)
(ox-extras-activate '(ignore-headlines))

(setq org-export-creator-string
      (format "Emacs %s (Org mode %s–%s)" emacs-version (org-release) (org-git-version)))

(defun org-export-filter-text-acronym (text backend _info)
  "Wrap suspected acronyms in acronyms-specific formatting.
Treat sequences of 2+ capital letters (optionally succeeded by \"s\") as an acronym.
Ignore if preceeded by \";\" (for manual prevention) or \"\\\" (for LaTeX commands).

TODO abstract backend implementations."
  (let ((base-backend
         (cond
          ((org-export-derived-backend-p backend 'latex) 'latex)
          ;; Markdown is derived from HTML, but we don't want to format it
          ((org-export-derived-backend-p backend 'md) nil)
          ((org-export-derived-backend-p backend 'html) 'html)))
        (case-fold-search nil))
    (when base-backend
      (replace-regexp-in-string
       "[;\\\\]?\\b[A-Z][A-Z]+s?\\(?:[^A-Za-z]\\|\\b\\)"
       (lambda (all-caps-str)
         (cond ((equal (aref all-caps-str 0) ?\\) all-caps-str)                ; don't format LaTeX commands
               ((equal (aref all-caps-str 0) ?\;) (substring all-caps-str 1))  ; just remove not-acronym indicator char ";"
               (t (let* ((final-char (if (string-match-p "[^A-Za-z]" (substring all-caps-str -1 (length all-caps-str)))
                                         (substring all-caps-str -1 (length all-caps-str))
                                       nil)) ; needed to re-insert the [^A-Za-z] at the end
                         (trailing-s (equal (aref all-caps-str (- (length all-caps-str) (if final-char 2 1))) ?s))
                         (acr (if final-char
                                  (substring all-caps-str 0 (if trailing-s -2 -1))
                                (substring all-caps-str 0 (+ (if trailing-s -1 (length all-caps-str)))))))
                    (pcase base-backend
                      ('latex (concat "\\acr{" (s-downcase acr) "}" (when trailing-s "\\acrs{}") final-char))
                      ('html (concat "<span class='acr'>" acr "</span>" (when trailing-s "<small>s</small>") final-char)))))))
       text t t))))

(add-to-list 'org-export-filter-plain-text-functions
             #'org-export-filter-text-acronym)

;; We won't use `org-export-filter-headline-functions' because it
;; passes (and formats) the entire section contents. That's no good.

(defun org-html-format-headline-acronymised (todo todo-type priority text tags info)
  "Like `org-html-format-headline-default-function', but with acronym formatting."
  (org-html-format-headline-default-function
   todo todo-type priority (org-export-filter-text-acronym text 'html info) tags info))
(setq org-html-format-headline-function #'org-html-format-headline-acronymised)

(defun org-latex-format-headline-acronymised (todo todo-type priority text tags info)
  "Like `org-latex-format-headline-default-function', but with acronym formatting."
  (org-latex-format-headline-default-function
   todo todo-type priority (org-export-filter-text-acronym text 'latex info) tags info))
(setq org-latex-format-headline-function #'org-latex-format-headline-acronymised)


(defun +org-mode--fontlock-only-mode ()
  "Just apply org-mode's font-lock once."
  (let (org-mode-hook
        org-hide-leading-stars
        org-hide-emphasis-markers)
    (org-set-font-lock-defaults)
    (font-lock-ensure))
  (setq-local major-mode #'fundamental-mode))

(defun +org-export-babel-mask-org-config (_backend)
  "Use `+org-mode--fontlock-only-mode' instead of `org-mode'."
  (setq-local org-src-lang-modes
              (append org-src-lang-modes
                      (list (cons "org" #'+org-mode--fontlock-only)))))

(add-hook 'org-export-before-processing-hook #'+org-export-babel-mask-org-config)


;; [[file:lang.org::*Reveal export][Reveal export:1]]
(setq org-re-reveal-theme "white"
      org-re-reveal-transition "slide"
      org-re-reveal-plugins '(markdown notes math search zoom))
;; Reveal export:1 ends here

;; [[file:lang.org::*Babel][Babel:1]]
(add-transient-hook! #'org-babel-execute-src-block
  (require 'ob-async))

(defvar org-babel-auto-async-languages '()
  "Babel languages which should be executed asyncronously by default.")

(defadvice! org-babel-get-src-block-info-eager-async-a (orig-fn &optional light datum)
  "Eagarly add an :async parameter to the src information, unless it seems problematic.
This only acts o languages in `org-babel-auto-async-languages'.
Not added when either:
+ session is not \"none\"
+ :sync is set"
  :around #'org-babel-get-src-block-info
  (let ((result (funcall orig-fn light datum)))
    (when (and (string= "none" (cdr (assoc :session (caddr result))))
               (member (car result) org-babel-auto-async-languages)
               (not (assoc :async (caddr result))) ; don't duplicate
               (not (assoc :sync (caddr result))))
      (push '(:async) (caddr result)))
    result))
;; Babel:1 ends here

;; [[file:lang.org::*ESS][ESS:1]]
(setq ess-eval-visibly 'nowait)
;; ESS:1 ends here

;; [[file:lang.org::*ESS][ESS:2]]
(setq ess-R-font-lock-keywords
      '((ess-R-fl-keyword:keywords . t)
        (ess-R-fl-keyword:constants . t)
        (ess-R-fl-keyword:modifiers . t)
        (ess-R-fl-keyword:fun-defs . t)
        (ess-R-fl-keyword:assign-ops . t)
        (ess-R-fl-keyword:%op% . t)
        (ess-fl-keyword:fun-calls . t)
        (ess-fl-keyword:numbers . t)
        (ess-fl-keyword:operators . t)
        (ess-fl-keyword:delimiters . t)
        (ess-fl-keyword:= . t)
        (ess-R-fl-keyword:F&T . t)))
;; ESS:2 ends here

;; [[file:lang.org::*ESS][ESS:3]]
(after! org
  (add-to-list '+org-babel-mode-alist '(jags . ess-jags)))
;; ESS:3 ends here

;; [[file:lang.org::*ASCII export][ASCII export:1]]
(setq org-ascii-charset 'utf-8)
;; ASCII export:1 ends here

;; [[file:lang.org::*ASCII export][ASCII export:3]]
(when (executable-find "latex2text")
  (after! ox-ascii
    (defvar org-ascii-convert-latex t
      "Use latex2text to convert LaTeX elements to unicode.")

    (defadvice! org-ascii-latex-environment-unicode-a (latex-environment _contents info)
      "Transcode a LATEX-ENVIRONMENT element from Org to ASCII, converting to unicode.
CONTENTS is nil.  INFO is a plist holding contextual
information."
      :override #'org-ascii-latex-environment
      (when (plist-get info :with-latex)
        (org-ascii--justify-element
         (org-remove-indentation
          (let* ((latex (org-element-property :value latex-environment))
                 (unicode (and (eq (plist-get info :ascii-charset) 'utf-8)
                               org-ascii-convert-latex
                               (doom-call-process "latex2text" "-q" "--code" latex))))
            (if (= (car unicode) 0) ; utf-8 set, and sucessfully ran latex2text
                (cdr unicode) latex)))
         latex-environment info)))

    (defadvice! org-ascii-latex-fragment-unicode-a (latex-fragment _contents info)
      "Transcode a LATEX-FRAGMENT object from Org to ASCII, converting to unicode.
CONTENTS is nil.  INFO is a plist holding contextual
information."
      :override #'org-ascii-latex-fragment
      (when (plist-get info :with-latex)
        (let* ((latex (org-element-property :value latex-fragment))
               (unicode (and (eq (plist-get info :ascii-charset) 'utf-8)
                             org-ascii-convert-latex
                             (doom-call-process "latex2text" "-q" "--code" latex))))
          (if (and unicode (= (car unicode) 0)) ; utf-8 set, and sucessfully ran latex2text
              (cdr unicode) latex))))))
;; ASCII export:3 ends here

;; [[file:lang.org::*GFM][GFM:2]]
(use-package! ox-gfm
  :after ox)
;; GFM:2 ends here

;; [[file:lang.org::*Character substitutions][Character substitutions:1]]
(defadvice! org-md-plain-text-unicode-a (orig-fn text info)
  "Locally rebind `org-html-special-string-regexps'"
  :around #'org-md-plain-text
  (let ((org-html-special-string-regexps
         '(("\\\\-" . "-")
           ("---\\([^-]\\|$\\)" . "—\\1")
           ("--\\([^-]\\|$\\)" . "–\\1")
           ("\\.\\.\\." . "…")
           ("<->" . "⟷")
           ("->" . "→")
           ("<-" . "←"))))
    (funcall orig-fn text (plist-put info :with-smart-quotes nil))))
;; Character substitutions:1 ends here

;; [[file:lang.org::*Character substitutions][Character substitutions:2]]
(after! ox-md
  (defun org-md-latex-fragment (latex-fragment _contents info)
    "Transcode a LATEX-FRAGMENT object from Org to Markdown."
    (let ((frag (org-element-property :value latex-fragment)))
      (cond
       ((string-match-p "^\\\\(" frag)
        (concat "$" (substring frag 2 -2) "$"))
       ((string-match-p "^\\\\\\[" frag)
        (concat "$$" (substring frag 2 -2) "$$"))
       (t (message "unrecognised fragment: %s" frag)
          frag))))

  (defun org-md-latex-environment (latex-environment contents info)
    "Transcode a LATEX-ENVIRONMENT object from Org to Markdown."
    (concat "$$\n"
            (org-html-latex-environment latex-environment contents info)
            "$$\n"))

  (defun org-utf8-entity (entity _contents _info)
    "Transcode an ENTITY object from Org to utf-8.
CONTENTS are the definition itself.  INFO is a plist holding
contextual information."
    (org-element-property :utf-8 entity))

  ;; We can't let this be immediately parsed and evaluated,
  ;; because eager macro-expansion tries to call as-of-yet
  ;; undefined functions.
  ;; NOTE in the near future this shouldn't be required
  (eval
   '(dolist (extra-transcoder
             '((latex-fragment . org-md-latex-fragment)
               (latex-environment . org-md-latex-environment)
               (entity . org-utf8-entity)))
      (unless (member extra-transcoder (org-export-backend-transcoders
                                        (org-export-get-backend 'md)))
        (push extra-transcoder (org-export-backend-transcoders
                                (org-export-get-backend 'md)))))))
;; Character substitutions:2 ends here

;; [[file:lang.org::*Org-journal][Org-journal:1]]
(use-package org-journal
      ;; :defer t
      :bind
      ("C-c n j" . org-journal-new-entry)
      :custom
      (org-journal-dir "~/org/journal")
      (org-journal-date-prefix "#+TITLE: ")
      (org-journal-time-prefix "** ")
      (org-journal-file-format "%Y-%m-%d.org")
      (org-journal-date-format "%A, %d %B %Y"))
(setq org-journal-enable-agenda-integration t)
;; Org-journal:1 ends here

;; [[file:lang.org::*Org-publish][Org-publish:1]]
(setq org-publish-use-timestamps-flag nil)
(setq org-export-with-broken-links t)
(setq org-publish-project-alist
      '(("my.site"
         :base-directory "~/org/mysite/"
         :base-extension "org"
         :publishing-directory "~/org/mysite/html/"
         :recursive t
         :exclude "org-html-themes/.*"
         :with-author nil           ;; Don't include author name
         :with-author nil           ;; Don't include author name
         :with-creator t            ;; Include Emacs and Org versions in footer
         :with-toc t                ;; Include a table of contents
         :section-numbers nil       ;; Don't include section numbers
         :time-stamp-file nil   ;; Don't include time stamp in file
         :publishing-function org-html-publish-to-html
         :headline-levels 4             ; Just the default for this project.
         :auto-preamble t)
         ("org-static"
         :base-directory "~/org/website"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/public_html/"
         :recursive t
         :exclude ".*/org-html-themes/.*"
         :publishing-function org-publish-attachment)
      ))
;; Generate the site output
;; (org-publish-all t)

;; (message "Build complete!")
;; Org-publish:1 ends here

;; [[file:lang.org::*improving the html output-file][improving the html output-file:1]]
;; Customize the HTML output
(setq org-html-validation-link nil            ;; Don't show validation link
      org-html-head-include-scripts nil       ;; Use our own scripts
      org-html-head-include-default-style nil ;; Use our own styles
      org-html-head "<link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\" />")
;; improving the html output-file:1 ends here

;; [[file:lang.org::*Org-appt settings][Org-appt settings:1]]
(require 'appt)

(setq-default appt-display-mode-line t)
(appt-activate 1)
(org-agenda-to-appt 1)
(appt-check 1)
(setq appt-message-warning-time 60)
(setq appt-display-interval 900)
;; Org-appt settings:1 ends here

;; [[file:lang.org::*Compilation][Compilation:1]]
(setq TeX-save-query nil
      TeX-show-compilation t
      TeX-command-extra-options "-shell-escape")
(after! latex
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t)))
;; Compilation:1 ends here

;; [[file:lang.org::*Compilation][Compilation:2]]
(setq +latex-viewers '(pdf-tools sioyek evince zathura okular skim sumatrapdf))
;; Compilation:2 ends here

;; [[file:lang.org::*Template][Template:2]]
(setq tec/yas-latex-template-preamble "
\\usepackage[pdfa,unicode=true,hidelinks]{hyperref}

\\usepackage[dvipsnames,svgnames,table,hyperref]{xcolor}
\\renewcommand{\\UrlFont}{\\ttfamily\\small}

\\usepackage[a-2b]{pdfx} % why not be archival

\\usepackage[T1]{fontenc}
\\usepackage[osf]{newpxtext}  % Palatino
\\usepackage{gillius}
\\usepackage[scale=0.9]{sourcecodepro}

\\usepackage[varbb]{newpxmath}
\\usepackage{mathtools}
\\usepackage{amssymb}

\\usepackage[activate={true,nocompatibility},final,tracking=true,kerning=true,spacing=true,factor=2000]{microtype}
% microtype makes text look nicer

\\usepackage{graphicx} % include graphics

\\usepackage{booktabs} % nice table rules
")

(defun tec/yas-latex-get-class-choice ()
  "Prompt user for LaTeX class choice"
  (setq tec/yas-latex-class-choice (completing-read "Select document class: " '("article" "scrartcl" "bmc"))))

(defun tec/yas-latex-preamble-if ()
  "Based on class choice prompt for insertion of default preamble"
  (if (equal tec/yas-latex-class-choice "bmc") 'nil
    (eq (read-char-choice "Include default preamble? [Type y/n]" '(?y ?n)) ?y)))
;; Template:2 ends here

;; [[file:lang.org::*Deliminators][Deliminators:1]]
(after! tex
  (defvar tec/tex-last-delim-char nil
    "Last open delim expanded in a tex document")
  (defvar tec/tex-delim-dot-second t
    "When the `tec/tex-last-delim-char' is . a second character (this) is prompted for")
  (defun tec/get-open-delim-char ()
    "Exclusivly read next char to tec/tex-last-delim-char"
    (setq tec/tex-delim-dot-second nil)
    (setq tec/tex-last-delim-char (read-char-exclusive "Opening deliminator, recognises: 9 ( [ { < | ."))
    (when (eql ?. tec/tex-last-delim-char)
      (setq tec/tex-delim-dot-second (read-char-exclusive "Other deliminator, recognises: 0 9 (  ) [ ] { } < > |"))))
  (defun tec/tex-open-delim-from-char (&optional open-char)
    "Find the associated opening delim as string"
    (unless open-char (setq open-char (if (eql ?. tec/tex-last-delim-char)
                                          tec/tex-delim-dot-second
                                        tec/tex-last-delim-char)))
    (pcase open-char
      (?\( "(")
      (?9  "(")
      (?\[ "[")
      (?\{ "\\{")
      (?<  "<")
      (?|  (if tec/tex-delim-dot-second "." "|"))
      (_   ".")))
  (defun tec/tex-close-delim-from-char (&optional open-char)
    "Find the associated closing delim as string"
    (if tec/tex-delim-dot-second
        (pcase tec/tex-delim-dot-second
          (?\) ")")
          (?0  ")")
          (?\] "]")
          (?\} "\\}")
          (?\> ">")
          (?|  "|")
          (_   "."))
      (pcase (or open-char tec/tex-last-delim-char)
        (?\( ")")
        (?9  ")")
        (?\[ "]")
        (?\{ "\\}")
        (?<  ">")
        (?\) ")")
        (?0  ")")
        (?\] "]")
        (?\} "\\}")
        (?\> ">")
        (?|  "|")
        (_   "."))))
  (defun tec/tex-next-char-smart-close-delim (&optional open-char)
    (and (bound-and-true-p smartparens-mode)
         (eql (char-after) (pcase (or open-char tec/tex-last-delim-char)
                             (?\( ?\))
                             (?\[ ?\])
                             (?{ ?})
                             (?< ?>)))))
  (defun tec/tex-delim-yas-expand (&optional open-char)
    (yas-expand-snippet (yas-lookup-snippet "_deliminators" 'latex-mode) (point) (+ (point) (if (tec/tex-next-char-smart-close-delim open-char) 2 1)))))
;; Deliminators:1 ends here

;; [[file:lang.org::*Editor visuals][Editor visuals:1]]
(after! latex
  (setcar (assoc "⋆" LaTeX-fold-math-spec-list) "★")) ;; make \star bigger

(setq TeX-fold-math-spec-list
      `(;; missing/better symbols
        ("≤" ("le"))
        ("≥" ("ge"))
        ("≠" ("ne"))
        ;; convenience shorts -- these don't work nicely ATM
        ;; ("‹" ("left"))
        ;; ("›" ("right"))
        ;; private macros
        ("ℝ" ("RR"))
        ("ℕ" ("NN"))
        ("ℤ" ("ZZ"))
        ("ℚ" ("QQ"))
        ("ℂ" ("CC"))
        ("ℙ" ("PP"))
        ("ℍ" ("HH"))
        ("𝔼" ("EE"))
        ("𝑑" ("dd"))
        ;; known commands
        ("" ("phantom"))
        (,(lambda (num den) (if (and (TeX-string-single-token-p num) (TeX-string-single-token-p den))
                                (concat num "／" den)
                              (concat "❪" num "／" den "❫"))) ("frac"))
        (,(lambda (arg) (concat "√" (TeX-fold-parenthesize-as-necessary arg))) ("sqrt"))
        (,(lambda (arg) (concat "⭡" (TeX-fold-parenthesize-as-necessary arg))) ("vec"))
        ("‘{1}’" ("text"))
        ;; private commands
        ("|{1}|" ("abs"))
        ("‖{1}‖" ("norm"))
        ("⌊{1}⌋" ("floor"))
        ("⌈{1}⌉" ("ceil"))
        ("⌊{1}⌉" ("round"))
        ("𝑑{1}/𝑑{2}" ("dv"))
        ("∂{1}/∂{2}" ("pdv"))
        ;; fancification
        ("{1}" ("mathrm"))
        (,(lambda (word) (string-offset-roman-chars 119743 word)) ("mathbf"))
        (,(lambda (word) (string-offset-roman-chars 119951 word)) ("mathcal"))
        (,(lambda (word) (string-offset-roman-chars 120003 word)) ("mathfrak"))
        (,(lambda (word) (string-offset-roman-chars 120055 word)) ("mathbb"))
        (,(lambda (word) (string-offset-roman-chars 120159 word)) ("mathsf"))
        (,(lambda (word) (string-offset-roman-chars 120367 word)) ("mathtt"))
        )
      TeX-fold-macro-spec-list
      '(
        ;; as the defaults
        ("[f]" ("footnote" "marginpar"))
        ("[c]" ("cite"))
        ("[l]" ("label"))
        ("[r]" ("ref" "pageref" "eqref"))
        ("[i]" ("index" "glossary"))
        ("..." ("dots"))
        ("{1}" ("emph" "textit" "textsl" "textmd" "textrm" "textsf" "texttt"
                "textbf" "textsc" "textup"))
        ;; tweaked defaults
        ("©" ("copyright"))
        ("®" ("textregistered"))
        ("™"  ("texttrademark"))
        ("[1]:||►" ("item"))
        ("❡❡ {1}" ("part" "part*"))
        ("❡ {1}" ("chapter" "chapter*"))
        ("§ {1}" ("section" "section*"))
        ("§§ {1}" ("subsection" "subsection*"))
        ("§§§ {1}" ("subsubsection" "subsubsection*"))
        ("¶ {1}" ("paragraph" "paragraph*"))
        ("¶¶ {1}" ("subparagraph" "subparagraph*"))
        ;; extra
        ("⬖ {1}" ("begin"))
        ("⬗ {1}" ("end"))
        ))

(defun string-offset-roman-chars (offset word)
  "Shift the codepoint of each character in WORD by OFFSET with an extra -6 shift if the letter is lowercase"
  (apply 'string
         (mapcar (lambda (c)
                   (string-offset-apply-roman-char-exceptions
                    (+ (if (>= c 97) (- c 6) c) offset)))
                 word)))

(defvar string-offset-roman-char-exceptions
  '(;; lowercase serif
    (119892 .  8462) ; ℎ
    ;; lowercase caligraphic
    (119994 . 8495) ; ℯ
    (119996 . 8458) ; ℊ
    (120004 . 8500) ; ℴ
    ;; caligraphic
    (119965 . 8492) ; ℬ
    (119968 . 8496) ; ℰ
    (119969 . 8497) ; ℱ
    (119971 . 8459) ; ℋ
    (119972 . 8464) ; ℐ
    (119975 . 8466) ; ℒ
    (119976 . 8499) ; ℳ
    (119981 . 8475) ; ℛ
    ;; fraktur
    (120070 . 8493) ; ℭ
    (120075 . 8460) ; ℌ
    (120076 . 8465) ; ℑ
    (120085 . 8476) ; ℜ
    (120092 . 8488) ; ℨ
    ;; blackboard
    (120122 . 8450) ; ℂ
    (120127 . 8461) ; ℍ
    (120133 . 8469) ; ℕ
    (120135 . 8473) ; ℙ
    (120136 . 8474) ; ℚ
    (120137 . 8477) ; ℝ
    (120145 . 8484) ; ℤ
    )
  "An alist of deceptive codepoints, and then where the glyph actually resides.")

(defun string-offset-apply-roman-char-exceptions (char)
  "Sometimes the codepoint doesn't contain the char you expect.
Such special cases should be remapped to another value, as given in `string-offset-roman-char-exceptions'."
  (if (assoc char string-offset-roman-char-exceptions)
      (cdr (assoc char string-offset-roman-char-exceptions))
    char))

(defun TeX-fold-parenthesize-as-necessary (tokens &optional suppress-left suppress-right)
  "Add ❪ ❫ parenthesis as if multiple LaTeX tokens appear to be present"
  (if (TeX-string-single-token-p tokens) tokens
    (concat (if suppress-left "" "❪")
            tokens
            (if suppress-right "" "❫"))))

(defun TeX-string-single-token-p (teststring)
  "Return t if TESTSTRING appears to be a single token, nil otherwise"
  (if (string-match-p "^\\\\?\\w+$" teststring) t nil))
;; Editor visuals:1 ends here

;; [[file:lang.org::*Editor visuals][Editor visuals:2]]
(after! tex
  (map!
   :map LaTeX-mode-map
   :ei [C-return] #'LaTeX-insert-item)
  (setq TeX-electric-math '("\\(" . "")))
;; Editor visuals:2 ends here

;; [[file:lang.org::*Editor visuals][Editor visuals:3]]
;; Making \( \) less visible
(defface unimportant-latex-face
  '((t :inherit font-lock-comment-face :weight extra-light))
  "Face used to make \\(\\), \\[\\] less visible."
  :group 'LaTeX-math)

(font-lock-add-keywords
 'latex-mode
 `(("\\\\[]()[]" 0 'unimportant-latex-face prepend))
 'end)

;; (font-lock-add-keywords
;;  'latex-mode
;;  '(("\\\\[[:word:]]+" 0 'font-lock-keyword-face prepend))
;;  'end)
;; Editor visuals:3 ends here

;; [[file:lang.org::*Editor visuals][Editor visuals:4]]
(setq preview-LaTeX-command '("%`%l \"\\nonstopmode\\nofiles\
\\PassOptionsToPackage{" ("," . preview-required-option-list) "}{preview}\
\\AtBeginDocument{\\ifx\\ifPreview\\undefined"
preview-default-preamble "\\fi}\"%' \"\\detokenize{\" %t \"}\""))
;; Editor visuals:4 ends here

;; [[file:lang.org::*CDLaTeX][CDLaTeX:1]]
(after! cdlatex
  (setq cdlatex-env-alist
        '(("bmatrix" "\\begin{bmatrix}\n?\n\\end{bmatrix}" nil)
          ("equation*" "\\begin{equation*}\n?\n\\end{equation*}" nil)))
  (setq ;; cdlatex-math-symbol-prefix ?\; ;; doesn't work at the moment :(
   cdlatex-math-symbol-alist
   '( ;; adding missing functions to 3rd level symbols
     (?_    ("\\downarrow"  ""           "\\inf"))
     (?2    ("^2"           "\\sqrt{?}"     ""     ))
     (?3    ("^3"           "\\sqrt[3]{?}"  ""     ))
     (?^    ("\\uparrow"    ""           "\\sup"))
     (?k    ("\\kappa"      ""           "\\ker"))
     (?m    ("\\mu"         ""           "\\lim"))
     (?c    (""             "\\circ"     "\\cos"))
     (?d    ("\\delta"      "\\partial"  "\\dim"))
     (?D    ("\\Delta"      "\\nabla"    "\\deg"))
     ;; no idea why \Phi isnt on 'F' in first place, \phi is on 'f'.
     (?F    ("\\Phi"))
     ;; now just convenience
     (?.    ("\\cdot" "\\dots"))
     (?:    ("\\vdots" "\\ddots"))
     (?*    ("\\times" "\\star" "\\ast")))
   cdlatex-math-modify-alist
   '( ;; my own stuff
     (?B    "\\mathbb"        nil          t    nil  nil)
     (?a    "\\abs"           nil          t    nil  nil))))
;; CDLaTeX:1 ends here

;; [[file:lang.org::*LAAS][LAAS:2]]
(use-package! laas
  :hook (LaTeX-mode . laas-mode)
  :config
  (defun laas-tex-fold-maybe ()
    (unless (equal "/" aas-transient-snippet-key)
      (+latex-fold-last-macro-a)))
  (add-hook 'aas-post-snippet-expand-hook #'laas-tex-fold-maybe))
;; LAAS:2 ends here

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

;; [[file:lang.org::*latex][latex:1]]
(with-eval-after-load 'ox-latex
(add-to-list 'org-latex-classes
             '("org-plain-latex"
               "\\documentclass{article}
           [NO-DEFAULT-PACKAGES]
           [PACKAGES]
           [EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
;; latex:1 ends here

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


(setq emojify-emoji-set "twemoji-v2")

(defvar emojify-disabled-emojis
  '(;; Org
    "◼" "☑" "☸" "⚙" "⏩" "⏪" "⬆" "⬇" "❓" "↔"
    ;; Terminal powerline
    "✔"
    ;; Box drawing
    "▶" "◀"
    ;; I just want to see this as text
    "©" "™")
  "Characters that should never be affected by `emojify-mode'.")

(defadvice! emojify-delete-from-data ()
  "Ensure `emojify-disabled-emojis' don't appear in `emojify-emojis'."
  :after #'emojify-set-emoji-data
  (dolist (emoji emojify-disabled-emojis)
    (remhash emoji emojify-emojis)))

(defun emojify--replace-text-with-emoji (orig-fn emoji text buffer start end &optional target)
  "Modify `emojify--propertize-text-for-emoji' to replace ascii/github emoticons with unicode emojis, on the fly."
  (if (or (not emoticon-to-emoji) (= 1 (length text)))
      (funcall orig-fn emoji text buffer start end target)
    (delete-region start end)
    (insert (ht-get emoji "unicode"))))

(define-minor-mode emoticon-to-emoji
  "Write ascii/gh emojis, and have them converted to unicode live."
  :global nil
  :init-value nil
  (if emoticon-to-emoji
      (progn
        (setq-local emojify-emoji-styles '(ascii github unicode))
        (advice-add 'emojify--propertize-text-for-emoji :around #'emojify--replace-text-with-emoji)
        (unless emojify-mode
          (emojify-turn-on-emojify-mode)))
    (setq-local emojify-emoji-styles (default-value 'emojify-emoji-styles))
    (advice-remove 'emojify--propertize-text-for-emoji #'emojify--replace-text-with-emoji)))

(add-hook! '(mu4e-compose-mode org-msg-edit-mode circe-channel-mode) (emoticon-to-emoji 1))





(use-package! gif-screencast
  :commands gif-screencast-mode
  :config
  (map! :map gif-screencast-mode-map
        :g "<f8>" #'gif-screencast-toggle-pause
        :g "<f9>" #'gif-screencast-stop)
  (setq gif-screencast-program "maim"
        gif-screencast-args `("--quality" "3" "-i" ,(string-trim-right
                                                     (shell-command-to-string
                                                      "xdotool getactivewindow")))
        gif-screencast-optimize-args '("--batch" "--optimize=3" "--usecolormap=/tmp/doom-color-theme"))
  (defun gif-screencast-write-colormap ()
    (f-write-text
     (replace-regexp-in-string
      "\n+" "\n"
      (mapconcat (lambda (c) (if (listp (cdr c))
                                 (cadr c))) doom-themes--colors "\n"))
     'utf-8
     "/tmp/doom-color-theme" ))
  (gif-screencast-write-colormap)
  (add-hook 'doom-load-theme-hook #'gif-screencast-write-colormap))

;; (define-globalized-minor-mode global-rainbow-mode rainbow-mode
;;   (lambda () (rainbow-mode 1)))
;; (global-rainbow-mode 1 )
 ;; (defun unicode-fonts-setup-h (frame)
 ;;    "Run unicode-fonts-setup, then remove the hook."
 ;;    (progn
 ;;      (select-frame frame)
 ;;      (unicode-fonts-setup)
 ;;      (message "Removing unicode-fonts-setup to after-make-frame-functions hook")
 ;;      (remove-hook 'after-make-frame-functions 'unicode-fonts-setup-h)
 ;;      ))

 ;;  (add-hook 'after-make-frame-functions 'unicode-fonts-setup-h nil)




;; ;; [[file:lang.org::*HTML Export][HTML Export:1]]
;; (define-minor-mode org-fancy-html-export-mode
;;   "Toggle my fabulous org export tweaks. While this mode itself does a little bit,
;; the vast majority of the change in behaviour comes from switch statements in:
;;  - `org-html-template-fancier'
;;  - `org-html--build-meta-info-extended'
;;  - `org-html-src-block-collapsable'
;;  - `org-html-block-collapsable'
;;  - `org-html-table-wrapped'
;;  - `org-html--format-toc-headline-colapseable'
;;  - `org-html--toc-text-stripped-leaves'
;;  - `org-export-html-headline-anchor'"
;;   :global t
;;   :init-value t
;;   (if org-fancy-html-export-mode
;;       (setq org-html-style-default org-html-style-fancy
;;             org-html-meta-tags #'org-html-meta-tags-fancy
;;             org-html-checkbox-type 'html-span)
;;     (setq org-html-style-default org-html-style-plain
;;           org-html-meta-tags #'org-html-meta-tags-default
;;           org-html-checkbox-type 'html)))
;; ;; HTML Export:1 ends here

;; ;; [[file:lang.org::*Extra header content][Extra header content:1]]
;; (defadvice! org-html-template-fancier (orig-fn contents info)
;;   "Return complete document string after HTML conversion.
;; CONTENTS is the transcoded contents string.  INFO is a plist
;; holding export options. Adds a few extra things to the body
;; compared to the default implementation."
;;   :around #'org-html-template
;;   (if (or (not org-fancy-html-export-mode) (bound-and-true-p org-msg-export-in-progress))
;;       (funcall orig-fn contents info)
;;     (concat
;;      (when (and (not (org-html-html5-p info)) (org-html-xhtml-p info))
;;        (let* ((xml-declaration (plist-get info :html-xml-declaration))
;;               (decl (or (and (stringp xml-declaration) xml-declaration)
;;                         (cdr (assoc (plist-get info :html-extension)
;;                                     xml-declaration))
;;                         (cdr (assoc "html" xml-declaration))
;;                         "")))
;;          (when (not (or (not decl) (string= "" decl)))
;;            (format "%s\n"
;;                    (format decl
;;                            (or (and org-html-coding-system
;;                                     (fboundp 'coding-system-get)
;;                                     (coding-system-get org-html-coding-system 'mime-charset))
;;                                "iso-8859-1"))))))
;;      (org-html-doctype info)
;;      "\n"
;;      (concat "<html"
;;              (cond ((org-html-xhtml-p info)
;;                     (format
;;                      " xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\""
;;                      (plist-get info :language) (plist-get info :language)))
;;                    ((org-html-html5-p info)
;;                     (format " lang=\"%s\"" (plist-get info :language))))
;;              ">\n")
;;      "<head>\n"
;;      (org-html--build-meta-info info)
;;      (org-html--build-head info)
;;      (org-html--build-mathjax-config info)
;;      "</head>\n"
;;      "<body>\n<input type='checkbox' id='theme-switch'><div id='page'><label id='switch-label' for='theme-switch'></label>"
;;      (let ((link-up (org-trim (plist-get info :html-link-up)))
;;            (link-home (org-trim (plist-get info :html-link-home))))
;;        (unless (and (string= link-up "") (string= link-home ""))
;;          (format (plist-get info :html-home/up-format)
;;                  (or link-up link-home)
;;                  (or link-home link-up))))
;;      ;; Preamble.
;;      (org-html--build-pre/postamble 'preamble info)
;;      ;; Document contents.
;;      (let ((div (assq 'content (plist-get info :html-divs))))
;;        (format "<%s id=\"%s\">\n" (nth 1 div) (nth 2 div)))
;;      ;; Document title.
;;      (when (plist-get info :with-title)
;;        (let ((title (and (plist-get info :with-title)
;;                          (plist-get info :title)))
;;              (subtitle (plist-get info :subtitle))
;;              (html5-fancy (org-html--html5-fancy-p info)))
;;          (when title
;;            (format
;;             (if html5-fancy
;;                 "<header class=\"page-header\">%s\n<h1 class=\"title\">%s</h1>\n%s</header>"
;;               "<h1 class=\"title\">%s%s</h1>\n")
;;             (if (or (plist-get info :with-date)
;;                     (plist-get info :with-author))
;;                 (concat "<div class=\"page-meta\">"
;;                         (when (plist-get info :with-date)
;;                           (org-export-data (plist-get info :date) info))
;;                         (when (and (plist-get info :with-date) (plist-get info :with-author)) ", ")
;;                         (when (plist-get info :with-author)
;;                           (org-export-data (plist-get info :author) info))
;;                         "</div>\n")
;;               "")
;;             (org-export-data title info)
;;             (if subtitle
;;                 (format
;;                  (if html5-fancy
;;                      "<p class=\"subtitle\" role=\"doc-subtitle\">%s</p>\n"
;;                    (concat "\n" (org-html-close-tag "br" nil info) "\n"
;;                            "<span class=\"subtitle\">%s</span>\n"))
;;                  (org-export-data subtitle info))
;;               "")))))
;;      contents
;;      (format "</%s>\n" (nth 1 (assq 'content (plist-get info :html-divs))))
;;      ;; Postamble.
;;      (org-html--build-pre/postamble 'postamble info)
;;      ;; Possibly use the Klipse library live code blocks.
;;      (when (plist-get info :html-klipsify-src)
;;        (concat "<script>" (plist-get info :html-klipse-selection-script)
;;                "</script><script src=\""
;;                org-html-klipse-js
;;                "\"></script><link rel=\"stylesheet\" type=\"text/css\" href=\""
;;                org-html-klipse-css "\"/>"))
;;      ;; Closing document.
;;      "</div>\n</body>\n</html>")))
;; ;; Extra header content:1 ends here

;; ;; [[file:lang.org::*Extra header content][Extra header content:2]]
;; (defadvice! org-html-toc-linked (depth info &optional scope)
;;   "Build a table of contents.

;; Just like `org-html-toc', except the header is a link to \"#\".

;; DEPTH is an integer specifying the depth of the table.  INFO is
;; a plist used as a communication channel.  Optional argument SCOPE
;; is an element defining the scope of the table.  Return the table
;; of contents as a string, or nil if it is empty."
;;   :override #'org-html-toc
;;   (let ((toc-entries
;;          (mapcar (lambda (headline)
;;                    (cons (org-html--format-toc-headline headline info)
;;                          (org-export-get-relative-level headline info)))
;;                  (org-export-collect-headlines info depth scope))))
;;     (when toc-entries
;;       (let ((toc (concat "<div id=\"text-table-of-contents\">"
;;                          (org-html--toc-text toc-entries)
;;                          "</div>\n")))
;;         (if scope toc
;;           (let ((outer-tag (if (org-html--html5-fancy-p info)
;;                                "nav"
;;                              "div")))
;;             (concat (format "<%s id=\"table-of-contents\">\n" outer-tag)
;;                     (let ((top-level (plist-get info :html-toplevel-hlevel)))
;;                       (format "<h%d><a href=\"#\" style=\"color:inherit; text-decoration: none;\">%s</a></h%d>\n"
;;                               top-level
;;                               (org-html--translate "Table of Contents" info)
;;                               top-level))
;;                     toc
;;                     (format "</%s>\n" outer-tag))))))))
;; ;; Extra header content:2 ends here

;; ;; [[file:lang.org::*Extra header content][Extra header content:3]]
;; (defvar org-html-meta-tags-opengraph-image
;;   '(:image "https://tecosaur.com/resources/org/nib.png"
;;     :type "image/png"
;;     :width "200"
;;     :height "200"
;;     :alt "Green fountain pen nib")
;;   "Plist of og:image:PROP properties and their value, for use in `org-html-meta-tags-fancy'.")

;; (defun org-html-meta-tags-fancy (info)
;;   "Use the INFO plist to construct the meta tags, as described in `org-html-meta-tags'."
;;   (let ((title (org-html-plain-text
;;                 (org-element-interpret-data (plist-get info :title)) info))
;;         (author (and (plist-get info :with-author)
;;                      (let ((auth (plist-get info :author)))
;;                        ;; Return raw Org syntax.
;;                        (and auth (org-html-plain-text
;;                                   (org-element-interpret-data auth) info))))))
;;     (append
;;      (list
;;       (when (org-string-nw-p author)
;;         (list "name" "author" author))
;;       (when (org-string-nw-p (plist-get info :description))
;;         (list "name" "description"
;;               (plist-get info :description)))
;;       '("name" "generator" "org mode")
;;       '("name" "theme-color" "#77aa99")
;;       '("property" "og:type" "article")
;;       (list "property" "og:title" title)
;;       (let ((subtitle (org-export-data (plist-get info :subtitle) info)))
;;         (when (org-string-nw-p subtitle)
;;           (list "property" "og:description" subtitle))))
;;      (when org-html-meta-tags-opengraph-image
;;        (list (list "property" "og:image" (plist-get org-html-meta-tags-opengraph-image :image))
;;              (list "property" "og:image:type" (plist-get org-html-meta-tags-opengraph-image :type))
;;              (list "property" "og:image:width" (plist-get org-html-meta-tags-opengraph-image :width))
;;              (list "property" "og:image:height" (plist-get org-html-meta-tags-opengraph-image :height))
;;              (list "property" "og:image:alt" (plist-get org-html-meta-tags-opengraph-image :alt))))
;;      (list
;;       (when (org-string-nw-p author)
;;         (list "property" "og:article:author:first_name" (car (s-split-up-to " " author 2))))
;;       (when (and (org-string-nw-p author) (s-contains-p " " author))
;;         (list "property" "og:article:author:last_name" (cadr (s-split-up-to " " author 2))))
;;       (list "property" "og:article:published_time"
;;             (format-time-string
;;              "%FT%T%z"
;;              (or
;;               (when-let ((date-str (cadar (org-collect-keywords '("DATE")))))
;;                 (unless (string= date-str (format-time-string "%F"))
;;                   (ignore-errors (encode-time (org-parse-time-string date-str)))))
;;               (if buffer-file-name
;;                   (file-attribute-modification-time (file-attributes buffer-file-name))
;;                 (current-time)))))
;;       (when buffer-file-name
;;         (list "property" "og:article:modified_time"
;;               (format-time-string "%FT%T%z" (file-attribute-modification-time (file-attributes buffer-file-name)))))))))

;; (unless (functionp #'org-html-meta-tags-default)
;;   (defalias 'org-html-meta-tags-default #'ignore))
;; (setq org-html-meta-tags #'org-html-meta-tags-fancy)
;; ;; Extra header content:3 ends here

;; ;; [[file:lang.org::*Custom CSS/JS][Custom CSS/JS:2]]
;; (setq org-html-style-plain org-html-style-default
;;       org-html-htmlize-output-type 'css
;;       org-html-doctype "html5"
;;       org-html-html5-fancy t)

;; (defun org-html-reload-fancy-style ()
;;   (interactive)
;;   (setq org-html-style-fancy
;;         (concat (f-read-text (expand-file-name "misc/org-export-header.html" doom-private-dir))
;;                 "<script>\n"
;;                 (f-read-text (expand-file-name "misc/org-css/main.js" doom-private-dir))
;;                 "</script>\n<style>\n"
;;                 (f-read-text (expand-file-name "misc/org-css/main.min.css" doom-private-dir))
;;                 "</style>"))
;;   (when org-fancy-html-export-mode
;;     (setq org-html-style-default org-html-style-fancy)))
;; (org-html-reload-fancy-style)
;; ;; Custom CSS/JS:2 ends here

;; ;; [[file:lang.org::*Collapsable src and example blocks][Collapsable src and example blocks:1]]
;; (defvar org-html-export-collapsed nil)
;; (eval '(cl-pushnew '(:collapsed "COLLAPSED" "collapsed" org-html-export-collapsed t)
;;                    (org-export-backend-options (org-export-get-backend 'html))))
;; (add-to-list 'org-default-properties "EXPORT_COLLAPSED")
;; ;; Collapsable src and example blocks:1 ends here

;; ;; [[file:lang.org::Src blocks][Src blocks]]
;; (defadvice! org-html-src-block-collapsable (orig-fn src-block contents info)
;;   "Wrap the usual <pre> block in a <details>"
;;   :around #'org-html-src-block
;;   (if (or (not org-fancy-html-export-mode) (bound-and-true-p org-msg-export-in-progress))
;;       (funcall orig-fn src-block contents info)
;;     (let* ((properties (cadr src-block))
;;            (lang (mode-name-to-lang-name
;;                   (plist-get properties :language)))
;;            (name (plist-get properties :name))
;;            (ref (org-export-get-reference src-block info))
;;            (collapsed-p (member (or (org-export-read-attribute :attr_html src-block :collapsed)
;;                                     (plist-get info :collapsed))
;;                                 '("y" "yes" "t" t "true" "all"))))
;;       (format
;;        "<details id='%s' class='code'%s><summary%s>%s</summary>
;; <div class='gutter'>
;; <a href='#%s'>#</a>
;; <button title='Copy to clipboard' onclick='copyPreToClipbord(this)'>⎘</button>\
;; </div>
;; %s
;; </details>"
;;        ref
;;        (if collapsed-p "" " open")
;;        (if name " class='named'" "")
;;        (concat
;;         (when name (concat "<span class=\"name\">" name "</span>"))
;;         "<span class=\"lang\">" lang "</span>")
;;        ref
;;        (if name
;;            (replace-regexp-in-string (format "<pre\\( class=\"[^\"]+\"\\)? id=\"%s\">" ref) "<pre\\1>"
;;                                      (funcall orig-fn src-block contents info))
;;          (funcall orig-fn src-block contents info))))))

;; (defun mode-name-to-lang-name (mode)
;;   (or (cadr (assoc mode
;;                    '(("asymptote" "Asymptote")
;;                      ("awk" "Awk")
;;                      ("C" "C")
;;                      ("clojure" "Clojure")
;;                      ("css" "CSS")
;;                      ("D" "D")
;;                      ("ditaa" "ditaa")
;;                      ("dot" "Graphviz")
;;                      ("calc" "Emacs Calc")
;;                      ("emacs-lisp" "Emacs Lisp")
;;                      ("fortran" "Fortran")
;;                      ("gnuplot" "gnuplot")
;;                      ("haskell" "Haskell")
;;                      ("hledger" "hledger")
;;                      ("java" "Java")
;;                      ("js" "Javascript")
;;                      ("latex" "LaTeX")
;;                      ("ledger" "Ledger")
;;                      ("lisp" "Lisp")
;;                      ("lilypond" "Lilypond")
;;                      ("lua" "Lua")
;;                      ("matlab" "MATLAB")
;;                      ("mscgen" "Mscgen")
;;                      ("ocaml" "Objective Caml")
;;                      ("octave" "Octave")
;;                      ("org" "Org mode")
;;                      ("oz" "OZ")
;;                      ("plantuml" "Plantuml")
;;                      ("processing" "Processing.js")
;;                      ("python" "Python")
;;                      ("R" "R")
;;                      ("ruby" "Ruby")
;;                      ("sass" "Sass")
;;                      ("scheme" "Scheme")
;;                      ("screen" "Gnu Screen")
;;                      ("sed" "Sed")
;;                      ("sh" "shell")
;;                      ("sql" "SQL")
;;                      ("sqlite" "SQLite")
;;                      ("forth" "Forth")
;;                      ("io" "IO")
;;                      ("J" "J")
;;                      ("makefile" "Makefile")
;;                      ("maxima" "Maxima")
;;                      ("perl" "Perl")
;;                      ("picolisp" "Pico Lisp")
;;                      ("scala" "Scala")
;;                      ("shell" "Shell Script")
;;                      ("ebnf2ps" "ebfn2ps")
;;                      ("cpp" "C++")
;;                      ("abc" "ABC")
;;                      ("coq" "Coq")
;;                      ("groovy" "Groovy")
;;                      ("bash" "bash")
;;                      ("csh" "csh")
;;                      ("ash" "ash")
;;                      ("dash" "dash")
;;                      ("ksh" "ksh")
;;                      ("mksh" "mksh")
;;                      ("posh" "posh")
;;                      ("ada" "Ada")
;;                      ("asm" "Assembler")
;;                      ("caml" "Caml")
;;                      ("delphi" "Delphi")
;;                      ("html" "HTML")
;;                      ("idl" "IDL")
;;                      ("mercury" "Mercury")
;;                      ("metapost" "MetaPost")
;;                      ("modula-2" "Modula-2")
;;                      ("pascal" "Pascal")
;;                      ("ps" "PostScript")
;;                      ("prolog" "Prolog")
;;                      ("simula" "Simula")
;;                      ("tcl" "tcl")
;;                      ("tex" "LaTeX")
;;                      ("plain-tex" "TeX")
;;                      ("verilog" "Verilog")
;;                      ("vhdl" "VHDL")
;;                      ("xml" "XML")
;;                      ("nxml" "XML")
;;                      ("conf" "Configuration File"))))
;;       mode))
;; ;; Src blocks ends here

;; ;; [[file:lang.org::Example, fixed width, and property blocks][Example, fixed width, and property blocks]]
;; (defun org-html-block-collapsable (orig-fn block contents info)
;;   "Wrap the usual block in a <details>"
;;   (if (or (not org-fancy-html-export-mode) (bound-and-true-p org-msg-export-in-progress))
;;       (funcall orig-fn block contents info)
;;     (let ((ref (org-export-get-reference block info))
;;           (type (pcase (car block)
;;                   ('property-drawer "Properties")))
;;           (collapsed-default (pcase (car block)
;;                                ('property-drawer t)
;;                                (_ nil)))
;;           (collapsed-value (org-export-read-attribute :attr_html block :collapsed))
;;           (collapsed-p (or (member (org-export-read-attribute :attr_html block :collapsed)
;;                                    '("y" "yes" "t" t "true"))
;;                            (member (plist-get info :collapsed) '("all")))))
;;       (format
;;        "<details id='%s' class='code'%s>
;; <summary%s>%s</summary>
;; <div class='gutter'>\
;; <a href='#%s'>#</a>
;; <button title='Copy to clipboard' onclick='copyPreToClipbord(this)'>⎘</button>\
;; </div>
;; %s\n
;; </details>"
;;        ref
;;        (if (or collapsed-p collapsed-default) "" " open")
;;        (if type " class='named'" "")
;;        (if type (format "<span class='type'>%s</span>" type) "")
;;        ref
;;        (funcall orig-fn block contents info)))))

;; (advice-add 'org-html-example-block   :around #'org-html-block-collapsable)
;; (advice-add 'org-html-fixed-width     :around #'org-html-block-collapsable)
;; (advice-add 'org-html-property-drawer :around #'org-html-block-collapsable)
;; ;; Example, fixed width, and property blocks ends here

;; ;; [[file:lang.org::*Include extra font-locking in htmlize][Include extra font-locking in htmlize:1]]
;; (autoload #'highlight-numbers--turn-on "highlight-numbers")
;; (add-hook 'htmlize-before-hook #'highlight-numbers--turn-on)
;; ;; Include extra font-locking in htmlize:1 ends here

;; ;; [[file:lang.org::*Handle table overflow][Handle table overflow:1]]
;; (defadvice! org-html-table-wrapped (orig-fn table contents info)
;;   "Wrap the usual <table> in a <div>"
;;   :around #'org-html-table
;;   (if (or (not org-fancy-html-export-mode) (bound-and-true-p org-msg-export-in-progress))
;;       (funcall orig-fn table contents info)
;;     (let* ((name (plist-get (cadr table) :name))
;;            (ref (org-export-get-reference table info)))
;;       (format "<div id='%s' class='table'>
;; <div class='gutter'><a href='#%s'>#</a></div>
;; <div class='tabular'>
;; %s
;; </div>\
;; </div>"
;;               ref ref
;;               (if name
;;                   (replace-regexp-in-string (format "<table id=\"%s\"" ref) "<table"
;;                                             (funcall orig-fn table contents info))
;;                 (funcall orig-fn table contents info))))))
;; ;; Handle table overflow:1 ends here

;; ;; [[file:lang.org::*TOC as a collapsable tree][TOC as a collapsable tree:1]]
;; (defadvice! org-html--format-toc-headline-colapseable (orig-fn headline info)
;;   "Add a label and checkbox to `org-html--format-toc-headline's usual output,
;; to allow the TOC to be a collapseable tree."
;;   :around #'org-html--format-toc-headline
;;   (if (or (not org-fancy-html-export-mode) (bound-and-true-p org-msg-export-in-progress))
;;       (funcall orig-fn headline info)
;;     (let ((id (or (org-element-property :CUSTOM_ID headline)
;;                   (org-export-get-reference headline info))))
;;       (format "<input type='checkbox' id='toc--%s'/><label for='toc--%s'>%s</label>"
;;               id id (funcall orig-fn headline info)))))
;; ;; TOC as a collapsable tree:1 ends here

;; ;; [[file:lang.org::*TOC as a collapsable tree][TOC as a collapsable tree:2]]
;; (defadvice! org-html--toc-text-stripped-leaves (orig-fn toc-entries)
;;   "Remove label"
;;   :around #'org-html--toc-text
;;   (if (or (not org-fancy-html-export-mode) (bound-and-true-p org-msg-export-in-progress))
;;       (funcall orig-fn toc-entries)
;;     (replace-regexp-in-string "<input [^>]+><label [^>]+>\\(.+?\\)</label></li>" "\\1</li>"
;;                               (funcall orig-fn toc-entries))))
;; ;; TOC as a collapsable tree:2 ends here

;; ;; [[file:lang.org::*Make verbatim different to code][Make verbatim different to code:1]]
;; (setq org-html-text-markup-alist
;;       '((bold . "<b>%s</b>")
;;         (code . "<code>%s</code>")
;;         (italic . "<i>%s</i>")
;;         (strike-through . "<del>%s</del>")
;;         (underline . "<span class=\"underline\">%s</span>")
;;         (verbatim . "<kbd>%s</kbd>")))
;; ;; Make verbatim different to code:1 ends here

;; ;; [[file:lang.org::*Change checkbox type][Change checkbox type:1]]
;; (appendq! org-html-checkbox-types
;;           '((html-span
;;              (on . "<span class='checkbox'></span>")
;;              (off . "<span class='checkbox'></span>")
;;              (trans . "<span class='checkbox'></span>"))))
;; (setq org-html-checkbox-type 'html-span)
;; ;; Change checkbox type:1 ends here

;; ;; [[file:lang.org::*Extra special strings][Extra special strings:1]]
;; (pushnew! org-html-special-string-regexps
;;           '("-&gt;" . "&#8594;")
;;           '("&lt;-" . "&#8592;"))
;; ;; Extra special strings:1 ends here

;; ;; [[file:lang.org::*Header anchors][Header anchors:1]]
;; (defun org-export-html-headline-anchor (text backend info)
;;   (when (and (org-export-derived-backend-p backend 'html)
;;              (not (org-export-derived-backend-p backend 're-reveal))
;;              org-fancy-html-export-mode)
;;     (unless (bound-and-true-p org-msg-export-in-progress)
;;       (replace-regexp-in-string
;;        "<h\\([0-9]\\) id=\"\\([a-z0-9-]+\\)\">\\(.*[^ ]\\)<\\/h[0-9]>" ; this is quite restrictive, but due to `org-reference-contraction' I can do this
;;        "<h\\1 id=\"\\2\">\\3<a aria-hidden=\"true\" href=\"#\\2\">#</a> </h\\1>"
;;        text))))

;; (add-to-list 'org-export-filter-headline-functions
;;              'org-export-html-headline-anchor)
;; ;; Header anchors:1 ends here

;; ;; [[file:lang.org::*Link previews][Link previews:1]]
;; (org-link-set-parameters "Https"
;;                          :follow (lambda (url arg) (browse-url (concat "https:" url) arg))
;;                          :export #'org-url-fancy-export)
;; ;; Link previews:1 ends here

;; ;; [[file:lang.org::*Link previews][Link previews:2]]
;; (defun org-url-fancy-export (url _desc backend)
;;   (let ((metadata (org-url-unfurl-metadata (concat "https:" url))))
;;     (cond
;;      ((org-export-derived-backend-p backend 'html)
;;       (concat
;;        "<div class=\"link-preview\">"
;;        (format "<a href=\"%s\">" (concat "https:" url))
;;        (when (plist-get metadata :image)
;;          (format "<img src=\"%s\"/>" (plist-get metadata :image)))
;;        "<small>"
;;        (replace-regexp-in-string "//\\(?:www\\.\\)?\\([^/]+\\)/?.*" "\\1" url)
;;        "</small><p>"
;;        (when (plist-get metadata :title)
;;          (concat "<b>" (org-html-encode-plain-text (plist-get metadata :title)) "</b></br>"))
;;        (when (plist-get metadata :description)
;;          (org-html-encode-plain-text (plist-get metadata :description)))
;;        "</p></a></div>"))
;;      (t url))))
;; ;; Link previews:2 ends here

;; ;; [[file:lang.org::*Link previews][Link previews:3]]
;; (setq org-url-unfurl-metadata--cache nil)
;; (defun org-url-unfurl-metadata (url)
;;   (cdr (or (assoc url org-url-unfurl-metadata--cache)
;;            (car (push
;;                  (cons
;;                   url
;;                   (let* ((head-data
;;                           (-filter #'listp
;;                                    (cdaddr
;;                                     (with-current-buffer (progn (message "Fetching metadata from %s" url)
;;                                                                 (url-retrieve-synchronously url t t 5))
;;                                       (goto-char (point-min))
;;                                       (delete-region (point-min) (- (search-forward "<head") 6))
;;                                       (delete-region (search-forward "</head>") (point-max))
;;                                       (goto-char (point-min))
;;                                       (while (re-search-forward "<script[^\u2800]+?</script>" nil t)
;;                                         (replace-match ""))
;;                                       (goto-char (point-min))
;;                                       (while (re-search-forward "<style[^\u2800]+?</style>" nil t)
;;                                         (replace-match ""))
;;                                       (libxml-parse-html-region (point-min) (point-max))))))
;;                          (meta (delq nil
;;                                      (mapcar
;;                                       (lambda (tag)
;;                                         (when (eq 'meta (car tag))
;;                                           (cons (or (cdr (assoc 'name (cadr tag)))
;;                                                     (cdr (assoc 'property (cadr tag))))
;;                                                 (cdr (assoc 'content (cadr tag))))))
;;                                       head-data))))
;;                     (let ((title (or (cdr (assoc "og:title" meta))
;;                                      (cdr (assoc "twitter:title" meta))
;;                                      (nth 2 (assq 'title head-data))))
;;                           (description (or (cdr (assoc "og:description" meta))
;;                                            (cdr (assoc "twitter:description" meta))
;;                                            (cdr (assoc "description" meta))))
;;                           (image (or (cdr (assoc "og:image" meta))
;;                                      (cdr (assoc "twitter:image" meta)))))
;;                       (when image
;;                         (setq image (replace-regexp-in-string
;;                                      "^/" (concat "https://" (replace-regexp-in-string "//\\([^/]+\\)/?.*" "\\1" url) "/")
;;                                      (replace-regexp-in-string
;;                                       "^//" "https://"
;;                                       image))))
;;                       (list :title title :description description :image image))))
;;                  org-url-unfurl-metadata--cache)))))
;; ;; Link previews:3 ends here

;; ;; [[file:lang.org::*Pre-rendered][Pre-rendered:1]]
;; ;; (setq-default org-html-with-latex `dvisvgm)
;; ;; Pre-rendered:1 ends here

;; ;; [[file:lang.org::*MathJax][MathJax:1]]
;; (setq org-html-mathjax-options
;;       '((path "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-svg.js" )
;;         (scale "1")
;;         (autonumber "ams")
;;         (multlinewidth "85%")
;;         (tagindent ".8em")
;;         (tagside "right")))

;; (setq org-html-mathjax-template
;;       "<script>
;; MathJax = {
;;   chtml: {
;;     scale: %SCALE
;;   },
;;   svg: {
;;     scale: %SCALE,
;;     fontCache: \"global\"
;;   },
;;   tex: {
;;     tags: \"%AUTONUMBER\",
;;     multlineWidth: \"%MULTLINEWIDTH\",
;;     tagSide: \"%TAGSIDE\",
;;     tagIndent: \"%TAGINDENT\"
;;   }
;; };
;; </script>
;; <script id=\"MathJax-script\" async
;;         src=\"%PATH\"></script>")
;; ;; MathJax:1 ends here
