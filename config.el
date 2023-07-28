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

(use-package! mixed-pitch
  :hook (org-mode . mixed-pitch-mode)
  :config
  (setq mixed-pitch-face 'variable-pitch))

(defvar mixed-pitch-modes '(org-mode LaTeX-mode markdown-mode gfm-mode Info-mode)
  "Modes that `mixed-pitch-mode' should be enabled in, but only after UI initialisation.")
(defun init-mixed-pitch-h ()
  "Hook `mixed-pitch-mode' into each mode in `mixed-pitch-modes'.
Also immediately enables `mixed-pitch-modes' if currently in one of the modes."
  (when (memq major-mode mixed-pitch-modes)
    (mixed-pitch-mode 1))
  (dolist (hook mixed-pitch-modes)
    (add-hook (intern (concat (symbol-name hook) "-hook")) #'mixed-pitch-mode)))
(add-hook 'doom-init-ui-hook #'init-mixed-pitch-h)

(autoload #'mixed-pitch-serif-mode "mixed-pitch"
  "Change the default face of the current buffer to a serifed variable pitch, while keeping some faces fixed pitch." t)

(setq! variable-pitch-serif-font (font-spec :family "Alegreya" :size 27))

(after! mixed-pitch
  (setq mixed-pitch-set-height t)
  (set-face-attribute 'variable-pitch-serif nil :font variable-pitch-serif-font)
  (defun mixed-pitch-serif-mode (&optional arg)
    "Change the default face of the current buffer to a serifed variable pitch, while keeping some faces fixed pitch."
    (interactive)
    (let ((mixed-pitch-face 'variable-pitch-serif))
      (mixed-pitch-mode (or arg 'toggle)))))

(autoload #'mixed-pitch-serif-mode "mixed-pitch"
  "Change the default face of the current buffer to a serifed variable pitch, while keeping some faces fixed pitch." t)

(setq! variable-pitch-serif-font (font-spec :family "Alegreya" :size 27))

(after! mixed-pitch
  (setq mixed-pitch-set-height t)
  (set-face-attribute 'variable-pitch-serif nil :font variable-pitch-serif-font)
  (defun mixed-pitch-serif-mode (&optional arg)
    "Change the default face of the current buffer to a serifed variable pitch, while keeping some faces fixed pitch."
    (interactive)
    (let ((mixed-pitch-face 'variable-pitch-serif))
      (mixed-pitch-mode (or arg 'toggle)))))

(set-char-table-range composition-function-table ?f '(["\\(?:ff?[fijlt]\\)" 0 font-shape-gstring]))
(set-char-table-range composition-function-table ?T '(["\\(?:Th\\)" 0 font-shape-gstring]))

(defface variable-pitch-serif
    '((t (:family "serif")))
    "A variable-pitch face with serifs."
    :group 'basic-faces)

(defcustom variable-pitch-serif-font (font-spec :family "serif")
  "The font face used for `variable-pitch-serif'."
  :group 'basic-faces
  :set (lambda (symbol value)
         (set-face-attribute 'variable-pitch-serif nil :font value)
         (set-default-toplevel-value symbol value)))
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
  ;; (telega nil)
  (load "~/.config/doom/lisp/setup-telega.el")
  (setq telega-notifications-mode t))
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

  (defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))


  (defun kill-dired-buffers ()
    "Kill all open dired buffers."
    (interactive)
    (mapc (lambda (buffer)
            (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
              (kill-buffer buffer)))
          (buffer-list)))

(map! :leader
      :desc "Switch to perspective NAME" "DEL" #'persp-switch
      :desc "Switch to buffer in perspective" "," #'persp-switch-to-buffer
      :desc "Switch to next perspective" "]" #'persp-next
      :desc "Switch to previous perspective" "[" #'persp-prev
      :desc "Add a buffer current perspective" "+" #'persp-add-buffer
      :desc "Remove perspective by name" "-" #'persp-remove-by-name)
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
;; load org mode
(load "~/.config/doom/lisp/setup-org.el")
(load "~/.config/doom/lisp/setup-org-capture.el")
(load "~/.config/doom/lisp/setup-org-roam.el")
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
