;;; lisp/setup-telega.el -*- lexical-binding: t; -*-


(setq telega-use-docker t)
(use-package! telega
    :commands (telega)
    :init
        (defun my/telega/olivetti () (setq-local olivetti-body-width 100))
        (defun my/telega/company-backends ()
            (setq-local company-backends
                (append '(telega-company-username telega-company-botcmd )
                        company-backends)))
    ;; :hook (telega-chat-mode . olivetti-mode)
          ;; (telega-chat-mode . my/telega/olivetti)
      :hook   (telega-chat-mode . my/telega/company-backends)
    :config
    ;; (telega-mode-line-mode)
    ;; (telega-notifications-mode 1)
  ;; clear all bindings (he willl introduce many conflicting bindings again :/)
  (setf (cdr telega-chat-mode-map) nil)
  (setf (cdr telega-msg-button-map) nil)
  ;; (setf (cdr telega-root-mode-map) nil)
  ;; (setq telega-root-mode-map nil)
  ;;Easy closing
  (map! :map telega-root-mode-map
        :n "q" 'bury-buffer)
  (map! :map telega-chat-mode-map
        :n "q" 'bury-buffer)
  ;;Open Telega
  (map! :leader
        :prefix "o"
        :desc "Telega" "g" #'telega)
  ;; :desc "Telega" "g" #'=telegram)
  (map! :after telega
        :mode telega-root-mode
        "C-j" 'telega-button-forward
        "C-k" 'telega-button-backward

        "C-/" 'telega-filter-undo
        "C-_" 'telega-filter-undo
        "C-x C-/" 'telega-filter-redo
        "C-x C-_" 'telega-filter-redo

        [?\t] 'telega-button-forward
        "\e\t" 'telega-button-backward
        [backtab] 'telega-button-backward

        ;; "M-g u" 'telega-root-next-unread
        ;; "M-g i" 'telega-root-next-important
        ;; "M-g m" 'telega-root-next-mention
        ;; "M-g @" 'telega-root-next-mention

        (:localleader
         ;; "q" 'bury-buffer
         ;; "Q" 'telega-kill
         "J" 'telega-chat-join-by-link
         "N" 'telega-chat-create

         ;; NOTE: Deleting all chats is very-very-very dangerous, so
         ;; disabled, use M-x telega-chats-filtered-delete RET if you know
         ;; what you are doing
         ;; (define-key map (kbd "D") 'telega-chats-filtered-delete)

         "K" 'telega-chats-filtered-kill-chatbuf
         "R" 'telega-chats-filtered-toggle-read

         :desc "sort" "\\" telega-sort-map

         :desc "filter" "/" telega-filter-map

         (:prefix ("?" . "describe")
          "w" 'telega-describe-connected-websites
          "s" 'telega-describe-active-sessions
          "n" 'telega-describe-network
          "y" 'telega-describe-notifications
          "N" 'telega-describe-notifications
          "p" 'telega-describe-privacy-settings)

         (:prefix ("F" . "folder")
          "+" 'telega-folder-create
          "-" 'telega-folder-delete
          "=" 'telega-folders-reorder
          "R" 'telega-folder-rename
          "a" 'telega-chat-add-to-folder
          "d" 'telega-chat-remove-from-folder)

         (:prefix ("c". "call")
          "a" 'telega-voip-accept
          "d" 'telega-voip-discard
          "b" 'telega-voip-buffer-show
          "l" 'telega-voip-list-calls)

         (:prefix ("v" . "View")
          "s" 'telega-view-search
          "s" 'telega-view-search
          "n" 'telega-view-nearby
          "v" 'telega-view-reset
          "0" 'telega-view-compact
          "1" 'telega-view-one-line
          "2" 'telega-view-two-lines
          "t" 'telega-view-topics
          "T" 'telega-view-top
          "S" 'telega-view-settings
          "c" 'telega-view-contacts
          "C" 'telega-view-calls
          "l" 'telega-view-last-messages
          "f" 'telega-view-folders
          "^" 'telega-view-pinned-messages
          "p" 'telega-view-pinned-messages
          "d" 'telega-view-deleted-chats)))

  (map! :mode telega-chat-mode
        :map telega-chat-mode-map
        "C-l" 'telega-chatbuf-recenter-1

        ;; ;; C-M-[ - cancels edit/reply
        "\e\e" 'telega-chatbuf-cancel-aux
        "C-c C-k" 'telega-chatbuf-cancel-aux
        "C-M-c" 'telega-chatbuf-cancel-aux
        "C-M-a" 'telega-chatbuf-beginning-of-thing

        "C-c ?" 'telega-describe-chat

        "RET" 'telega-chatbuf-newline-or-input-send
        "M-p" 'telega-chatbuf-edit-prev
        "M-n" 'telega-chatbuf-edit-next
        "M-r" 'telega-chatbuf-input-search

        ;; ;;; ellit-org: chatbuf-attach-bindings
        ;; ;; - {{{where-is(telega-chatbuf-attach,telega-chat-mode-map)}}} ::
        ;; ;;   {{{fundoc(telega-chatbuf-attach,2)}}}
        ;; (define-key map (kbd "C-c C-a") 'telega-chatbuf-attach)
        ;; ;;; ellit-org: chatbuf-attach-bindings
        ;; ;; - {{{where-is(telega-chatbuf-attach-media,telega-chat-mode-map)}}} ::
        ;; ;;   {{{fundoc(telega-chatbuf-attach-media,2)}}}
        ;; (define-key map (kbd "C-c C-f") 'telega-chatbuf-attach-media)
        ;; ;;; ellit-org: chatbuf-attach-bindings
        ;; ;; - {{{where-is(telega-chatbuf-attach-clipboard,telega-chat-mode-map)}}} ::
        ;; ;;   {{{fundoc(telega-chatbuf-attach-clipboard,2)}}}
        ;; (define-key map (kbd "C-c C-v") 'telega-chatbuf-attach-clipboard)

        ;; ;;; ellit-org: chatbuf-filtering-bindings
        ;; ;; - {{{where-is(telega-chatbuf-filter,telega-chat-mode-map)}}} ::
        ;; ;;   {{{fundoc(telega-chatbuf-filter,2)}}}
        ;; (define-key map (kbd "C-c /") 'telega-chatbuf-filter)
        ;; ;;; ellit-org: chatbuf-filtering-bindings
        ;; ;; - {{{where-is(telega-chatbuf-filter-cancel,telega-chat-mode-map)}}} ::
        ;; ;;   {{{fundoc(telega-chatbuf-filter-cancel, 2)}}}
        ;; (define-key map (kbd "C-c C-c") 'telega-chatbuf-filter-cancel)
        ;; ;;; ellit-org: chatbuf-filtering-bindings
        ;; ;; - {{{where-is(telega-chatbuf-filter-search,telega-chat-mode-map)}}} ::
        ;; ;;   {{{fundoc(telega-chatbuf-filter-search, 2)}}}
        ;; (define-key map (kbd "C-c C-s") 'telega-chatbuf-filter-search)
        ;; (define-key map (kbd "C-c C-r") 'telega-chatbuf-filter-search)

        ;; ;; jumping around links
        "TAB" 'telega-chatbuf-complete-or-next-link
        "<backtab>" 'telega-chatbuf-prev-link

        ;; Additional prefix keymaps
        "M-g" telega-chatbuf-fastnav-map
        (:localleader
         ;; "i" 'telega-describe-chat
         ;; "h" 'telega-describe-chat
         ;; "a" 'telega-chat-add-member
         ;; "o" 'telega-chat-set-custom-order
         ;; "r" 'telega-chat-toggle-read
         ;; "d" 'telega-chat-delete
         ;; "P" 'telega-chat-toggle-pin
         ;; "^" 'telega-chat-toggle-pin
         ;; "C" 'telega-chat-call
         ;; "DEL" 'telega-chat-delete

         :desc "Filter" "/" #'telega-chatbuf-filter
         :desc "Describe" "?" #'telega-chatbuf-describe
         :desc "Attach from clipboard" "p" #'telega-chatbuf-attach-clipboard
         :desc "Attach" "a" #'telega-chatbuf-attach
         :desc "Attach File" "f" #'telega-chatbuf-attach-fille
         :desc "Filter cancel" "c" #'telega-chatbuf-filter-cancel
         :desc "Cancel aux" "k" #'telega-chatbuf-cancel-aux
         :desc "Filter search" "s" #'telega-chatbuf-filter-search
         :desc "My search" "S" #'+hydra/telega-search/body
         ))

  (map! :map telega-msg-button-map
        "r" #'telega-msg-reply
        "c" 'telega-msg-copy-text
        "n" #'evil-ex-search-next
        "C-j" #'telega-button-forward
        "C-k" #'telega-button-backward
        "c" #'telega-msg-edit
        (:localleader
         "k" #'telega-chatbuf-cancel-aux
         ;; ;; ffplay media controls for some media messages
         "," 'telega-msg--vvnote-rewind-10-backward
         "<" 'telega-msg--vvnote-rewind-10-backward
         "." 'telega-msg--vvnote-rewind-10-forward
         ">" 'telega-msg--vvnote-rewind-10-forward
         "x" 'telega-msg--vvnote-play-speed-toggle

         "0" 'telega-msg--vvnote-stop
         "1" 'telega-msg--vvnote-rewind-part
         "2" 'telega-msg--vvnote-rewind-part
         "3" 'telega-msg--vvnote-rewind-part
         "4" 'telega-msg--vvnote-rewind-part
         "5" 'telega-msg--vvnote-rewind-part
         "6" 'telega-msg--vvnote-rewind-part
         "7" 'telega-msg--vvnote-rewind-part
         "8" 'telega-msg--vvnote-rewind-part
         "9" 'telega-msg--vvnote-rewind-part
         "B" 'telega-msg-ban-sender
         "F" 'telega-msg-forward-marked-or-at-point-to-multiple-chats
         "L" 'telega-msg-redisplay
         "U" 'telega-chatbuf-msg-marks-toggle
         ;; ;; Menu for right mouse on a message
         ;; (define-key map [down-mouse-3] telega-msg-button-menu-map)
         ;; (define-key map [mouse-3] #'ignore)
         "DEL" 'telega-msg-delete-marked-or-at-point
         "*" 'telega-msg-favorite-toggle
         "m" #'telega-msg-mark-toggle
         "U" #'telega-chatbuf-unmark-all
         "i" #'telega-describe-message
         "e" #'telega-msg-edit
         "f" #'telega-msg-forward-marked-or-at-point
         "d" #'telega-msg-delete-marked-or-at-point
         ;; "k" #'telega-msg-delete-marked-or-at-point
         "l" #'telega-msg-redisplay
         "T" #'telega-msg-open-thread
         "=" #'telega-msg-diff-edits
         "y" #'telega-msg-copy-text
         "Y" #'telega-chatbuf-copy-link
         "j" #'+hydra/telega-search/body/body
         "R" #'telega-msg-resend
         "S" #'telega-msg-save
         "P" #'telega-msg-pin-toggle
         "^" #'telega-msg-pin-toggle))

  ;; (map! :leader "G" telega-prefix-map)
  (map! :leader "y" telega-prefix-map)
  (map! :map telega-prefix-map
        :leader
        :prefix "y"
        :desc "View folders" "F" #'telega-view-folders
        :desc "Kill telega" "k" #'telega-kill)

  (setq telega-chat-input-markups '("markdown1" nil "markdown2"))

  ;; (setq telega-completing-read-function 'ivy-completing-read)
  (setq telega-completing-read-function 'completing-read)

  ;;FIXME the path is in nix store!
  ;;;;;;;Contributed packages
  (add-load-path! (concat doom-emacs-dir ".local/straight/repos/telega.el/contrib"))
  (require 'telega-mnz) ;;;syntax highlight for code blocks
  (add-hook! 'telega-root-mode-hook 'global-telega-mnz-mode)
  (require 'telega-url-shorten) ;;;Url shortening with small svg icons
  (add-hook! 'telega-root-mode-hook 'global-telega-url-shorten-mode)
  (require 'telega-dired-dwim) ;;; select multiple files from dired
  ;; (require 'telega-transient)
  ;; (telega-transient-mode 1)

  ;; font some icons
  (set-fontset-font t 'unicode "Symbola" nil 'append)

  (setq telega-notifications-mode 1)

  (after! doom-modeline
    ;; (setq telega-mode-line--logo-image-cache
    ;;       (doom-modeline-icon 'fileicon "telegram" "✈️" "#"
    ;;                           :face
    ;;                           'all-the-icons-blue
    ;;                           'mode-line-inactive))
    ;; (defun telega-mode-line-logo-image()
    ;;   (doom-modeline-icon 'fileicon "telegram" "✈️" "#"
    ;;                       :face
    ;;                       (if (telega-user-online-p (telega-user-me))
    ;;                           'all-the-icons-blue
    ;;                         'all-the-icons-red
    ;;                         )
    ;;                       'mode-line-inactive))
    (setq telega-mode-line-string-format
          '(" "
            (:eval
             (telega-mode-line-icon))
            (:eval
             (when telega-use-tracking-for
               (telega-mode-line-tracking)))
            (:eval
             (telega-mode-line-unread-unmuted))
            (:eval
             (telega-mode-line-mentions 'messages))))
    (telega-mode-line-mode))

  ;; ;; '(image :type svg
  ;; ;;         :file "/home/striker/.config/emacs/.local/straight/build-28.0.50/telega/etc/telega-logo.svg"
  ;; ;;         :ascent center :background "#191729" :height 18))

  (add-to-list 'display-buffer-alist
               '((lambda (bufname _action)
                   (with-current-buffer bufname
                     (eq major-mode 'telega-image-mode)))
                 (display-buffer-in-side-window)
                 (reusable-frames     . visible)
                 (side                . right)
                 ;; (window-height       . 0.33)
                 (window-width        . 0.45)
                 ))

  ;; fallback buffer
  ;; (add-hook! 'telega-image-mode-hook #'doom-mark-buffer-as-real-h)
  ;; (add-hook! 'telega-chat-mode-hook #'doom-mark-buffer-as-real-h)
  ;; (doom-set-buffer-real (get-buffer "*Telega Root*") t)

  ;; (add-to-list 'display-buffer-alist
  ;;              '("^\\*Image\\*$"
  ;;                (reusable-frames     . visible)
  ;;                (side                . right)
  ;;                (window-height       . 0.33)
  ;;                )))

  ;; Found %count messages with %query
  (after! hydra
    (defhydra +hydra/telega-search
      (:hint nil)
      "
     Found %(plist-get telega-chatbuf--msg-filter 'total-count) messages with
     [_g_] First    [_l_] Show list
     [_k_] Previous [_s_] seach in another chat     ╭─────────────────────
     [_j_] Next     [_c_] change query              │[_q_] quit
     [_G_] Last                                   │[_q_] Quit and go back"
      ;; ("j" (progn (git-gutter:next-hunk 1) (recenter)))
      ("j" (progn (telega-chatbuf--input-search-next) (recenter)))
      ("k" (progn (telega-chatbuf--input-search-next) (recenter)))
      ("g" (progn (goto-char (point-min)) (git-gutter:next-hunk 1)))
      ("G" (progn (goto-char (point-min)) (git-gutter:previous-hunk 1)))
      ("l" git-gutter:popup-hunk)
      ("s" telega-chatbuf-filter)
      ("c" telega-chatbuf-filter-search)
      ("q"
       telega-chatbuf-filter-cancel
       ;; (when (get-buffer git-gutter:popup-buffer)
       ;;   (kill-buffer (get-buffer git-gutter:popup-buffer)))
       nil
       :color blue)
      ))

    :custom (telega-server-libs-prefix "/usr/local")
            (telega-chat-bidi-display-reordering 'right-to-left)
            (telega-sticker-size '(10 . 24))
            (telega-emoji-use-images nil))

;; (setq telega-tdlib-max-version "1.8.5")
