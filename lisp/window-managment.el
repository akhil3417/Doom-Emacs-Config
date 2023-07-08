;;; lisp/window-managment.el -*- lexical-binding: t; -*-


;; * WINDOW MANAGEMENT
;;;################################################################

;;;----------------------------------------------------------------
;; ** +SHACKLE+
;;;----------------------------------------------------------------

;; Wasamasa's Shackle package simplifies Emacs' rather arcane display-buffer
;; rules so you don't have to tear your hair out understanding how to configure
;; it. Unfortunately I did, see [[*SETUP-WINDOWS][setup-windows]].
(use-package! shackle
  :disabled t
  :init (shackle-mode))

;;;----------------------------------------------------------------
;; ** SETUP-WINDOWS
;;;----------------------------------------------------------------

;; Setup-windows defines window rules for displaying various kinds of buffers.
(load "~/.config/doom/lisp/setup-windows")
(use-package! setup-windows
  :demand t
  :hook ((help-mode . visual-line-mode)
         (Custom-mode . visual-line-mode)
         (helpful-mode . visual-line-mode))
  ;; :bind (;; ("C-x +" . balance-windows-area)
  ;;        ("<f8>" . +make-frame-floating-with-current-buffer)
  ;;        ("C-M-`" . window-toggle-side-windows))
  :bind
  ("<f7>" . +make-frame-floating-with-current-buffer)
  ;; "C-M-`" 'window-toggle-side-windows
  :general
  (:keymaps 'space-menu-window-map
            :wk-full-keys nil
            "w" '(window-toggle-side-windows :wk "toggle side windows")))

(use-package! window
  :bind (("H-+" . balance-windows-area)
         ("C-x +" . balance-windows-area)
         ("C-x q" . my/kill-buffer-and-window))
  :config
  (defun my/kill-buffer-and-window ()
    "Kill buffer.

Also kill this window, tab or frame if necessary."
    (interactive)
    (cl-letf ((symbol-function 'delete-window)
              (symbol-function 'my/delete-window-or-delete-frame))
      (kill-buffer-and-window))))

;; setup-windows:

;;;----------------------------------------------------------------
;; ** POPUPS
;;;----------------------------------------------------------------

;; Designate buffers to popup status and toggle or cycle through them
(use-package! popper
  :after (setup-windows setup-project)
  :commands popper-mode
  :bind (("C-`" . popper-toggle-latest)
         ("C-M-`" . popper-cycle)
         ("H-`" . popper-toggle-latest)
         ("H-M-`" . popper-cycle)
         ("H-6" . popper-toggle-type)
         ("H-M-k" . popper-kill-latest-popup))
  :init
  (setq popper-group-function
        (defun my/popper-group-by-heuristic ()
          "Group popups according to heuristic rules suitable for
          my usage."
          (let ((dd (abbreviate-file-name default-directory)))
            (cond
             ((string-match-p "\\(?:~/\\.config/\\|~/dotfiles/\\)" dd)
              'config)
             ((or (string-match-p "local/share/git" dd)
                  (string-match-p "plugins/" dd))
              'projects)
             ((string-match-p "\\(?:KarthikBa\\|research/\\)" dd)
              'research)
             ((string-match-p "karthinks" dd) 'website)
             ((locate-dominating-file dd "research") 'documents)
             ((locate-dominating-file dd "init.el") 'emacs)
             (t (popper-group-by-project))))))
  (setq ;; popper-mode-line nil
        popper-reference-buffers
        (append my/help-modes-list
                my/repl-modes-list
                my/occur-grep-modes-list
                ;; my/man-modes-list
                '(Custom-mode
                  (compilation-mode . hide)
                  messages-buffer-mode)
                '(("^\\*Warnings\\*$" . hide)
                  ("^\\*Compile-Log\\*$" . hide)
                  "^\\*Matlab Help\\*"
                  ;; "^\\*Messages\\*$"
                  "^\\*Backtrace\\*"
                  "^\\*evil-registers\\*"
                  "^\\*Apropos"
                  "^Calc:"
                  "^\\*eldoc\\*"
                  "^\\*TeX errors\\*"
                  "^\\*ielm\\*"
                  "^\\*TeX Help\\*"
                  "\\*Shell Command Output\\*"
                  ("\\*Async Shell Command\\*" . hide)
                  "\\*Completions\\*"
                  ;; "\\*scratch\\*"
                  "[Oo]utput\\*")))

  (use-package! popper-echo
    :config
    (defun popper-message-shorten (name)
      (cond
       ((string-match "^\\*[hH]elpful.*?: \\(.*\\)\\*$" name)
        (concat (match-string 1 name)
                "(H)"))
       ((string-match "^\\*Help:?\\(.*\\)\\*$" name)
        (concat (match-string 1 name)
                "(H)"))
       ((string-match "^\\*eshell:? ?\\(.*\\)\\*$" name)
        (concat (match-string 1 name)
                (if (string-empty-p (match-string 1 name)) "shell(E)" "(E)")))
       ((string-match "^\\*\\(.*?\\)\\(?:Output\\|Command\\)\\*$" name)
        (concat (match-string 1 name)
                "(O)"))
       ((string-match "^\\*\\(.*?\\)[ -][Ll]og\\*$" name)
        (concat (match-string 1 name)
                "(L)"))
       ((string-match "^\\*[Cc]ompil\\(?:e\\|ation\\)\\(.*\\)\\*$" name)
        (concat (match-string 1 name)
                "(C)"))
       (t name)))
    (setq popper-echo-transform-function #'popper-message-shorten)
    (setq popper-echo-dispatch-keys '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)
          popper-echo-dispatch-actions t)
    (advice-add 'popper-echo :around
                (defun my/popper-echo-no-which-key (orig-fn)
                  (let ((which-key-show-transient-maps nil))
                    (funcall orig-fn))))
    (popper-echo-mode +1))

  :config
  (setq popper-display-control 'user)
  (defun my/popup-raise-popup ()
    "Choose a popup-window to raise as a regular window"
    (interactive)
    (popper-raise-popup
     (completing-read "Raise popup: "
                      (mapcar (lambda (win-and-buf) (buffer-name (cdr win-and-buf)))
                              (cl-mapcan (lambda (group) )
                                         (append popper-open-popup-alist
                                                popper-buried-popup-alist)))
                      nil t)))

  (defun my/popup-lower-to-popup ()
    "Choose a regular window to make a popup"
    (interactive)
    (let ((window-list (cl-set-difference
                        (window-list)
                        (mapcar 'car popper-open-popup-alist))))
      (if (< (length window-list) 2)
          (message "Only one main window!")
        (popper-lower-to-popup
         (get-buffer
          (completing-read "Lower to popup: "
                           (mapcar (lambda (win) (buffer-name (window-buffer win)))
                                   window-list)
                           nil t))))))
  :general
  (:states 'motion
           "C-w ^" '(popper-raise-popup :wk "raise popup")
           "C-w _" '(popper-lower-to-popup :wk "lower to popup"))
  (:keymaps 'space-menu-window-map
            "^" '(my/popup-raise-popup :wk "raise popup")
            "_" '(my/popup-lower-to-popup :wk "lower to popup")))

;;----------------------------------------------------------------
;; ** WINUM
;;----------------------------------------------------------------

;; Add window numbers and use them to switch windows
;; (use-package! winum
;;   :ensure
;;   :init
;;   (defun my/winum-select (num)
;;     (lambda (&optional arg) (interactive "P")
;;       (if arg
;;           (winum-select-window-by-number (- 0 num))
;;         (if (equal num (winum-get-number))
;;             (winum-select-window-by-number (winum-get-number (get-mru-window t)))
;;           (winum-select-window-by-number num)))))

;;   (setq winum-keymap
;;         (let ((map (make-sparse-keymap)))
;;           (define-key map (kbd "C-M-0") 'winum-select-window-0-or-10)
;;           (dolist (num '(1 2 3 4 5 6 7 8 9) nil)
;;             (define-key map (kbd (concat "C-M-" (int-to-string num)))
;;               (my/winum-select num)))
;;           map))

  ;; If evil-mode is enabled further mode-line customization is needed before
  ;; enabling winum:
  ;; (unless (bound-and-true-p evil-mode)
  ;;   (winum-mode 1)))


;;----------------------------------------------------------------
;; ** Ace-window
;;----------------------------------------------------------------

;; (use-package! ace-window
;;   :ensure t
;;   :bind
;;   (("C-x o" . ace-window)
;;    ("H-o"   . ace-window)
;;    ("M-o" . other-window)
;;    ("M-O" . my/other-window-prev))
;;   :general
;;   (:keymaps 'space-menu-map
;;             "`" 'ace-window)
;;   ;; :custom-face
;;   ;; (aw-leading-char-face ((t (:height 2.5 :weight normal))))
;;   :config
;;   (setq aw-dispatch-always t
;;         aw-scope 'global
;;         aw-background nil
;;         aw-keys '(?q ?w ?e ?r ?t ?y ?u ?i ?p))
;;   (setq aw-dispatch-alist
;;         '((?k aw-delete-window "Delete Window")
;;           (?m aw-swap-window "Swap Windows")
;;           (?M aw-move-window "Move Window")
;;           (?c aw-copy-window "Copy Window")
;;           (?j aw-switch-buffer-in-window "Select Buffer")
;;           (?\t aw-flip-window)
;;           (?b aw-switch-buffer-other-window "Switch Buffer Other Window")
;;           (?c aw-split-window-fair "Split Fair Window")
;;           (?s aw-split-window-vert "Split Vert Window")
;;           (?v aw-split-window-horz "Split Horz Window")
;;           (?o delete-other-windows "Delete Other Windows")
;;           (?? aw-show-dispatch-help)))
;;   (defun my/other-window-prev (&optional arg all-frames)
;;     (interactive "p")
;;     (other-window (if arg (- arg) -1) all-frames)))

;; (use-package! emacs
;;   :config
;;   (defun my/enlarge-window-horizontally (&optional repeat)
;;     "Enlarge window horizontally by 8% of the frame width."
;;     (interactive "p")
;;     (enlarge-window-horizontally (* (or repeat 1)
;;                                     (floor (frame-width) 20))))
;;   (defun my/shrink-window-horizontally (&optional repeat)
;;     "Enlarge window horizontally by 8% of the frame width."
;;     (interactive "p")
;;     (shrink-window-horizontally (* (or repeat 1)
;;                                    (floor (frame-width) 20))))
;;   (defun my/shrink-window (&optional repeat)
;;     "Enlarge window horizontally by 8% of the frame height."
;;     (interactive "p")
;;     (shrink-window (* (or repeat 1)
;;                       (floor (frame-height) 20))))
;;   (defun my/enlarge-window (&optional repeat)
;;     "Enlarge window horizontally by 8% of the frame height."
;;     (interactive "p")
;;     (enlarge-window (* (or repeat 1)
;;                        (floor (frame-height) 20))))
;;   :bind
;;   (("<C-S-right>" . my/enlarge-window-horizontally)
;;    ("<C-S-left>"  . my/shrink-window-horizontally)
;;    ("<C-S-up>"    . my/enlarge-window)
;;    ("<C-S-down>"  . my/shrink-window)))

;;----------------------------------------------------------------
;; ** Windmove
;;----------------------------------------------------------------

(use-package! windmove
  :bind
  (("s-S-<right>" . windmove-swap-states-right)
   ("s-S-<down>" . windmove-swap-states-down)
   ("s-S-<up>" . windmove-swap-states-up)
   ("s-S-<left>" . windmove-swap-states-left)))
  ;; :config
  ;; (use-package! emacs-i3))

;;----------------------------------------------------------------
;; ** Transpose-frame
;;----------------------------------------------------------------

(use-package! transpose-frame
  ;; :ensure t
  :bind (("H-\\" . rotate-frame-anticlockwise)
         :map ctl-x-4-map
         ("|" . flop-frame)
         ("_" . flip-frame)
         ("\\" . rotate-frame-anticlockwise)))

;;----------------------------------------------------------------
;; ** Auto-revert
;;----------------------------------------------------------------

(use-package! autorevert
  :hook ((prog-mode
          text-mode
          tex-mode
          org-mode
          conf-mode) . auto-revert-mode))

;;----------------------------------------------------------------
;; ** Re-Builder
;;----------------------------------------------------------------
(use-package! re-builder
  :bind (("C-M-5" . re-builder)
         ("C-M-%" . re-builder)
         :map reb-mode-map
         ("C-c C-k" . reb-quit)
         ("RET" . reb-replace-regexp)
         :map reb-lisp-mode-map
         ("RET" . reb-replace-regexp))
  :config
  ;; reb-fix modifies reb-update-overlays to restrict matches to region
  (load "~/.config/doom/lisp/reb-fix")
  (use-package! reb-fix)
  (defvar my/re-builder-positions nil
    "Store point and region bounds before calling re-builder")
  (advice-add 're-builder
              :before
              (defun my/re-builder-save-state (&rest _)
                "Save into `my/re-builder-positions' the point and region
positions before calling `re-builder'."
                (setq my/re-builder-positions
                      (cons (point)
                            (when (region-active-p)
                              (list (region-beginning)
                                    (region-end)))))))
  (defun reb-replace-regexp (&optional delimited)
  "Run `query-replace-regexp' with the contents of re-builder. With
non-nil optional argument DELIMITED, only replace matches
surrounded by word boundaries."
  (interactive "P")
  (reb-update-regexp)
  (let* ((re (reb-target-binding reb-regexp))
         (replacement (query-replace-read-to
                       re
                       (concat "Query replace"
		               (if current-prefix-arg
		                   (if (eq current-prefix-arg '-) " backward" " word")
		                 "")
		               " regexp"
		               (if (with-selected-window reb-target-window
                                     (region-active-p)) " in region" ""))
                       t))
         (pnt (car my/re-builder-positions))
         (beg (cadr my/re-builder-positions))
         (end (caddr my/re-builder-positions)))
    (with-selected-window reb-target-window
      (goto-char pnt)
      (setq my/re-builder-positions nil)
      (reb-quit)
      (query-replace-regexp re replacement delimited beg end)))))
