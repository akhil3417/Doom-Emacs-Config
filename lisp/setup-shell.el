;; -*- lexical-binding: t -*-
(use-package! fish-completion
  :disabled
  :when (executable-find "fish")
  :hook ((eshell-mode shell-mode) . fish-completion-mode)
  :config
  (defun fish-completion--list-completions (raw-prompt)
    (mapcar (lambda (e)
              (string-match "\\`\\([^\t]*\\)\t?\\(.*\\)\\'" e)
              (propertize (match-string 1 e) 'fish-completion--annotation (match-string 2 e)))
            (split-string
             (fish-completion--list-completions-with-desc raw-prompt)
             "\n" t)))

  (defun fish-completion--annotate (cand)
    (when-let* ((pos (or (next-single-property-change 0 'fish-completion--annotation cand)
                         0))
                (ann (get-text-property pos 'fish-completion--annotation cand)))
      (concat (propertize " " 'display '(space :align-to center)) ann)))

  (defun fish-completion--provide-annotation-function (table)
    (nconc table (list :annotation-function #'fish-completion--annotate)))

  (advice-remove 'pcomplete-completions-at-point #'fish-completion--provide-annotation-function)
  (advice-add #'pcomplete-completions-at-point
              :filter-return #'fish-completion--provide-annotation-function)

  (with-eval-after-load 'eshell
    (setq eshell-command-completion-function
          (lambda ()
            (pcomplete-here
             (my/eshell-fish-complete-commands-list))))
    (defun my/eshell-fish-complete-commands-list ()
      "Gerenate list of appliclable, visible commands by combining
Eshell specific completions with those returned by fish shell.

Falls back to native Eshell completion if fish-completion is not available.

Filenames are always matched by eshell."
      (if (fboundp 'fish-completion--list-completions)
          (let ((filename (pcomplete-arg)) glob-name)
            (if (file-name-directory filename)
                (if eshell-force-execution
                    (pcomplete-dirs-or-entries nil #'file-readable-p)
                  (pcomplete-executables))
              (if (and (> (length filename) 0)
	               (eq (aref filename 0) eshell-explicit-command-char))
	          (setq filename (substring filename 1)
		        pcomplete-stub filename
		        glob-name t))
              (let ((completions (fish-completion--list-completions filename)))
                ;; Add aliases which are currently visible, and Lisp functions.
	        ;; (pcomplete-uniquify-list)
	        (if glob-name
	            completions
	          (setq completions
		        (append (if (fboundp 'eshell-alias-completions)
			            (eshell-alias-completions filename))
			        (eshell-winnow-list
			         (mapcar
			          (function
			           (lambda (name)
			             (substring name 7)))
			          (all-completions (concat "eshell/" filename)
					           obarray #'functionp))
			         nil '(eshell-find-alias-function))
			        completions))
	          (append (and (or eshell-show-lisp-completions
			           (and eshell-show-lisp-alternatives
				        (null completions)))
			       (all-completions filename obarray #'functionp))
                          completions)))))
        (eshell-complete-commands-list)))))

;; Superceded by pcmpl-args
(use-package! pcomplete
  :disabled
  :config
  (defun pcomplete/xargs ()
    "Completion for `xargs'."
    (while (string-prefix-p "-" (pcomplete-arg 0))
      (funcall pcomplete-default-completion-function))
    (funcall pcomplete-command-completion-function)
    (funcall (or (pcomplete-find-completion-function (pcomplete-arg 1))
	         pcomplete-default-completion-function)))
  (defun pcomplete/time ()
    "Completion for `time'."
    (pcomplete-here (funcall pcomplete-command-completion-function))
    (funcall (or (pcomplete-find-completion-function (pcomplete-arg 1))
	         pcomplete-default-completion-function))))

(use-package! pcmpl-args
  :ensure
  :hook ((eshell-mode . my/pcmpl-args-pcomplete-settings)
         (eshell-first-time-mode . my/pcmpl-extras))
  :config
  (defun my/pcmpl-extras ()
    (dolist (cmd '("fd" "rg" "pacman" "systemctl"
                   "aura"))
      (defalias
        (intern (concat "pcomplete/" cmd))
        'pcmpl-args-pcomplete-on-man)))
  (defun my/pcmpl-args-pcomplete-settings ()
    (setq-local pcomplete-try-first-hook
                '(eshell-complete-host-reference
                  eshell-complete-history-reference
                  eshell-complete-user-reference
                  ;;eshell-complete-variable-assignment
                  eshell-complete-variable-reference
                  eshell-complete-lisp-symbols
                  t))))

;; ** Eshell built-ins customizations
(use-package! eshell
  :hook ((eshell-mode . my/eshell-keys-and-modes)
         (eshell-first-time-mode . my/eshell-first-load-settings))
                   ;; When calling dabbrev, hippie-expand uses strings
                   ;; containing words and symbols to:
                   ;;   1) determine the string to expand
                   ;;   2) determine what to expand it with
                   ;; (see hippie-expand-dabbrev-as-symbol)
                   ;; so for instance if I'm typing "curl foo/bar" on
                   ;; an eshell buffer, as "/" is a symbol in eshell
                   ;; mode, it will use "foo/bar" as string to
                   ;; expand. In some cases this is undesirable, for
                   ;; instance when completing URLs, as it's more
                   ;; likely that I'll want to expand the current
                   ;; component ("bar"), not the whole URL. Moving "/"
                   ;; to a non-symbol syntax class works around
                   ;; this. I can't just set
                   ;; hippie-expand-dabbrev-as-symbol to false because
                   ;; if I did h-e wouldn't expand FQDNs, i.e. "bar"
                   ;; would be expanded to "barhost" and not
                   ;; "barhost.example.org"
                   ;; (modify-syntax-entry ?/ "|"))
  :config
  (defun my/eshell-first-load-settings ()
    (setq eshell-visual-commands (append eshell-visual-commands
                                         '("btmenu" "fzf" "pulsemixer" "mpv"
                                           "ncmpcpp" "progress" "julia"
                                           "ranger" "watch" "bluetoothctl"))
          ;; eshell-input-filter-functions '(eshell-expand-history-references)
          eshell-hist-ignoredups t
          eshell-destroy-buffer-when-process-dies t
          eshell-directory-name (dir-concat user-cache-directory "~/eshell/")
          eshell-history-file-name (concat (file-name-as-directory
                                            eshell-directory-name)
                                           "history")
          eshell-last-dir-ring-file-name (concat (file-name-as-directory
                                            eshell-directory-name)
                                           "lastdir")
          eshell-history-size 4096
          eshell-glob-case-insensitive t
          eshell-error-if-no-glob t)
    (setq eshell-aliases-file
          (concat (file-name-as-directory
                   eshell-directory-name)
                  "alias"))
    (eshell-read-aliases-list))

  (defun my/eshell-keys-and-modes ()
    (setq outline-regexp eshell-prompt-regexp)
    (abbrev-mode 1)
    (define-key eshell-mode-map (kbd "s-<return>") 'my/delete-window-or-delete-frame)
    (define-key eshell-mode-map (kbd "M-s") nil)
    (define-key eshell-mode-map (kbd "C-c C-SPC") 'eshell-mark-output)
    (define-key eshell-mode-map (kbd "C-<return>") 'my/eshell-send-detached-input)
    (setq-local company-minimum-prefix-length 2)
    ;; (setq-local completion-in-region-function #'consult-completion-in-region)
    (setq eshell-cmpl-cycle-cutoff-length 2)))

;; ** Better paging
(load "em-pager")
(use-package! em-pager
  :disabled
  :after eshell
  :config
  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key eshell-mode-map [remap keyboard-quit]
                #'eshell-pager-quit)))
  ;; Setup the pager
  (remove-hook 'eshell-output-filter-functions #'eshell-postoutput-scroll-to-bottom)
  (add-hook 'eshell-output-filter-functions #'eshell-pager-reposition)
  (add-hook 'eshell-pre-command-hook #'eshell-pager-pre)
  (add-hook 'eshell-post-command-hook #'eshell-pager-post))

;; ** Better buffer redirection
(use-package! eshell
  :defer
  :config
  ;; From https://gist.github.com/minad/19df21c3edbd8232f3a7d5430daa103a
  (defun my/eshell-font-lock-and-pop (fun object target)
    (let* ((buf (and (markerp target) (marker-buffer target)))
           (str (and buf (stringp object) (string-match-p "\e\\[" object) object)))
      (funcall fun (if str (ansi-color-apply str) object) target)
      (when buf
        (with-current-buffer buf
          (goto-char (point-min))
          (font-lock-mode)
          (pop-to-buffer buf)))))
  (advice-add 'eshell-output-object-to-target
              :around #'my/eshell-font-lock-and-pop)

  ;; From https://emacs.stackexchange.com/questions/42113/customize-eshell-redirection-to-buffer
  (defun my/eshell-syntax-buffer-redirect ()
    "Parse buffer redirection > #buf and >#."
    (when (and (not eshell-current-argument)
               (not eshell-current-quoted)
               ;; Don't overwrite `eshell-parse-special-reference'
               (not (looking-at "#<\\(\\(buffer\\|process\\)\\s-\\)?"))
               (looking-at "#\\(\\S-+\\)?"))
      (goto-char (match-end 0)) ;; Go to the end of the match.
      (list #'get-buffer-create
            (or
             (match-string 1)
             (format "*eshell export: %s*"
                     (replace-regexp-in-string
                      "\\s-*>+\\s-*#.*\\'" ""
                      (buffer-substring-no-properties (line-beginning-position) (point))))))))
  (add-hook 'eshell-parse-argument-hook #'my/eshell-syntax-buffer-redirect))

;; ** Better eshell history management
(use-package! eshell
  :hook ((eshell-mode . my/eshell-hist-use-global-history)
         (eshell-pre-command . eshell-save-some-history)
         (eshell-pre-command . my/eshell-history-remove-duplicates))
  :config
  ;; https://gitlab.com/ambrevar/dotfiles/-/blob/master/.emacs.d/lisp/init-eshell.el
  (setq eshell-input-filter
        (lambda (str)
          (not (or
                ;; Here we can filter out failing commands.  This is usually a bad
                ;; idea since a lot of useful commands have non-zero exit codes
                ;; (including Emacs/Eshell functions).
                ;; (/= eshell-last-command-status 0)
                (string= "" str)
                (string= "cd" str)
                (string-prefix-p "cd " str)
                ;; Filter out space-beginning commands from history.
                (string-prefix-p " " str)))))

  ;; From https://gitlab.com/ambrevar/dotfiles/-/blob/master/.emacs.d/lisp/init-eshell.el
  (defvar my/eshell-history-global-ring nil
    "History ring shared across Eshell sessions.")

  (defun my/eshell-hist-use-global-history ()
    "Make Eshell history shared across different sessions."
    (unless my/eshell-history-global-ring
      (when eshell-history-file-name
        (eshell-read-history nil t))
      (setq my/eshell-history-global-ring (or eshell-history-ring (make-ring eshell-history-size))))
    (setq eshell-history-ring my/eshell-history-global-ring))

  (defun my/ring-delete-first-item-duplicates (ring)
    "Remove duplicates of last command in history.
Return RING.

This should be faster then `seq-uniq'.  Unlike
`eshell-hist-ignoredups' or `comint-input-ignoredups', it does
not allow duplicates ever.
Surrounding spaces are ignored when comparing."
    (let ((first (ring-ref ring 0))
          (index 1))
      (while (<= index (1- (ring-length ring)))
        (if (string= (string-trim first)
                     (string-trim (ring-ref ring index)))
            ;; REVIEW: We could stop at the first match, it would be faster and it
            ;; would eliminate duplicates if we started from a fresh history.
            ;; From an existing history that would not clean up existing
            ;; duplicates beyond the first one.
            (ring-remove ring index)
          (setq index (1+ index))))
      ring))

  (defun my/eshell-history-remove-duplicates ()
    (my/ring-delete-first-item-duplicates eshell-history-ring))

  (defun my/eshell-previous-matching-input ()
    (interactive)
    (when-let ((input (completing-read "History element: "
                                  (delete-dups (ring-elements eshell-history-ring))
                                  nil t
                                  (buffer-substring-no-properties
                                   eshell-last-output-end
                                   (point)))))
      (delete-region (save-excursion (eshell-bol)) (point))
      (insert input)))
  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key eshell-mode-map (kbd "M-r")
                'my/eshell-previous-matching-input))))

;; ** Custom functions for use in eshell
(use-package! eshell
  :defer
  :config
  (defalias 'eshell/v 'eshell-exec-visual)
  (defalias 'eshell/x #'eshell/exit)

  ;; From https://gist.github.com/minad/19df21c3edbd8232f3a7d5430daa103a
  (defun eshell/b (regexp)
    "Output buffer content of buffer matching REGEXP."
    (cl-loop for buf in (buffer-list)
             thereis
             (and (string-match-p regexp (buffer-name buf))
                  (with-current-buffer buf
                    (buffer-substring-no-properties (point-min) (point-max))))))

  ;; From https://github.com/LemonBreezes/.doom.d/blob/master/modules/private/eshell/autoload.el
  (defun eshell/hat (&rest files)
    "Output FILES with highlighting."
    (dolist (f files)
      (eshell-print (my/eshell-file-contents f))))

  (defun eshell/view (&optional file)
    (if (or (not file)
            (not (file-exists-p file))
            (file-directory-p file))
        (dired-other-window default-directory)
      (find-file-other-window file)
      (read-only-mode 1)
      (view-mode 1)))

  (defun eshell/z (&optional regexp)
    "Navigate to a previously visited directory in eshell."
    (let ((eshell-dirs (delete-dups (mapcar 'abbreviate-file-name
                                            (ring-elements eshell-last-dir-ring)))))
      (cond
       ((and (not regexp) (featurep 'consult-dir))
        (let* ((consult-dir--source-eshell `(:name "Eshell"
                                             :narrow ?e
                                             :category file
                                             :face consult-file
                                             :items ,eshell-dirs))
               (consult-dir-sources (cons consult-dir--source-eshell consult-dir-sources)))
          (eshell/cd (substring-no-properties (consult-dir--pick "Switch directory: ")))))
       (t (eshell/cd (if regexp (eshell-find-previous-directory regexp)
                       (completing-read "cd: " eshell-dirs)))))))

  ;; From https://protesilaos.com/dotemacs
  (defun my/eshell-export-output (&optional arg)
    "Export output of the last command to a buffer.
With prefix ARG, also copy the prompt and input."
    (interactive)
    (let ((orig (current-buffer))
          (beg (if arg (eshell-beginning-of-input)
                 (eshell-beginning-of-output)))
          (end (eshell-end-of-output))
          (buffer (get-buffer-create
                   (format "*eshell export: %s*"
                           (buffer-substring-no-properties
                            (eshell-beginning-of-input)
                            (1- (eshell-beginning-of-output)))))))
      (with-current-buffer buffer
        (font-lock-mode)
        (insert-buffer-substring orig beg end)
        (goto-char (point-min)))
      ;; Taken from `eshell-kill-output'
      (goto-char (eshell-beginning-of-output))
      (insert (format "Exported to %S\n" buffer))
      (delete-region (point) (eshell-end-of-output))
      (goto-char (point-max))
      (pop-to-buffer buffer)))

  (defun my/eshell-copy-output (&optional arg)
  "Copy output of the last command to the kill ring. With prefix
argument arg, Also copy the prompt and input."
  (interactive "P")
  (copy-region-as-kill (if arg (eshell-beginning-of-input)
                         (eshell-beginning-of-output))
                       (eshell-end-of-output))
  (message (if arg "Copied last input and output to kill ring."
             "Copied last output to kill ring.")))
  (add-hook
   'eshell-mode-hook
   (lambda ()
     (define-key eshell-mode-map (kbd "C-c M-w") 'my/eshell-copy-output)
     (define-key eshell-mode-map (kbd "C-c a C-l") 'my/eshell-export-output)))

  ;;From https://github.com/nbarrientos/dotfiles/.emacs.d/init.el
  (defun my/eshell-send-detached-input (&optional arg)
    "Send the current Eshell input to a compilation buffer.
With universal prefix argument bury the compilation buffer and
send a notification when the process has exited."
    (interactive "p")
    (let* ((cmd (buffer-substring
                 eshell-last-output-end (point-max)))
           (hostname (car (split-string
                           (or
                            (file-remote-p default-directory 'host)
                            (system-name))
                           "\\.")))
           (compile-command nil)
           (compilation-buffer-name-function
            (lambda (_major-mode)
              (format "D# %s (%s)" cmd hostname)))
           (compilation-buffer (compile cmd)))
      (when (equal arg 4)
        (with-current-buffer compilation-buffer
          (switch-to-prev-buffer (get-buffer-window (current-buffer)))
          (setq-local compilation-finish-functions
                      `((lambda (buffer str)
                          (notifications-notify
                           :body ,cmd
                           :timeout 8000
                           :category "detached_process"
                           :actions '("default" "Switch to buffer")
                           :on-action (lambda (id key) (switch-to-buffer-other-window ,(buffer-name compilation-buffer)))
                           :title (format "Process running in '%s' finished!" ,hostname)
                           :urgency (if (string-prefix-p "finished" str) 'normal 'critical)))))))
      (eshell-add-input-to-history cmd)
      (eshell-reset)))

  ;; From https://gist.github.com/minad/19df21c3edbd8232f3a7d5430daa103a
  (defun my/eshell-buffer-contents (buffer)
    "Return fontified buffer contents for BUFFER."
    (with-current-buffer buffer
      (font-lock-ensure (point-min) (point-max))
      (buffer-string)))

  ;; From https://gist.github.com/minad/19df21c3edbd8232f3a7d5430daa103a
  (defun my/eshell-file-contents (file)
    "Return fontified file contents for FILE."
    (let ((buffer (get-file-buffer file)))
      (if buffer
          (my/eshell-buffer-contents buffer)
        (unwind-protect
            (my/eshell-buffer-contents
             (setq buffer
                   (let ((inhibit-message t)
                         (non-essential t)
                         (enable-dir-local-variables nil)
                         (enable-local-variables (and enable-local-variables :safe)))
                     (find-file-noselect file))))
          (when buffer
            (kill-buffer buffer)))))))

  (defun my/eshell-kill-ring-save-outputs ()
    "Add to the kill ring CURRENT-PREFIX-ARG outputs, including prompts.
If no universal argument is passed, assume only one output"
    (interactive)
    (save-excursion
      (let (times)
        (if (or (null current-prefix-arg) (< current-prefix-arg 1))
            (setq times 1)
          (setq times current-prefix-arg))
        (eshell-previous-prompt times)
        (forward-line -1) ; Two lines prompt
        (beginning-of-line)
        (message (format "Shell output added to the kill ring (%d commands)" times))
        (kill-ring-save (point) (eshell-end-of-output)))))

(defun my/eshell-narrow-to-output ()
  "Narrow to the output of the command at point."
  (interactive)
  (save-excursion
    (let* ((start (progn (eshell-previous-prompt 1)
                         (forward-line +1)
                         (point)))
           (end (progn (eshell-next-prompt 1)
                       (forward-line -1)
                       (point))))
      (narrow-to-region start end))))

;; ** Calling and exiting eshell
(use-package! eshell
  :bind (("s-<return>" . eshell)
         ("s-!" . eshell-here))
  :config
  (advice-add 'eshell-life-is-too-much :after #'delete-window-if-not-single)
  (advice-add 'eshell-mark-output :after #'activate-mark)
  ;; From http://howardism.org/Technical/Emacs/eshell-fun.html
  (defun eshell-here ()
      "Opens up a new shell in the directory associated with the
    current buffer's file. The eshell is renamed to match that
    directory to make multiple eshell windows easier."
      (interactive)
      (let* ((parent (if (buffer-file-name)
                         (file-name-directory (buffer-file-name))
                       default-directory))
             (name   (car (last (split-string parent "/" t)))))
        (if-let* ((eshell-name (concat "*eshell: " name "*"))
                  (existing-eshell-buffer (get-buffer eshell-name)))
            (select-window (display-buffer existing-eshell-buffer))
          (select-window (display-buffer (eshell "new")))
          (rename-buffer eshell-name)
          (insert (concat "ls"))
          (eshell-send-input)))))

;; ** Eshell appearance and prompt
(use-package! eshell
  :defer
  :config
  (setq eshell-prompt-regexp "^.* λ "
          eshell-prompt-function #'my/eshell-default-prompt-fn)

  ;; From the Doom emacs config
  (setq eshell-banner-message
        '(format "%s %s\n"
                 (propertize (format " %s " (string-trim (buffer-name)))
                             'face 'mode-line-highlight)
                 (propertize (current-time-string)
                             'face 'font-lock-keyword-face)))

  (defun my/eshell-default-prompt-fn ()
    "Generate the prompt string for eshell. Use for `eshell-prompt-function'."
    (concat (if (bobp) "" "\n")
            (when (bound-and-true-p conda-env-current-name)
              (propertize (concat "(" conda-env-current-name ") ")
                          'face 'my/eshell-prompt-git-branch))
            (let ((pwd (eshell/pwd)))
              (propertize (if (equal pwd "~")
                              pwd
                            (abbreviate-file-name pwd))
                          'face 'my/eshell-prompt-pwd))
            (propertize (my/eshell--current-git-branch)
                        'face 'my/eshell-prompt-git-branch)
            (propertize " λ" 'face (if (zerop eshell-last-command-status) 'success 'error))
            " "))

  (defsubst my/eshell--current-git-branch ()
    ;; TODO Refactor me
    (cl-destructuring-bind (status . output)
        (with-temp-buffer (cons
                           (or (call-process "git" nil t nil "symbolic-ref" "-q" "--short" "HEAD")
                               (call-process "git" nil t nil "describe" "--all" "--always" "HEAD")
                               -1)
                           (string-trim (buffer-string))))
      (if (equal status 0)
          (format " [%s]" output)
        "")))

  (defface my/eshell-prompt-pwd '((t (:inherit font-lock-constant-face)))
    "TODO"
    :group 'eshell)

  (defface my/eshell-prompt-git-branch '((t (:inherit font-lock-builtin-face)))
    "TODO"
    :group 'eshell))

;; (use-package! eshell-bookmark
;;   :disabled
;;   :hook (eshell-mode . eshell-bookmark-setup))

(use-package! eshell-bookmark
  :after eshell
  :config
  (add-hook 'eshell-mode-hook #'eshell-bookmark-setup))

(use-package! shell
  :defer
  :config
  (setq async-shell-command-buffer 'new-buffer))

;; capf-autosuggest is a good idea, but it's too slow and laggy in practice.
(use-package! capf-autosuggest
  ;; :ensure t
  :disabled
  :hook ((comint-mode) . capf-autosuggest-mode))

(require 'em-cmpl)
(load "prot-eshell.el")
(use-package! prot-eshell
  :config
  (setq prot-eshell-output-buffer "*Exported Eshell output*")
  (setq prot-eshell-output-delimiter "* * *")
  (let ((map eshell-mode-map))
    (define-key map (kbd "M-k") #'eshell-kill-input)
    (define-key map (kbd "C-c a C-f") #'prot-eshell-ffap-find-file)
    (define-key map (kbd "C-c a C-j") #'prot-eshell-ffap-dired-jump)
    (define-key map (kbd "C-c a C-w") #'prot-eshell-ffap-kill-save)
    (define-key map (kbd "C-c a C-r") #'prot-eshell-redirect-to-buffer)
    (define-key map (kbd "C-c a C-e") #'prot-eshell-export)
    (define-key map (kbd "C-c a C-r") #'prot-eshell-root-dir))
  (let ((map eshell-cmpl-mode-map))
    (define-key map (kbd "C-c a TAB") #'prot-eshell-ffap-insert) ; C-c C-i
    (define-key map (kbd "C-c a C-h") #'prot-eshell-narrow-output-highlight-regexp))
  (let ((map eshell-hist-mode-map))
    (define-key map (kbd "M-s") #'nil) ; I use this prefix for lots of more useful commands
    ;; (define-key map (kbd "M-r") #'prot-eshell-complete-history)
    (define-key map (kbd "C-c a C-d") #'prot-eshell-complete-recent-dir)
    (define-key map (kbd "C-c a C-s") #'prot-eshell-find-subdirectory-recursive)))

;; (use-package! eshell-prompt-extras
;;   :after (eshell)
;;   :config
;;   (defun my/epe-theme-prompt ()
;;     (setq eshell-prompt-regexp "^λ ")
;;     (concat
;;      (let ((prompt-path (epe-fish-path (tramp-file-local-name (eshell/pwd)))))
;;        (format
;;         (epe-colorize-with-face "[%s]" 'epe-remote-face)
;;         (epe-colorize-with-face
;;          (if (string-empty-p prompt-path)
;;              "/"
;;            prompt-path)
;;          'epe-dir-face)))
;;      (if (epe-remote-p)
;;          (epe-colorize-with-face
;;           (concat "@" (epe-remote-host))
;;           'epe-remote-face)
;;        (epe-colorize-with-face
;;         (concat "@" (system-name))
;;         'epe-git-face))
;;      (if (eshell-exit-success-p)
;;          (epe-colorize-with-face "\nλ" 'success)
;;        (epe-colorize-with-face "\nλ" 'error))
;;      " "))
;;   (with-eval-after-load "esh-opt"
;;     (setq eshell-highlight-prompt nil
;;           eshell-prompt-function 'my/epe-theme-prompt)))
;;
(require 'em-cmpl)
(add-to-list 'load-path "~/.config/doom/lisp/")
(load "prot-eshell.el")
(use-package! prot-eshell
  :config
  (setq prot-eshell-output-buffer "*Exported Eshell output*")
  (setq prot-eshell-output-delimiter "* * *")
  (let ((map eshell-mode-map))
    (define-key map (kbd "M-k") #'eshell-kill-input)
    (define-key map (kbd "C-c a C-f") #'prot-eshell-ffap-find-file)
    (define-key map (kbd "C-c a C-j") #'prot-eshell-ffap-dired-jump)
    (define-key map (kbd "C-c a C-w") #'prot-eshell-ffap-kill-save)
    (define-key map (kbd "C-c a C-r") #'prot-eshell-redirect-to-buffer)
    (define-key map (kbd "C-c a C-e") #'prot-eshell-export)
    (define-key map (kbd "C-c a C-r") #'prot-eshell-root-dir))
  (let ((map eshell-cmpl-mode-map))
    (define-key map (kbd "C-c a TAB") #'prot-eshell-ffap-insert) ; C-c C-i
    (define-key map (kbd "C-c a C-h") #'prot-eshell-narrow-output-highlight-regexp))
  (let ((map eshell-hist-mode-map))
    (define-key map (kbd "M-s") #'nil) ; I use this prefix for lots of more useful commands
    ;; (define-key map (kbd "M-r") #'prot-eshell-complete-history)
    (define-key map (kbd "C-c a C-d") #'prot-eshell-complete-recent-dir)
    (define-key map (kbd "C-c a C-s") #'prot-eshell-find-subdirectory-recursive)))

;; I like to sync the $PWD with the buffer name, so an eshell in my home has as buffer name *eshell /home/op*.
;; <2021-06-16 Wed> Instead of advice-add I should use the eshell-directory-change-hook hook.
  (defun op/eshell-bufname (dir)
    (concat "*doom:eshell " (expand-file-name dir) "*"))

(defun op/eshell-after-cd (&rest _)
  (rename-buffer (op/eshell-bufname default-directory) t))

(advice-add #'eshell/cd :after #'op/eshell-after-cd)
;; To define custom commands in eshell (what would be functions or scripts in other shells), say whatever, one can define a eshell/whatever function. These are some aliases I find useful:

(defun eshell/emacs (&rest args)
  "Open a file in emacs (from the wiki)."
  (if (null args)
      (bury-buffer)
    (mapc #'find-file
          (mapcar #'expand-file-name
                  (eshell-flatten-list (nreverse args))))))

(defalias 'eshell/less #'find-file)

(defun eshell/dired ()
  (dired (eshell/pwd)))

(use-package! shell
  :config
  (setq shell-command-prompt-show-cwd t) ; Emacs 27.1
  (setq ansi-color-for-comint-mode t))

(provide 'setup-shell)
;; setup eshell:1 ends here
