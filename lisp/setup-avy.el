;;; lisp/setup-avy.el -*- lexical-binding: t; -*-


;; ** AVY
;;;----------------------------------------------------------------
(use-package! avy
  :defer t
  :commands (avy-goto-word-1 avy-goto-char-2 avy-goto-char-timer)
  :config
  (setq avy-timeout-seconds 0.24)

  (setq avy-all-windows nil) ; only the current window
  (setq avy-all-windows-alt t) ; all windows with C-u
  (setq avy-single-candidate-jump t)
  (setq avy-background nil)
  (setq avy-case-fold-search nil) ; case is significant
  (setq avy-timeout-seconds 0.5)
  (setq avy-style 'pre) ; prefixes candidate; otherwise use `at-full'
  ;; (define-key global-map (kbd "C-.") #'avy-goto-char-timer))

  (setq avy-keys '(?a ?h ?e ?t ?o ?d ?c
                   ?n ?b ?g ?, ?r ?j ?k ?, ?.
                   ?l ?q ?2 ?3 ?'))
  (setq avy-dispatch-alist '((?m . avy-action-mark)
                             (?   . avy-action-mark-to-char)
                             (?i . avy-action-ispell)
                             (?z . avy-action-zap-to-char)
                             (?o . avy-action-embark)
                             (?= . avy-action-define)
                             (67108925 . avy-action-tuxi)
                             ;; (?W . avy-action-tuxi)
                             (?h . avy-action-helpful)
                             (?x . avy-action-exchange)

                             (11 . avy-action-kill-line)
                             (25 . avy-action-yank-line)

                             (?w . avy-action-easy-copy)
                             ;; (134217847  . avy-action-easy-copy)
                             (?k . avy-action-kill-stay)
                             (?y . avy-action-yank)
                             (?t . avy-action-teleport)

                             (?W . avy-action-copy-whole-line)
                             (?K . avy-action-kill-whole-line)
                             (?Y . avy-action-yank-whole-line)
                             (?T . avy-action-teleport-whole-line)))

  (defun avy-action-easy-copy (pt)
        (require 'easy-kill)
        (goto-char pt)
        (cl-letf (((symbol-function 'easy-kill-activate-keymap)
                   (lambda ()
                     (let ((map (easy-kill-map)))
                       (set-transient-map
                        map
                        (lambda ()
                          ;; Prevent any error from activating the keymap forever.
                          (condition-case err
                              (or (and (not (easy-kill-exit-p this-command))
                                       (or (eq this-command
                                               (lookup-key map (this-single-command-keys)))
                                           (let ((cmd (key-binding
                                                       (this-single-command-keys) nil t)))
                                             (command-remapping cmd nil (list map)))))
                                  (ignore
                                   (easy-kill-destroy-candidate)
                                   (unless (or (easy-kill-get mark) (easy-kill-exit-p this-command))
                                     (easy-kill-save-candidate))))
                            (error (message "%s:%s" this-command (error-message-string err))
                                   nil)))
                        (lambda ()
                          (let ((dat (ring-ref avy-ring 0)))
                            (select-frame-set-input-focus
                             (window-frame (cdr dat)))
                            (select-window (cdr dat))
                            (goto-char (car dat)))))))))
          (easy-kill)))

  (defun avy-action-exchange (pt)
  "Exchange sexp at PT with the one at point."
  (set-mark pt)
  (transpose-sexps 0))

  (defun avy-action-helpful (pt)
    (save-excursion
      (goto-char pt)
      (helpful-at-point))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (defun avy-action-define (pt)
    (cl-letf (((symbol-function 'keyboard-quit)
            #'abort-recursive-edit))
      (save-excursion
        (goto-char pt)
        (dictionary-search-dwim))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (defun avy-action-tuxi (pt)
    (cl-letf (((symbol-function 'keyboard-quit)
            #'abort-recursive-edit))
      (save-excursion
        (goto-char pt)
        (google-search-at-point))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act)))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (defun avy-action-kill-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-line))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)

  (defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)

  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))

  (defun my/avy-goto-char-this-window (&optional arg)
    "Goto char in this window with hints."
    (interactive "P")
    (let ((avy-all-windows t)
          (current-prefix-arg (if arg 4)))
      (call-interactively 'avy-goto-word-1)))

  (defun my/avy-isearch (&optional arg)
    "Goto isearch candidate in this window with hints."
    (interactive "P")
    (let ((avy-all-windows)
          (current-prefix-arg (if arg 4)))
      (call-interactively 'avy-isearch)))

  (defun my/avy--read-char-2 (char1 char2)
    "Read two characters from the minibuffer."
    (interactive (list (let ((c1 (read-char "char 1: " t)))
                         (if (memq c1 '(? ?\b))
                             (keyboard-quit)
                           c1))
                       (let ((c2 (read-char "char 2: " t)))
                         (cond ((eq c2 ?)
                                (keyboard-quit))
                               ((memq c2 '(8 127))
                                (keyboard-escape-quit)
                                (call-interactively 'my/avy-next-char-2))
                               (t
                                c2)))))

    (when (eq char1 ?) (setq char1 ?\n))
    (when (eq char2 ?) (setq char2 ?\n))
    (string char1 char2))

  (defun my/avy-next-char-2 (&optional str2 arg)
    "Go to the next occurrence of two characters"
    (interactive (list
                  (call-interactively 'my/avy--read-char-2)
                  current-prefix-arg))
    (let* ((ev last-command-event)
           (echo-keystrokes nil))
      (push-mark (point) t)
      (if (search-forward str2 nil t
                           (+ (if (looking-at (regexp-quote str2))
                                  1 0)
                              (or arg 1)))
           (backward-char 2)
        (pop-mark)))

    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map (kbd ";") (lambda (&optional arg) (interactive)
                                   (my/avy-next-char-2 str2 arg)))
       (define-key map (kbd ",") (lambda (&optional arg) (interactive)
                                   (my/avy-previous-char-2 str2 arg)))
       map)))

  (defun my/avy-previous-char-2 (&optional str2 arg)
    "Go to the next occurrence of two characters"
       (interactive (list
                  (call-interactively 'my/avy--read-char-2)
                  current-prefix-arg))
       (let* ((ev last-command-event)
              (echo-keystrokes nil))
         (push-mark (point) t)
         (unless (search-backward str2 nil t (or arg 1))
           (pop-mark)))

    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map (kbd ";") (lambda (&optional arg) (interactive)
                                   (my/avy-next-char-2 str2 arg)))
       (define-key map (kbd ",") (lambda (&optional arg) (interactive)
                                   (my/avy-previous-char-2 str2 arg)))
       map)))

  (defun my/avy-copy-line-no-prompt (arg)
    (interactive "p")
    (avy-copy-line arg)
    (beginning-of-line)
    (zap-to-char 1 32)
    (delete-forward-char 1)
    (move-end-of-line 1))

  :general
  ("C-."        '(my/avy-goto-char-this-window :wk "Avy goto char")
   "M-s j"      '(avy-goto-char-2            :wk "Avy goto char 2")
   "M-s y"      '(avy-copy-line              :wk "Avy copy line above")
   "M-s M-y"    '(avy-copy-region            :wk "Avy copy region above")
   "M-s M-k"    '(avy-kill-whole-line        :wk "Avy copy line as kill")
   "M-j"        '(avy-goto-char-timer        :wk "Avy goto char timer")
   "M-s C-w"    '(avy-kill-region            :wk "Avy kill region")
   "M-s M-w"    '(avy-kill-ring-save-region  :wk "Avy copy as kill")
   "M-s t"      '(avy-move-line              :wk "Avy move line")
   "M-s M-t"    '(avy-move-region            :wk "Avy move region")
   "M-s s"      '(my/avy-next-char-2         :wk "Avy snipe forward")
   "M-s r"      '(my/avy-previous-char-2     :wk "Avy snipe backward")
   "M-g l"      '(avy-goto-end-of-line       :wk "Avy goto line")
   "M-s z"      '(my/avy-copy-line-no-prompt :wk "Avy copy and zap"))
  ;; (:states '(normal visual)
  ;;  :prefix "g"
  ;;  "s" 'avy-goto-char-timer)
  :bind (:map isearch-mode-map
         ("C-." . my/avy-isearch)
         ("C-j" . my/avy-isearch)
         ("M-j" . my/avy-isearch)))
