;;; lisp/new/editing.el -*- lexical-binding: t; -*-


(global-evil-mc-mode  1)
(with-eval-after-load 'evil-maps
  (global-set-key (kbd "M-D") 'evil-mc-make-and-goto-next-match)
  (define-key evil-normal-state-map (kbd "C-J") 'evil-mc-make-cursor-move-next-line)
  (define-key evil-normal-state-map (kbd "C-K") 'evil-mc-make-cursor-move-prev-line)

  (define-key evil-visual-state-map (kbd "M-D") 'evil-mc-make-and-goto-next-match)
  (define-key evil-normal-state-map (kbd "M-D") 'evil-mc-make-and-goto-next-match))

(defun +make-evil-multiedit-case-sensitive (fn &rest args)
  (let ((case-fold-search (not iedit-case-sensitive)))
    (apply fn args)))

(advice-add #'evil-multiedit-match-and-next :around #'+make-evil-multiedit-case-sensitive)

(define-key evil-normal-state-map (kbd "C-t") 'transpose-chars)

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

;; [[file:config.org::*Unbind certain Emacs keybindings in =evil-mode=][Unbind certain Emacs keybindings in =evil-mode=:5]]
(define-key global-map (kbd "<tab>") nil)
(define-key evil-insert-state-map (kbd "<tab>") nil)
(evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
;; Unbind certain Emacs keybindings in =evil-mode=:5 ends here

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

;; (setq ispell-program-name "aspell"
;;       ispell-extra-args '("--sug-mode=ultra" "--lang=en_us"))
;; ispell-dictionary "en"
(setq ispell-dictionary "en-custom")
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(setq ispell-personal-dictionary (expand-file-name ".ispell_personal" doom-user-dir))
(setq ispell-hunspell-dictionary-alist "/usr/share/myspell/en-custom.dic")
