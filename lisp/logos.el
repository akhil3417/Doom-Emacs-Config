;;; logos.el --- Simple focus mode and extras -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://gitlab.com/protesilaos/logos
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, focus, writing, presentation, narrowing

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package provides a simple "focus mode" which can be applied to
;; any buffer for reading, writing, or even doing a presentation.  The
;; buffer can be divided in pages using the `page-delimiter', outline
;; structure, or any other pattern.  Commands are provided to move
;; between those pages.  These motions work even when narrowing is in
;; effect (and they preserve it).  `logos.el' is designed to be simple
;; by default and easy to extend.  This manual provides concrete
;; examples to that end.
;;
;; Logos does not define any key bindings.  Try something like this:
;;
;;     (let ((map global-map))
;;       (define-key map [remap narrow-to-region] #'logos-narrow-dwim)
;;       (define-key map [remap forward-page] #'logos-forward-page-dwim)
;;       (define-key map [remap backward-page] #'logos-backward-page-dwim))
;;
;; By default those key bindings are: C-x n n, C-x ], C-x [.
;;
;; The `logos-focus-mode' tweaks the aesthetics of the current buffer.
;; When enabled it sets the buffer-local value of these user options:
;; `logos-scroll-lock', `logos-variable-pitch',`logos-hide-mode-line',
;; `logos-indicate-buffer-boundaries', `logos-buffer-read-only', and
;; `logos-olivetti'.
;;
;; Logos is the familiar word derived from Greek (watch my presentation
;; on philosophy about Cosmos, Logos, and the living universe:
;; <https://protesilaos.com/books/2022-02-05-cosmos-logos-living-universe/>),
;; though it also stands for these two perhaps equally insightful
;; backronyms about the mechanics of this package:
;;
;; 1. ^L Only Generates Ostensible Slides
;; 2. Logos Optionally Garners Outline Sections
;;
;; Consult the manual for all sorts of tweaks and extras:
;; <https://protesilaos.com/emacs/logos>.

;;; Code:

;;;; General utilities

(defgroup logos ()
  "Simple focus mode and extras."
  :group 'editing)

(defcustom logos-outlines-are-pages nil
  "When non-nil, every outline heading is a page delimiter.
What constitutes an outline is determined by the user option
`logos-outline-regexp-alist'.

When this variable is nil, pages are demarcated by the
`page-delimiter'."
  :type 'boolean
  :group 'logos)

(defconst logos--page-delimiter (default-value 'page-delimiter)
  "The default value of `page-delimiter'.")

(defcustom logos-outline-regexp-alist   ; TODO 2022-03-02: more sensible outlines?
  `((emacs-lisp-mode . "^;;;+ ")
    (org-mode . "^\\*+ +")
    (t . ,(or outline-regexp logos--page-delimiter)))
  "Alist of major mode and regular expression of the outline.
Only used when `logos-outlines-are-pages' is non-nil.

The major mode also targets any of its derivatives.  For example,
`lisp-interaction-mode' (the standard scratch buffer) is based on
`emacs-lisp-mode' so one only needs to set the outline regexp of
the latter."
  :type `(alist :key-type symbol :value-type string) ; TODO 2022-03-02: ensure symbol is mode?
  :group 'logos)

(defcustom logos-hide-mode-line nil
  "When non-nil hide the modeline.
This is only relevant when `logos-focus-mode' is enabled."
  :type 'boolean
  :group 'logos
  :local t)

(defcustom logos-scroll-lock nil
  "When non-nil, use `scroll-lock-mode'.
This is only relevant when `logos-focus-mode' is enabled."
  :type 'boolean
  :group 'logos
  :local t)

(defcustom logos-variable-pitch nil
  "When non-nil, `text-mode' buffers use `variable-pitch-mode'.
In programming modes the default font is always used, as that is
assumed to be a monospaced typeface.

This is only relevant when `logos-focus-mode' is enabled."
  :type 'boolean
  :group 'logos
  :local t)

(defcustom logos-indicate-buffer-boundaries nil
  "If non-nil locally disable `indicate-buffer-boundaries'.
This is only relevant when `logos-focus-mode' is enabled."
  :type 'boolean
  :group 'logos
  :local t)

(defcustom logos-buffer-read-only nil
  "If non-nil make buffer read-only.
This applies when `logos-focus-mode' is enabled."
  :type 'boolean
  :group 'logos
  :local t)

(defcustom logos-olivetti nil
  "If non-nil center buffer in its window with Olivetti package.
This is only relevant when `logos-focus-mode' is enabled."
  :type 'boolean
  :group 'logos
  :local t)

;;;; General utilities

(defun logos--focus-p ()
  "Return non-nil if `logos-focus-mode' is bound locally."
  (when (bound-and-true-p logos-focus-mode)
    (buffer-local-value 'logos-focus-mode (current-buffer))))

;;;; Page motions

(defun logos--outline-regexp ()
  "Return page delimiter from `logos-outline-regexp-alist'."
  (let ((outline logos-outline-regexp-alist)
        (mode major-mode))
    (or (alist-get mode outline)
        (alist-get (get mode 'derived-mode-parent) outline)
        (alist-get t outline))))

(defun logos--page-delimiter ()
  "Determine the `page-delimiter'."
  (if logos-outlines-are-pages
      (setq-local page-delimiter (logos--outline-regexp))
    (setq-local page-delimiter logos--page-delimiter)))

(defun logos--narrow-to-page (count &optional back)
  "Narrow to COUNTth page with optional BACK motion."
  (if back
      (narrow-to-page (or (- count) -1))
    (narrow-to-page (or (abs count) 1)))
  ;; Avoids the problem of skipping pages while cycling back and forth.
  (goto-char (point-min)))

(defvar logos-page-motion-hook nil
  "Hook that runs after a page motion.
See `logos-forward-page-dwim' or `logos-backward-page-dwim'.")

(defun logos--page-motion (&optional count back)
  "Routine for page motions.
With optional numeric COUNT move by that many pages.  With
optional BACK perform the motion backwards."
  (let ((cmd (if back #'backward-page #'forward-page)))
    (logos--page-delimiter)
    (if (buffer-narrowed-p)
        (logos--narrow-to-page count back)
      (funcall cmd count)
      (setq this-command cmd))
    (run-hooks 'logos-page-motion-hook)))

;;;###autoload
(defun logos-forward-page-dwim (&optional count)
  "Move to next or COUNTth page forward.
If the buffer is narrowed, keep the effect while performing the
motion.  Always move point to the beginning of the narrowed
page."
  (interactive "p")
  (logos--page-motion count))

;;;###autoload
(defun logos-backward-page-dwim (&optional count)
  "Move to previous or COUNTth page backward.
If the buffer is narrowed, keep the effect while performing the
motion.  Always move point to the beginning of the narrowed
page."
  (interactive "p")
  (logos--page-motion count :back))

(declare-function org-at-heading-p "org" (&optional _))
(declare-function org-show-entry "org")
(declare-function outline-on-heading-p "outline" (&optional invisible-ok))
(declare-function outline-show-entry "outline")

(defun logos--reveal-entry ()
  "Reveal Org or Outline entry."
  (cond
   ((and (eq major-mode 'org-mode)
         (org-at-heading-p))
    (org-show-entry))
   ((and (or (eq major-mode 'outline-mode)
             (bound-and-true-p outline-minor-mode))
         (outline-on-heading-p))
    (outline-show-entry))))

(add-hook 'logos-page-motion-hook #'logos--reveal-entry)

;;;; Narrowing
;; NOTE 2022-03-02: This section is most likely unnecessary, but let's
;; keep it for now.

(defun logos--window-bounds ()
  "Determine start and end points in the window."
  (list (window-start) (window-end)))

(defun logos--page-p ()
  "Return non-nil if there is a `page-delimiter' in the buffer.
This function does not use `widen': it only checks the accessible
portion of the buffer."
  (or (save-excursion (re-search-forward page-delimiter nil t))
      (save-excursion (re-search-backward page-delimiter nil t))))

(defun logos-narrow-visible-window ()
  "Narrow buffer to visible window area.
Also check `logos-narrow-dwim'."
  (interactive)
  (let* ((bounds (logos--window-bounds))
         (window-area (- (cadr bounds) (car bounds)))
         (buffer-area (- (point-max) (point-min))))
    (if (/= buffer-area window-area)
        (narrow-to-region (car bounds) (cadr bounds))
      (user-error "Buffer fits in the window; won't narrow"))))

;;;###autoload
(defun logos-narrow-dwim ()
  "Do-what-I-mean narrowing.

If region is active, narrow the buffer to the region's
boundaries.

If pages are defined by virtue of `logos--page-p', narrow to
the current page boundaries.

If no region is active and no pages exist, narrow to the visible
portion of the window.

If narrowing is in effect, widen the view."
  (interactive)
  (unless mark-ring                  ; needed when entering a new buffer
    (push-mark (point) t nil))
  (cond
   ((and (use-region-p)
         (null (buffer-narrowed-p)))
    (narrow-to-region (region-beginning) (region-end)))
   ((logos--page-p)
    (narrow-to-page))
   ((null (buffer-narrowed-p))
    (logos-narrow-visible-window))
   ((widen))))

;;;; Optional "focus mode" and utilities

;; I learnt about the method of using `logos--mode' and `logos--set'
;; from Daniel Mendler: <https://github.com/minad>.
(defvar-local logos--restore nil)

(defun logos--mode (mode arg)
  "Set MODE to ARG.
ARG is either 1 or -1.  The current value changes to its
alternate, thus toggling MODE."
  (let ((old (if (and (boundp mode) (symbol-value mode)) 1 -1)))
    (unless (eq old arg)
      (push (lambda () (funcall mode old)) logos--restore)
      (funcall mode arg))))

(defun logos--set (var val)
  "Set VAR to buffer-local VAL."
  (let ((old (and (boundp var) (symbol-value var))))
    (unless (equal old val)
      (set var val)
      (if (local-variable-p var)
          (push (lambda () (set var old)) logos--restore)
        (make-local-variable var)
        (push (lambda () (kill-local-variable var)) logos--restore)))))

(define-minor-mode logos-focus-mode
  "Buffer-local mode for focused editing.
When enabled it sets the buffer-local value of these user
options: `logos-scroll-lock', `logos-variable-pitch',
`logos-hide-mode-line'."
  :init-value nil
  :global nil
  :lighter " Λ" ; lambda majuscule
  (mapc #'funcall logos--restore)
  (setq logos--restore nil)
  (when logos-focus-mode
    (logos--setup)))

(defun logos--setup ()
  "Set up aesthetics for presentation."
  ;; modes
  (logos--variable-pitch)
  (logos--scroll-lock)
  (logos--olivetti)
  ;; variables
  (logos--hide-mode-line)
  (logos--indicate-buffer-boundaries)
  (logos--buffer-read-only))

(defun logos--variable-pitch ()
  "Set `logos-variable-pitch'."
  (when (and logos-variable-pitch (derived-mode-p 'text-mode))
    (logos--mode 'variable-pitch-mode 1)))

(defun logos--scroll-lock ()
  "Set `logos-scroll-lock'."
  (when logos-scroll-lock
    (logos--mode 'scroll-lock-mode 1)))

(defun logos--indicate-buffer-boundaries ()
  "Set `logos-indicate-buffer-boundaries'."
  (when logos-indicate-buffer-boundaries
    (logos--set 'indicate-buffer-boundaries nil)))

;; FIXME 2022-03-13: The mode line is not redrawn properly.  Not even
;; with `force-mode-line-update', unless something happens like
;; switching to the other window.  Using `redisplay' does not fix the
;; issue.  I can reproduce the problem on both Emacs 29 and 27.
;;
;; When using `logos-olivetti' the problem no longer occurs, presumably
;; because Olivetti triggers some kind of redraw.  Which one?
(defun logos--hide-mode-line ()
  "Set `logos-hide-mode-line'."
  (when logos-hide-mode-line
    (logos--set 'mode-line-format nil)))

(defun logos--buffer-read-only ()
  "Set `logos-buffer-read-only'."
  (when logos-buffer-read-only
    (logos--set 'buffer-read-only t)))

(defun logos--olivetti ()
  "Set `logos-olivetti'."
  (when (and logos-olivetti (require 'olivetti nil t))
    (logos--mode 'olivetti-mode 1)))

(provide 'logos)
;;; logos.el ends here
