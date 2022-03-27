;;; lin.el --- Make `hl-line-mode' more suitable for selection UIs -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://gitlab.com/protesilaos/lin
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, faces, theme

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
;; Lin is a stylistic enhancement for Emacs' built-in `hl-line-mode'.
;; It remaps the `hl-line face (or equivalent) buffer-locally to a style
;; that is optimal for major modes where line selection is the primary
;; mode of interaction.
;;
;; The idea is that `hl-line-mode' cannot work equally well for contexts
;; with competing priorities: (i) line selection, or (ii) simple line
;; highlight.  In the former case, the current line needs to be made
;; prominent because it carries a specific meaning of some significance in
;; the given context: the user has to select a line.  Whereas in the latter
;; case, the primary mode of interaction does not revolve around the line
;; highlight itself: it may be because the focus is on editing text or
;; reading through the buffer's contents, so the current line highlight is
;; more of a reminder of the point's location on the vertical axis.
;;
;; `lin-mode' enables `hl-line-mode' by adding it to every hook specified
;; in the user option `lin-mode-hooks'.  Users are advised to configure
;; that variable with `customize-set-variable', or the Custom UI, or
;; equivalent as it has a function which automatically sets up Lin.  Those
;; who prefer to set values with `setq' must handle the process manually,
;; by using the `lin-setup' function.
;;
;; Users can selected their preferred style by customizing the user
;; option `lin-face'.  Options include the faces `lin-red', `lin-green',
;; `lin-yellow', `lin-blue' (default), `lin-magenta', `lin-cyan',
;; `lin-mac', `lin-red-override-fg', `lin-green-override-fg',
;; `lin-yellow-override-fg', `lin-blue-override-fg',
;; `lin-magenta-override-fg', `lin-cyan-override-fg',
;; `lin-mac-override-fg', or any other face that preferably has a
;; background attribute.  The Lin faces with the =-override-fg= suffix
;; set a foreground value which replaces that of the underlying text.
;; Whereas the others only specify a background attribute.
;;
;; Sample configuration:
;;
;;     (require 'lin)
;;
;;     (setq lin-face 'lin-blue)
;;
;;     (lin-setup)
;;
;;     (customize-set-variable
;;      'lin-mode-hooks
;;      '(dired-mode-hook
;;        elfeed-search-mode-hook
;;        git-rebase-mode-hook
;;        ibuffer-mode-hook
;;        ilist-mode-hook
;;        ledger-report-mode-hook
;;        log-view-mode-hook
;;        magit-log-mode-hook
;;        mu4e-headers-mode
;;        notmuch-search-mode-hook
;;        notmuch-tree-mode-hook
;;        occur-mode-hook
;;        org-agenda-mode-hook
;;        tabulated-list-mode-hook))
;;
;; Consult the manual for further details.  Or visit the documentation's
;; web page: <https://protesilaos.com/emacs/lin>.

;;; Code:

(require 'face-remap)
(require 'hl-line)

(make-obsolete 'lin--default-foreign-hooks 'lin-mode-hooks "0.2.0")

(defgroup lin ()
  "Make `hl-line' appropriate for selection UIs."
  :group 'convenience)

(defcustom lin-mode-hooks
  '(bongo-mode-hook
    dired-mode-hook
    elfeed-search-mode-hook
    git-rebase-mode-hook
    ibuffer-mode-hook
    ilist-mode-hook
    ledger-report-mode-hook
    log-view-mode-hook
    magit-log-mode-hook
    mu4e-headers-mode
    notmuch-search-mode-hook
    notmuch-tree-mode-hook
    occur-mode-hook
    org-agenda-mode-hook
    tabulated-list-mode-hook)
  "List of hooks that should enable `lin-mode'.

When Lin is set up, it activates `hl-line-mode' and remaps its
face to `lin-face'.  This makes it possible to distinguish
between the two use-cases of permanent line highlighting: (i)
gentle reminder of where the point is while editing, (ii) current
selection.

Set this user option with `customize-set-variable', the Custom
UI, or equivalent.  It has a custom setter function which
automatically sets things up when configured that way.  Users who
prefer to use `setq' must run `lin-setup' manually.  Consult its
doc string."
  :type '(repeat variable)
  :initialize #'custom-initialize-default
  :set (lambda (symbol value)
         (if (eq value (default-value symbol))
             (set-default symbol value)
           (lin-setup 'reverse)
           (set-default symbol value)
           (lin-setup)))
  :group 'lin)

(defcustom lin-face 'lin-blue
  "Face to use for the pulse line.
Users can select one among `lin-red', `lin-green', `lin-yellow',
`lin-blue' (default), `lin-magenta', `lin-cyan', `lin-mac',
`lin-red-override-fg', `lin-green-override-fg',
`lin-yellow-override-fg', `lin-blue-override-fg',
`lin-magenta-override-fg', `lin-cyan-override-fg',
`lin-mac-override-fg', or any other face that preferably has a
background attribute.

Set this user option with `customize-set-variable', the Custom
UI, or equivalent.  It has a custom setter function which
automatically sets things up when configured that way.  Users who
prefer to use `setq' must run `lin-restart-mode-in-buffers'
manually.  Consult its doc string."
  :type '(radio (face :tag "Red style" lin-red)
                (face :tag "Green style" lin-green)
                (face :tag "Yellow style" lin-yellow)
                (face :tag "Blue style (default)" lin-blue)
                (face :tag "Magenta style" lin-magenta)
                (face :tag "Cyan style" lin-cyan)
                (face :tag "macOS style" lin-mac)
                (face :tag "Red style that also overrides the underlying foreground" lin-red-override-fg)
                (face :tag "Green style that also overrides the underlying foreground" lin-green-override-fg)
                (face :tag "Yellow style that also overrides the underlying foreground" lin-yellow-override-fg)
                (face :tag "Blue style that also overrides the underlying foreground" lin-blue-override-fg)
                (face :tag "Magenta style that also overrides the underlying foreground" lin-magenta-override-fg)
                (face :tag "Cyan style that also overrides the underlying foreground" lin-cyan-override-fg)
                (face :tag "macOS style that also overrides the underlying foreground" lin-mac-override-fg)
                (face :tag "Other face (must have a background)"))
  :initialize #'custom-initialize-default
  :set (lambda (symbol value)
         (set-default symbol value)
         (lin-restart-mode-in-buffers))
  :group 'lin)

;;;; Faces

(defgroup lin-faces ()
  "Faces for `lin.el'."
  :group 'lin)

(defface lin-red
  '((default :foreground unspecified :underline nil :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#ffd3d3")
    (((class color) (min-colors 88) (background dark))
     :background "#500f0f")
    (t :background "red"))
  "Alternative red face for `lin-face'."
  :group 'lin-faces)

(defface lin-red-override-fg
  '((default :inherit lin-red)
    (((background light))
     :foreground "black")
    (((background dark))
     :foreground "white"))
  "Like `lin-red' but also sets a foreground."
  :group 'lin-faces)

(defface lin-green
  '((default :foreground unspecified :underline nil :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#bfeabf")
    (((class color) (min-colors 88) (background dark))
     :background "#0f300f")
    (t :background "green"))
  "Alternative green face for `lin-face'."
  :group 'lin-faces)

(defface lin-green-override-fg
  '((default :inherit lin-green)
    (((background light))
     :foreground "black")
    (((background dark))
     :foreground "white"))
  "Like `lin-green' but also sets a foreground."
  :group 'lin-faces)

(defface lin-yellow
  '((default :foreground unspecified :underline nil :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#ffecaa")
    (((class color) (min-colors 88) (background dark))
     :background "#412200")
    (t :background "yellow"))
  "Alternative yellow face for `lin-face'."
  :group 'lin-faces)

(defface lin-yellow-override-fg
  '((default :inherit lin-yellow)
    (((background light))
     :foreground "black")
    (((background dark))
     :foreground "white"))
  "Like `lin-yellow' but also sets a foreground."
  :group 'lin-faces)

(defface lin-blue
  '((default :foreground unspecified :underline nil :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#c0e4ff")
    (((class color) (min-colors 88) (background dark))
     :background "#002460")
    (t :background "blue"))
  "Alternative blue face for `lin-face'."
  :group 'lin-faces)

(defface lin-blue-override-fg
  '((default :inherit lin-blue)
    (((background light))
     :foreground "black")
    (((background dark))
     :foreground "white"))
  "Like `lin-blue' but also sets a foreground."
  :group 'lin-faces)

(defface lin-magenta
  '((default :foreground unspecified :underline nil :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#ffd9ff")
    (((class color) (min-colors 88) (background dark))
     :background "#401d40")
    (t :background "magenta"))
  "Alternative magenta face for `lin-face'."
  :group 'lin-faces)

(defface lin-magenta-override-fg
  '((default :inherit lin-magenta)
    (((background light))
     :foreground "black")
    (((background dark))
     :foreground "white"))
  "Like `lin-magenta' but also sets a foreground."
  :group 'lin-faces)

(defface lin-cyan
  '((default :foreground unspecified :underline nil :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#baeaf8")
    (((class color) (min-colors 88) (background dark))
     :background "#002f3f")
    (t :background "cyan"))
  "Alternative cyan face for `lin-face'."
  :group 'lin-faces)

(defface lin-cyan-override-fg
  '((default :inherit lin-cyan)
    (((background light))
     :foreground "black")
    (((background dark))
     :foreground "white"))
  "Like `lin-cyan' but also sets a foreground."
  :group 'lin-faces)

;; TODO 2022-03-18: Can we find all system styles?  Then we can rename
;; this to `lin-system'.
(defface lin-mac
  '((((type ns))
     ;; <https://developer.apple.com/design/human-interface-guidelines/macos/visual-design/color/>.
     :background "selectedContentBackgroundColor" :extend t)
    (t :inherit lin-blue))
  "Alternative macOS-style face for `lin-face'."
  :group 'lin-faces)

(defface lin-mac-override-fg
  '((t :inherit lin-mac :foreground "alternateSelectedControlTextColor"))
  "Like `lin-mac' but also sets a foreground."
  :group 'lin-faces)

;;;; Lin setup

(defvar-local lin--cookie nil
  "Cookie returned by `face-remap-add-relative'.")

(defun lin--source-face ()
  "Determine the source face: what to remap."
  (cond
   ((derived-mode-p 'mu4e-headers-mode)
    'mu4e-header-highlight-face)
   ((derived-mode-p 'magit-mode)
    'magit-section-highlight)
   ;; Do not target `hl-line' directly, as it can be changed by
   ;; `hl-line-face'.
   ((bound-and-true-p hl-line-face)
    hl-line-face)
   (t
    'hl-line)))

(define-minor-mode lin-mode
  "Enable `hl-line-mode' and remap its face to `lin-face'."
  :local t
  :init-value nil
  (if lin-mode
      (progn
        (setq lin--cookie
              (face-remap-add-relative (lin--source-face) lin-face))
        (hl-line-mode 1))
    (face-remap-remove-relative lin--cookie)
    (hl-line-mode -1)))

(defun lin-setup (&optional reverse)
  "Set up Lin for select mode hooks.

This adds `lin-mode' and `hl-line-mode' to every hook in
`lin-mode-hooks'.

With optional non-nil REVERSE argument, remove those hooks."
  (if reverse
      (dolist (hook lin-mode-hooks)
        (remove-hook hook #'lin-mode))
    (dolist (hook lin-mode-hooks)
      (add-hook hook #'lin-mode))))

(defun lin--mode-restart (buffer)
  "Restart `lin-mode' if already enabled in BUFFER."
  (with-current-buffer buffer
    (when lin-mode
      (lin-mode 1))))

(defun lin-restart-mode-in-buffers ()
  "Restart `lin-mode' if already enabled in any buffer.
This checks the `buffer-list'."
  (mapc #'lin--mode-restart (buffer-list)))

(provide 'lin)

;;; lin.el ends here
