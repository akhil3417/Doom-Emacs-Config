;;; prot-fonts.el --- Font configurations for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
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
;; This set of configurations pertains to my font settings, for use in
;; my Emacs setup: https://protesilaos.com/emacs/dotemacs.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

;;; Customisation options
(defgroup prot-fonts ()
  "Font-related configurations for my dotemacs."
  :group 'font)

;; NOTE: "Hack" and "Iosevka Comfy" are personal builds of Hack and
;; Iosevka respectively:
;;
;; 1. https://gitlab.com/protesilaos/hack-font-mod
;; 2. https://gitlab.com/protesilaos/iosevka-comfy
(defcustom prot-fonts-typeface-sets-alist
  '((small . ( :fixed-pitch-family "Hack"
               :fixed-pitch-regular-weight regular
               :fixed-pitch-heavy-weight bold
               :fixed-pitch-height 75
               :fixed-pitch-line-spacing 1
               :variable-pitch-family "FiraGO"
               :variable-pitch-height 1.05
               :variable-pitch-regular-weight normal))

    (regular . ( :fixed-pitch-family "Hack"
                 :fixed-pitch-regular-weight regular
                 :fixed-pitch-heavy-weight bold
                 :fixed-pitch-height 90
                 :fixed-pitch-line-spacing nil
                 :variable-pitch-family "FiraGO"
                 :variable-pitch-height 1.05
                 :variable-pitch-regular-weight normal))

    (large . ( :fixed-pitch-family "Source Code Pro"
               :fixed-pitch-regular-weight normal
               :fixed-pitch-heavy-weight bold
               :fixed-pitch-height 130
               :fixed-pitch-line-spacing nil
               :variable-pitch-family "Noto Serif"
               :variable-pitch-height 1.0
               :variable-pitch-regular-weight normal))

    (large-alt . ( :fixed-pitch-family "Iosevka Comfy"
                   :fixed-pitch-regular-weight book
                   :fixed-pitch-heavy-weight extrabold
                   :fixed-pitch-height 135
                   :fixed-pitch-line-spacing nil
                   :variable-pitch-family "Noto Sans"
                   :variable-pitch-height 1.0
                   :variable-pitch-regular-weight normal)))
  "Alist of desired typeface properties.

The car of each cons cell is an arbitrary key that broadly
describes the display type.  We use 'laptop', 'desktop' though
any symbol will do, e.g. 'video'.

The cdr is a plist that specifies the typographic properties of
fixed-pitch and variable-pitch fonts.  A few notes about those
properties:

- We specify typographic properties both for the `fixed-pitch'
  and `variable-pitch' faces.  This allows us to be explicit
  about all font families that may be used by the active
  theme (Modus themes) under various circumstances (e.g. enabling
  `variable-pitch' for the UI, or using `variable-pitch-mode').

- A semibold weight can only be used by font families that have
  one.  Otherwise it is better to specify bold, in order to avoid
  any potential unpredictable behaviour.

- Never set the :variable-pitch-height to an absolute number
  because that will break the layout of `text-scale-adjust'.  Use
  a floating point instead, so that when the text scale is
  adjusted those expand or contract accordingly.

- An absolute height is only need for the `default' face, which
  we here designated as a fixed-pitch typeface (so the faces
  `fixed-pitch' and `default' share the same font family, though
  their role remains distinct).

- The line height applies to the entirety of the Emacs session.
  We declare it as :fixed-pitch-line-spacing because the face
  `default' starts with a fixed-pitch font family.

- No tests are performed to determined the presence of the font
  families specified herein.  It is assumed that those always
  exist.

It is recommended that the order of the cons cells follows from
the smallest to largest font heights, to simplify the process of
identifying the set that belongs to the small and larger display
respectively (see code of `prot-fonts--laptop-desktop-keys')."
  :group 'prot-fonts
  :type 'alist)

(defun prot-fonts--laptop-desktop-keys ()
  "List laptop and desktop fontsets.
The elements of the list are the cars of the first two cons cells
of `prot-fonts-laptop-desktop-keys-list'"
  (let ((sets (mapcar #'car prot-fonts-typeface-sets-alist)))
    (list (nth 0 sets) (nth 1 sets))))

(defcustom prot-fonts-laptop-desktop-keys-list
  (prot-fonts--laptop-desktop-keys)
  "Symbols for `prot-fonts-fonts-per-monitor'.
This is a list whose first item denotes the smallest desirable
entry in `prot-fonts-typeface-sets-alist' for use on a laptop or
just smaller monitor, while the second points to a larger
display's key in that same alist.

The helper function `prot-fonts--laptop-desktop-keys' picks the
first two entries in `prot-fonts-typeface-sets-alist'."
  :group 'prot-fonts
  :type 'list)

(defcustom prot-fonts-max-small-resolution-width 1366
  "Maximum width for use in `prot-fonts-fonts-per-monitor'.
If the screen width is higher than this value (measuring pixels),
then the larger fonts will be used, as specified by the nth 1 of
`prot-fonts-laptop-desktop-keys-list'.  Otherwise the smaller
fonts, else nth 0, are applied."
  :group 'prot-fonts
  :type 'integer)

;;;; Helper functions

(defun prot-fonts--set-face-attribute (face family &optional weight height)
  "Set FACE font to FAMILY, with optional HEIGHT and WEIGHT."
  (let* ((u (if (eq face 'default) 100 1.0))
         (h (or height u))
         (w (or weight 'normal)))
    ;; ;; Read this: <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=45920>
    ;; ;; Hence why the following fails.  Keeping it for posterity...
    ;; (set-face-attribute face nil :family family :weight w :height h)
    (if (eq (face-attribute face :weight) w)
          (internal-set-lisp-face-attribute face :family family 0)
      (internal-set-lisp-face-attribute face :weight w 0)
      (internal-set-lisp-face-attribute face :family family 0)
      (internal-set-lisp-face-attribute face :weight w 0))
    (internal-set-lisp-face-attribute face :height h 0)))

;;;; Set preset families

(defvar prot-fonts--font-display-hist '()
  "History of inputs for display-related font associations.")

(defun prot-fonts--set-fonts-prompt ()
  "Prompt for font set (used by `prot-fonts-set-fonts')."
  (let ((def (nth 1 prot-fonts--font-display-hist)))
    (completing-read
     (format "Select font set for DISPLAY [%s]: " def)
     (mapcar #'car prot-fonts-typeface-sets-alist)
     nil t nil 'prot-fonts--font-display-hist def)))

(defvar prot-fonts-set-typeface-hook nil
  "Hook that runs after setting fonts.")

(defvar prot-fonts--current-spec nil
  "Current font set in `prot-fonts-typeface-sets-alist'.")

;;;###autoload
(defun prot-fonts-set-fonts (display)
  "Set fonts based on font set associated with DISPLAY.

DISPLAY is a symbol that represents the car of a cons cell in
`prot-fonts-typeface-sets-alist'."
  (interactive (list (prot-fonts--set-fonts-prompt)))
  (when window-system
      (let* ((fonts (if (stringp display) (intern display) display))
             (properties (alist-get fonts prot-fonts-typeface-sets-alist))
             (fixed-pitch-family (plist-get properties :fixed-pitch-family))
             (fixed-pitch-height (plist-get properties :fixed-pitch-height))
             (fixed-pitch-regular-weight (plist-get properties :fixed-pitch-regular-weight))
             (fixed-pitch-heavy-weight (plist-get properties :fixed-pitch-heavy-weight))
             (fixed-pitch-line-spacing (plist-get properties :fixed-pitch-line-spacing))
             (variable-pitch-family (plist-get properties :variable-pitch-family))
             (variable-pitch-height (plist-get properties :variable-pitch-height))
             (variable-pitch-regular-weight (plist-get properties :variable-pitch-regular-weight)))
        (prot-fonts--set-face-attribute
         'default fixed-pitch-family fixed-pitch-regular-weight fixed-pitch-height)
        (prot-fonts--set-face-attribute
         'fixed-pitch fixed-pitch-family fixed-pitch-regular-weight)
        (prot-fonts--set-face-attribute
         'variable-pitch variable-pitch-family variable-pitch-regular-weight variable-pitch-height)
        (set-face-attribute 'bold nil :weight fixed-pitch-heavy-weight)
        (setq-default line-spacing fixed-pitch-line-spacing)
        (add-to-history 'prot-fonts--font-display-hist (format "%s" display))
        (setq prot-fonts--current-spec (format "%s" display))
        (run-hooks 'prot-fonts-set-typeface-hook))))

;;;; Set default only

(defvar prot-fonts--system-typeface-hist ()
  "Minibuffer history for `prot-fonts--system-typefaces-prompt'.")

(defun prot-fonts--monospace-family-list (&optional frame)
  "Return a list of available monospace font family on FRAME."
  (seq-uniq
   (seq-map
    (lambda (fam)
      (symbol-name (aref fam 0)))
    (seq-filter
     (lambda (fam)
       (aref fam 5))
     (x-family-fonts nil frame)))))

(defun prot-fonts--system-typefaces-prompt (family-list-func)
  "Select installed fonts generated by FAMILY-LIST-FUNC."
  (let ((def (or (nth 1 prot-fonts--system-typeface-hist) "Monospace")))
    (completing-read
     (format "Select typeface [%s]: " def)
     (delete-dups (funcall family-list-func))
     nil t nil 'prot-fonts--system-typeface-hist def)))

(defvar prot-fonts--font-weights
  '( thin ultralight extralight light semilight regular medium
     semibold bold heavy extrabold ultrabold)
  "List of font weights.")

(defvar prot-fonts--font-weight-hist ()
  "Minibuffer history for `prot-fonts--font-weight-prompt'.")

(defun prot-fonts--font-weight-prompt ()
  "Select weight from `prot-fonts--font-weights'."
  (let ((def (or (car prot-fonts--font-weight-hist) 'regular)))
    (intern
     (completing-read
      (format "Select font weight [%s]: " def)
      prot-fonts--font-weights
      nil t nil 'prot-fonts--font-weight-hist def))))

(defvar prot-fonts--font-height-hist ()
  "Minibuffer history for `prot-fonts--font-weight-prompt'.")

(defvar prot-fonts--font-heights
  '(80 85 90 95 100 105 110 120 130 140 150 160 170 180)
  "Non-exhuastive list of font heights.")

(defun prot-fonts--font-height-prompt ()
  "Select height from `prot-fonts--font-heights'."
  (let ((def (or (car prot-fonts--font-height-hist) 100)))
    ;; FIXME 2022-01-24: the `string-to-number' and `number-to-string'
    ;; roundabout feels wrong.  Can't we complete against numbers
    ;; directly?
    (string-to-number
     ;; `read-number' does not support completion...
     (completing-read
      (format "Select font height [%s]: " def)
      (mapcar #'number-to-string prot-fonts--font-heights)
      nil nil nil 'prot-fonts--font-height-hist def))))

;;;###autoload
(defun prot-fonts-set-default-font (font weight height)
  "Change `default' face family to FONT with WEIGHT and HEIGHT.
When called interactively, if prefix argument is non-nil, then list only
monospaced fonts.
To pick a preset, use `prot-fonts-set-fonts' instead."
  (interactive
   (list (if current-prefix-arg
	     (prot-fonts--system-typefaces-prompt #'prot-fonts--monospace-family-list)
	   (prot-fonts--system-typefaces-prompt #'font-family-list))
         (prot-fonts--font-weight-prompt)
         (prot-fonts--font-height-prompt)))
  (prot-fonts--set-face-attribute 'default font weight height)
  (run-hooks 'prot-fonts-set-typeface-hook))

;;;; Automatic font adjustments

(defun prot-fonts-restore-last ()
  "Restore last fontset.
This is necessary when/if changes to face specs alter some
typographic properties.  For example, when switching themes the
:weight of the `bold' face will be set to whatever the theme
specifies, typically 'bold', which is not what we always have on
our end."
  (let ((ultimate (nth 0 prot-fonts--font-display-hist))
        (penultimate (nth 1 prot-fonts--font-display-hist)))
    (if (string= ultimate prot-fonts--current-spec)
        (prot-fonts-set-fonts ultimate)
      (prot-fonts-set-fonts penultimate))))

(defun prot-fonts--display-type-for-monitor (&optional smaller larger)
  "Determine typeface specs based on monitor width.
Optional SMALLER and LARGER are two keys that point to entries in
`prot-fonts-typeface-sets-alist'.  The default uses the relevant
keys from `prot-fonts-laptop-desktop-keys-list'."
  (let* ((keys prot-fonts-laptop-desktop-keys-list)
         (face-specs prot-fonts-typeface-sets-alist)
         (small (or smaller (nth 0 keys)))
         (large (or larger (nth 1 keys)))
         (max-width prot-fonts-max-small-resolution-width)
         (spec (if (<= (display-pixel-width) max-width)
                   small
                 large)))
    (unless (assoc spec face-specs)
      (error (concat "Key <<%s>> in `prot-fonts-laptop-desktop-keys-list' "
                     "does not reference anything in "
                     "`prot-fonts-typeface-sets-alist'")
             spec))
    spec))

;;;###autoload
(defun prot-fonts-fonts-per-monitor ()
  "Use font settings based on screen size.
The breakpoint is `prot-fonts-max-small-resolution-width', while
`prot-fonts-laptop-desktop-keys-list' contains the keys of the
two font sets to be used: its first element should point at
smaller fonts than the second element."
  (when window-system
    (let ((display (prot-fonts--display-type-for-monitor)))
      (prot-fonts-set-fonts display))))

(provide 'prot-fonts)
;;; prot-fonts.el ends here
