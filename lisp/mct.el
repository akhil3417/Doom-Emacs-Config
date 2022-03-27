;;; mct.el --- Minibuffer and Completions in Tandem -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://gitlab.com/protesilaos/mct
;; Version: 0.5.0
;; Package-Requires: ((emacs "27.1"))

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
;; MCT enhances the default Emacs completion.  It makes the minibuffer
;; and Completions' buffer work together as part of a singular
;; completion UI.
;;
;; Read the documentation for basic usage and configuration.

;;; Code:

;;;; General utilities

(defgroup mct ()
  "Minibuffer and Completions in Tandem.
A layer of interactivity that integrates the standard minibuffer
and the Completions."
  :group 'minibuffer)

(make-obsolete 'mct-completion-windows-regexp 'mct--completions-window-name "0.5.0")

(defcustom mct-completion-window-size (cons #'mct--frame-height-fraction 1)
  "Set the maximum and minimum height of the Completions' buffer.

The value is a cons cell in the form of (max-height . min-height)
where each value is either a natural number or a function which
returns such a number.

The default maximum height of the window is calculated by the
function `mct--frame-height-fraction', which finds the closest
round number to 1/3 of the frame's height.  While the default
minimum height is 1.  This means that during live completions the
Completions' window will shrink or grow to show candidates within
the specified boundaries.  To disable this bouncing effect, set
both max-height and min-height to the same number.

If nil, do not try to fit the Completions' buffer to its window.

Also see `mct-live-completion'."
  :type '(choice (const :tag "Disable size constraints" nil)
                 (cons
                  (choice (function :tag "Function to determine maximum height")
                          (natnum :tag "Maximum height in number of lines"))
                  (choice (function :tag "Function to determine minimum height")
                          (natnum :tag "Minimum height in number of lines"))))
  :group 'mct)

(defcustom mct-remove-shadowed-file-names nil
  "Delete shadowed parts of file names from the minibuffer.

For example, if the user types ~/ after a long path name,
everything preceding the ~/ is removed so the interactive
selection process starts again from the user's $HOME.

Only works when variable `file-name-shadow-mode' is non-nil."
  :type 'boolean
  :group 'mct)

(defcustom mct-hide-completion-mode-line nil
  "When non-nil, hide the Completions' buffer mode line."
  :type 'boolean
  :group 'mct)

(defcustom mct-apply-completion-stripes nil
  "When non-nil, use alternating backgrounds in the Completions."
  :type 'boolean
  :group 'mct)

(defcustom mct-live-completion t
  "Control auto-display and live-update of Completions' buffer.

When nil, the user has to manually request completions, using the
regular activating commands.  The Completions' buffer is never
updated live to match user input.  Updating has to be handled
manually.  This is like the out-of-the-box minibuffer completion
experience.

When set to the value `visible', the Completions' buffer is live
updated only if it is visible.  The actual display of the
completions is still handled manually.  For this reason, the
`visible' style does not read the `mct-minimum-input', meaning
that it will always try to live update the visible completions,
regardless of input length.

When non-nil (the default), the Completions' buffer is
automatically displayed once the `mct-minimum-input' is met and
is hidden if the input drops below that threshold.  While
visible, the buffer is updated live to match the user's input.

Note that every command or completion category in the
`mct-completion-passlist' ignores this option altogether.  This
means that every such symbol will always show the Completions'
buffer automatically and will always update its contents live.
Same principle for `mct-completion-blocklist', which will always
disable both the automatic display and live updating of the
Completions' buffer.

Also see `mct-completion-window-size'."
  :type '(choice
          (const :tag "Disable live-updating" nil)
          (const :tag "Enable live-updating" t)
          (const :tag "Live update only visible Completions" 'visible))
  :group 'mct)

(defcustom mct-minimum-input 3
  "Live update completions when input is >= N.

Setting this to a value greater than 1 can help reduce the total
number of candidates that are being computed."
  :type 'natnum
  :group 'mct)

(defcustom mct-live-update-delay 0.3
  "Delay in seconds before updating the Completions' buffer.
Set this to 0 to disable the delay.

This applies in all cases covered by `mct-live-completion'."
  :type 'number
  :group 'mct)

(defcustom mct-completion-blocklist nil
  "List of symbols where live completions are outright disabled.

The value of this user option is a list of symbols.  Those can
refer to commands like `find-file' or completion categories such
as `file', `buffer', or what other packages define like Consult's
`consult-location' category.

This means that they ignore `mct-live-completion'.  They do not
automatically display the Completions' buffer, nor do they update
it to match user input.

The Completions' buffer can still be accessed with commands that
place it in a window (such as `mct-list-completions-toggle',
`mct-switch-to-completions-top').

Perhaps a less drastic measure is to set `mct-minimum-input' to
an appropriate value.  Or better use `mct-completion-passlist'.

Read the manual for known completion categories."
  :type '(repeat symbol)
  :group 'mct)

(defcustom mct-completion-passlist nil
  "List of symbols where live completions are always enabled.

The value of this user option is a list of symbols.  Those can
refer to commands like `find-file' or completion categories such
as `file', `buffer', or what other packages define like Consult's
`consult-location' category.

This means that they ignore the value of `mct-live-completion'
and the `mct-minimum-input'.  They also bypass any possible delay
introduced by `mct-live-update-delay'.

Read the manual for known completion categories."
  :type '(repeat symbol)
  :group 'mct)

(defcustom mct-display-buffer-action
  '((display-buffer-reuse-window display-buffer-at-bottom))
  "The action used to display the Completions' buffer.

The value has the form (FUNCTION . ALIST), where FUNCTIONS is
either an \"action function\" or a possibly empty list of action
functions.  ALIST is a possibly empty \"action alist\".

Sample configuration:

    (setq mct-display-buffer-action
          (quote ((display-buffer-reuse-window
                   display-buffer-in-side-window)
                  (side . left)
                  (slot . 99)
                  (window-width . 0.3))))

See Info node `(elisp) Displaying Buffers' for more details
and/or the documentation string of `display-buffer'."
  :type '(cons (choice (function :tag "Display Function")
                       (repeat :tag "Display Functions" function))
               alist)
  :group 'mct)

(defcustom mct-completions-format 'one-column
  "Set the presentation of candidates in the Completions' buffer.
See `completions-format' for possible values."
  :type '(choice (const horizontal) (const vertical) (const one-column))
  :group 'mct)

(make-obsolete 'mct-region-completions-format 'mct-completions-format "0.5.0")

(defcustom mct-persist-dynamic-completion t
  "When non-nil, keep dynamic completion live.

Without any intervention from MCT, the default Emacs behavior for
commands such as `find-file' or for a `file' completion category
is to hide the `*Completions*' buffer after updating the list of
candidates in a non-exiting fashion (e.g. select a directory and
expect to continue typing the path).  This, however, runs
contrary to the interaction model of MCT when it performs live
completions, because the user expects the Completions' buffer to
remain visible while typing out the path to the file.

When this user option is non-nil (the default) it makes all
non-exiting commands keep the `*Completions*' visible when
updating the list of candidates.

This applies to prompts in the `file' completion category
whenever the user selects a candidate with
`mct-choose-completion-no-exit', `mct-edit-completion',
`minibuffer-complete', `minibuffer-force-complete' (i.e. any
command that does not exit the minibuffer).

The two exceptions are (i) when the current completion session
runs a command or category that is blocked by the
`mct-completion-blocklist' or (ii) the user option
`mct-live-completion' is nil.

The underlying rationale:

Most completion commands present a flat list of candidates to
choose from.  Picking a candidate concludes the session.  Some
prompts, however, can recalculate the list of completions based
on the selected candidate.  A case in point is `find-file' (or
any command with the `file' completion category) which
dynamically adjusts the completions to show only the elements
which extend the given file system path.  We call such cases
\"dynamic completion\".  Due to their particular nature, these
need to be handled explicitly.  The present user option is
provided primarily to raise awareness about this state of
affairs."
  :type 'boolean
  :group 'mct)

;;;; Completion metadata

(defun mct--this-command ()
  "Return this command."
  (or (bound-and-true-p current-minibuffer-command) this-command))

(defun mct--completion-category ()
  "Return completion category."
  (when-let ((window (active-minibuffer-window)))
    (with-current-buffer (window-buffer window)
      (completion-metadata-get
       (completion-metadata (buffer-substring-no-properties
                             (minibuffer-prompt-end)
                             (max (minibuffer-prompt-end) (point)))
                            minibuffer-completion-table
                            minibuffer-completion-predicate)
       'category))))

(defun mct--symbol-in-list (list)
  "Test if command or category is in LIST."
  (or (memq (mct--this-command) list)
      (memq (mct--completion-category) list)))

(defun mct--passlist-p ()
  "Return non-nil if symbol is in the `mct-completion-passlist'."
  (or (memq (mct--this-command) mct-completion-passlist)
      (memq (mct--completion-category) mct-completion-passlist)))

(defun mct--blocklist-p ()
  "Return non-nil if symbol is in the `mct-completion-blocklist'."
  (or (memq (mct--this-command) mct-completion-blocklist)
      (memq (mct--completion-category) mct-completion-blocklist)))

;; Normally we would also include `imenu', but it has its own defcustom
;; for popping up the Completions eagerly...  Let's not interfere with
;; that.
;;
;; See bug#52389: <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=52389>.
(defvar mct--dynamic-completion-categories '(file)
  "Completion categories that perform dynamic completion.")

;;;; Basics of intersection between minibuffer and Completions' buffer

(define-obsolete-variable-alias
  'mct-hl-line 'mct-highlight-candidate "0.3.0")

(defface mct-highlight-candidate
  '((t :inherit highlight :extend t))
  "Face for current candidate in the completions' buffer."
  :group 'mct)

(defun mct--first-line-completion-p ()
  "Return non-nil if first line has completion candidates."
  (eq (line-number-at-pos (point-min))
      (line-number-at-pos (mct--first-completion-point))))

;; Thanks to Omar Antolín Camarena for recommending the use of
;; `cursor-sensor-functions' and the concomitant hook with
;; `cursor-sensor-mode' instead of the dirty hacks I had before to
;; prevent the cursor from moving to that position where no completion
;; candidates could be found at point (e.g. it would break `embark-act'
;; as it could not read the topmost candidate when point was at the
;; beginning of the line, unless the point was moved forward).
(defun mct--setup-clean-completions ()
  "Keep only completion candidates in the Completions."
  (with-current-buffer standard-output
    (unless (mct--first-line-completion-p)
      (goto-char (point-min))
      (let ((inhibit-read-only t))
        (delete-region (point-at-bol) (1+ (point-at-eol)))
        (insert (propertize " "
                            'cursor-sensor-functions
                            (list
                             (lambda (_win prev dir)
                               (when (eq dir 'entered)
                                 (goto-char prev))))))
        (put-text-property (point-min) (point) 'invisible t)))))

(defun mct--frame-height-fraction ()
  "Return round number of 1/3 of `frame-height'.
Can be used in `mct-completion-window-size'."
  (floor (frame-height) 3))

(defun mct--height (param)
  "Return height of PARAM in number of lines."
  (cond
   ((natnump param) param)
   ((functionp param) (funcall param))
   ;; There is no compelling reason to fall back to 5.  It just feels
   ;; like a reasonable small value...
   (t 5)))

(defun mct--fit-completions-window (&rest _args)
  "Fit Completions' buffer to its window."
  (when-let* ((window (mct--get-completion-window))
              (size mct-completion-window-size)
              (max (car size))
              (min (cdr size)))
    (fit-window-to-buffer window (mct--height max) (mct--height min))))

(defun mct--minimum-input ()
  "Test for minimum requisite input for live completions.
See `mct-minimum-input'."
  (>= (- (point-max) (minibuffer-prompt-end)) mct-minimum-input))

;;;;; Live-updating Completions' buffer

(defvar mct--completions-window-name "\\`\\*Completions.*\\*\\'"
  "Regexp to match window names with completion candidates.")

;; Adapted from Omar Antolín Camarena's live-completions library:
;; <https://github.com/oantolin/live-completions>.
(defun mct--live-completions-refresh-immediately ()
  "Update the *Completions* buffer immediately."
  (when (minibufferp) ; skip if we've exited already
    (while-no-input
      (if (or (mct--minimum-input)
              (eq mct-live-completion 'visible))
          (condition-case nil
              (save-match-data
                (save-excursion
                  (goto-char (point-max))
                  (mct--show-completions)))
            (quit (abort-recursive-edit)))
        (minibuffer-hide-completions)))))

(defvar mct--timer nil
  "Latest timer object for live completions.")

(defun mct--live-completions-refresh (&rest _)
  "Update the *Completions* buffer with a delay.
Meant to be added to `after-change-functions'."
  (when (and
         ;; Check that live completions are enabled by looking at
         ;; after-change-functions. This check is needed for Consult
         ;; integration, which refreshes the display asynchronously.
         (memq #'mct--live-completions-refresh after-change-functions)
         ;; Update only visible completion windows?
         (or (not (eq mct-live-completion 'visible))
             (window-live-p (mct--get-completion-window))))
    (when mct--timer
      (cancel-timer mct--timer)
      (setq mct--timer nil))
    (if (> mct-live-update-delay 0)
        (setq mct--timer (run-with-idle-timer
                          mct-live-update-delay
                          nil #'mct--live-completions-refresh-immediately))
      (mct--live-completions-refresh-immediately))))

(defun mct--setup-live-completions ()
  "Set up the completions' buffer."
  (cond
   ((null mct-live-completion))
   ;; ;; NOTE 2022-02-25: The passlist setup we had here was being
   ;; ;; called too early in `mct--completing-read-advice'.  It would
   ;; ;; fail to filter out the current candidate from the list
   ;; ;; (e.g. current buffer from `switch-to-buffer').  This would, in
   ;; ;; turn, hinder the scrolling behaviour of `minibuffer-complete'.
   ;; ;; See: <https://gitlab.com/protesilaos/mct/-/issues/24>.  The
   ;; ;; replacement function is `mct--setup-passlist' which is hooked
   ;; ;; directly to `minibuffer-setup-hook'.
   ;;
   ;; ((mct--passlist-p)
   ;;  (setq-local mct-minimum-input 0)
   ;;  (setq-local mct-live-update-delay 0)
   ;;  (mct--show-completions)
   ;;  (add-hook 'after-change-functions #'mct--live-completions-refresh nil t))
   ((not (mct--blocklist-p))
    (add-hook 'after-change-functions #'mct--live-completions-refresh nil t))))

(defun mct--setup-passlist ()
  "Set up the minibuffer for `mct-completion-passlist'."
  (when (and (mct--passlist-p) (mct--minibuffer-p))
    (setq-local mct-minimum-input 0)
    (setq-local mct-live-update-delay 0)
    (mct--show-completions)))

(defvar-local mct--active nil
  "Minibuffer local variable, t if Mct is active.")

(defun mct--minibuffer-p ()
  "Return t if Mct is active."
  (when-let* ((win (active-minibuffer-window))
              (buf (window-buffer win)))
      (buffer-local-value 'mct--active buf)))

(defun mct--region-p ()
  "Return non-nil if Mct is completing in region."
  (when-let ((buf (mct--region-current-buffer)))
    (buffer-local-value 'mct-region-mode buf)))

(defun mct--minibuffer-completion-help-advice (&rest app)
  "Prepare advice around `display-completion-list'.
Apply APP by first let binding the `completions-format' to
`mct-completions-format'."
  (if (mct--minibuffer-p)
      (let ((completions-format mct-completions-format))
        (apply app)
        (mct--fit-completions-window))
    (apply app)))

(defun mct--region-completion-help-advice (&rest app)
  "Prepare advice around `display-completion-list'.
Apply APP by first let binding the `completions-format' to
`mct-completions-format'."
  (if (mct--region-p)
      (let ((completions-format mct-completions-format))
        (apply app)
        (mct--fit-completions-window))
    (apply app)))

(defun mct--completing-read-advice (&rest app)
  "Prepare advice around `completing-read-default'.
Apply APP by first setting up the minibuffer to work with Mct."
  (minibuffer-with-setup-hook
      (lambda ()
        (setq-local resize-mini-windows t
                    completion-auto-help t)
        (setq mct--active t)
        (mct--setup-live-completions)
        (mct--setup-minibuffer-keymap)
        (mct--setup-shadow-files))
    (apply app)))

;;;;; Alternating backgrounds (else "stripes")

;; Based on `stripes.el' (maintained by Štěpán Němec) and the
;; `embark-collect-zebra-minor-mode' from Omar Antolín Camarena's
;; Embark:
;;
;; 1. <https://gitlab.com/stepnem/stripes-el>
;; 2. <https://github.com/oantolin/embark>
(defface mct-stripe
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#f0f0f0")
    (((class color) (min-colors 88) (background dark))
     :background "#191a1b"))
  "Face for alternating backgrounds in the Completions' buffer."
  :group 'mct)

(defun mct--remove-stripes ()
  "Remove `mct-stripe' overlays."
  (remove-overlays nil nil 'face 'mct-stripe))

(defun mct--add-stripes ()
  "Overlay alternate rows with the `mct-stripe' face."
  (when (derived-mode-p 'completion-list-mode)
    (mct--remove-stripes)
    (save-excursion
      (goto-char (point-min))
      (when (overlays-at (point)) (forward-line))
      (while (not (eobp))
        (condition-case nil
            (forward-line 1)
          (user-error (goto-char (point-max))))
        (unless (eobp)
          (let ((pt (point)))
            (condition-case nil
                (forward-line 1)
              (user-error (goto-char (point-max))))
            ;; We set the overlay this way and give it a low priority so
            ;; that `mct--highlight-overlay' and/or the active region
            ;; can override it.
            (let ((stripe (make-overlay pt (point))))
              (overlay-put stripe 'priority -100)
              (overlay-put stripe 'face 'mct-stripe))))))))

;;;; Commands and helper functions

;; TODO 2021-11-17: We must `autoload' instead of `declare-function' for
;; things to work on Emacs 27.  Perhaps we should keep the latter but
;; add (eval-when-compile (require 'text-property-search))?  That should
;; work for packages, but not if we just `eval-buffer', right?

(autoload 'text-property-search-backward "text-property-search")
(autoload 'text-property-search-forward "text-property-search")
(autoload 'prop-match-beginning "text-property-search")
(autoload 'prop-match-end "text-property-search")

;; (declare-function text-property-search-backward "text-property-search" (property &optional value predicate not-current))
;; (declare-function text-property-search-forward "text-property-search" (property &optional value predicate not-current))
;; (declare-function prop-match-beginning "text-property-search" (cl-x))
;; (declare-function prop-match-end "text-property-search" (cl-x))

;; We need this to make things work on Emacs 27.
(defun mct--one-column-p ()
  "Test if we have a one-column view available."
  (and (eq mct-completions-format 'one-column)
       (>= emacs-major-version 28)))

;;;;; Focus minibuffer and/or show completions

;;;###autoload
(defun mct-focus-minibuffer ()
  "Focus the active minibuffer."
  (interactive nil mct-minibuffer-mode)
  (when-let ((mini (active-minibuffer-window)))
    (select-window mini)))

(defun mct--get-completion-window ()
  "Find a live window showing completion candidates."
  (get-window-with-predicate
   (lambda (window)
     (string-match-p
      mct--completions-window-name
      (buffer-name (window-buffer window))))))

(defun mct--show-completions ()
  "Show the completions' buffer."
  (let ((display-buffer-alist
         (cons (cons mct--completions-window-name mct-display-buffer-action)
               display-buffer-alist))
        ;; don't ring the bell in `minibuffer-completion-help'
        ;; when <= 1 completion exists.
        (ring-bell-function #'ignore)
        (message-log-max nil)
        (inhibit-message t))
    (save-excursion
      (pcase (and completion-in-region-mode completion-in-region--data)
        (`(,start ,end ,collection . ,plist)
         (let ((minibuffer-completion-table collection)
               (minibuffer-completion-predicate (plist-get plist :predicate))
               (completion-extra-properties plist))
           (minibuffer-completion-help start end)))
        (_ (minibuffer-completion-help))))))

;;;###autoload
(defun mct-focus-mini-or-completions ()
  "Focus the active minibuffer or the completions' window.

If both the minibuffer and the Completions are present, this
command will first move per invocation to the former, then the
latter, and then continue to switch between the two.

The continuous switch is essentially the same as running
`mct-focus-minibuffer' and `switch-to-completions' in
succession.

What constitutes a completions' window is ultimately determined
by `mct--completions-window-name'."
  (interactive nil mct-minibuffer-mode)
  (let* ((mini (active-minibuffer-window))
         (completions (mct--get-completion-window)))
    (cond
     ((and mini (not (minibufferp)))
      (select-window mini nil))
     ((and completions (not (eq (selected-window) completions)))
      (select-window completions nil)))))

;;;###autoload
(defun mct-list-completions-toggle ()
  "Toggle the presentation of the completions' buffer."
  (interactive nil mct-minibuffer-mode)
  (if (mct--get-completion-window)
      (minibuffer-hide-completions)
    (mct--show-completions)))

;;;;; Cyclic motions between minibuffer and completions' buffer

(defun mct--completion-at-point-p ()
  "Return non-nil if there is a completion at point."
  (let ((point (point)))
    ;; The `or' is for Emacs 27 where there are no completion--string
    ;; properties.
    (or (get-text-property point 'completion--string)
        (get-text-property point 'mouse-face))))

(defun mct--arg-completion-point-p (arg)
  "Return non-nil if ARGth next completion exists."
  (save-excursion
    (mct--next-completion arg)
    (mct--completion-at-point-p)))

(defun mct--first-completion-point ()
  "Return the `point' of the first completion."
  (save-excursion
    (goto-char (point-min))
    (next-completion 1)
    (point)))

(defun mct--last-completion-point ()
  "Return the `point' of the last completion."
  (save-excursion
    (goto-char (point-max))
    (next-completion -1)
    (point)))

(defun mct--completions-line-boundary (boundary)
  "Determine if current line has reached BOUNDARY.
BOUNDARY is a line position at the top or bottom of the
Completions' buffer.  See `mct--first-completion-point' or
`mct--last-completion-point'.

This check only applies when `completions-format' is not assigned
a `one-column' value."
  (and (= (line-number-at-pos) (line-number-at-pos boundary))
       (not (mct--one-column-p))))

(defun mct--completions-no-completion-line-p (arg)
  "Check if ARGth line has a completion candidate."
  (save-excursion
    (vertical-motion arg)
    (null (mct--completion-at-point-p))))

(defun mct--switch-to-completions ()
  "Subroutine for switching to the completions' buffer."
  (unless (mct--get-completion-window)
    (mct--show-completions))
  (switch-to-completions))

(defun mct--restore-old-point-in-grid (line)
  "Restore old point in window if LINE is on its line."
  (unless (mct--one-column-p)
    (let (old-line old-point)
      (when-let ((window (mct--get-completion-window)))
        (setq old-point (window-old-point window)
              old-line (line-number-at-pos old-point))
        (when (= (line-number-at-pos line) old-line)
          (if (eq old-point (point-min))
              (goto-char (mct--first-completion-point))
            (goto-char old-point)))))))

(defun mct-switch-to-completions-top ()
  "Switch to the top of the completions' buffer."
  (interactive nil mct-minibuffer-mode mct-region-mode)
  (mct--switch-to-completions)
  (goto-char (mct--first-completion-point))
  (mct--restore-old-point-in-grid (point)))

(defun mct-switch-to-completions-bottom ()
  "Switch to the bottom of the completions' buffer."
  (interactive nil mct-minibuffer-mode mct-region-mode)
  (mct--switch-to-completions)
  (goto-char (point-max))
  (next-completion -1)
  (goto-char (point-at-bol))
  (unless (mct--completion-at-point-p)
    (next-completion 1))
  (mct--restore-old-point-in-grid (point))
  (recenter
   (- -1
      (min (max 0 scroll-margin)
           (truncate (/ (window-body-height) 4.0))))
   t))

(defun mct--empty-line-p (arg)
  "Return non-nil if ARGth line is empty."
  (unless (mct--arg-completion-point-p arg)
    (save-excursion
      (goto-char (point-at-bol))
      (and (not (bobp))
	       (or (beginning-of-line (1+ arg)) t)
	       (save-match-data
	         (looking-at "[\s\t]*$"))))))

(defun mct--bottom-of-completions-p (arg)
  "Test if point is at the notional bottom of the Completions.
ARG is a numeric argument for `next-completion', as described in
`mct-next-completion-or-mini'."
  (or (eobp)
      (mct--completions-line-boundary (mct--last-completion-point))
      (= (save-excursion (next-completion arg) (point)) (point-max))
      ;; The empty final line case which should avoid candidates with
      ;; spaces or line breaks...
      (mct--empty-line-p arg)))

(defun mct--next-completion (arg)
  "Routine to move to the next ARGth completion candidate."
  (if (not (mct--one-column-p))
      ;; Retaining the column number ensures that things work
      ;; intuitively in a grid view.
      (let ((col (current-column)))
        ;; The `when' is meant to skip past lines that do not
        ;; contain completion candidates, such as those with
        ;; `completions-group-format'.
        (when (mct--completions-no-completion-line-p (or arg 1))
          (if arg
              (setq arg 2)
            (setq arg (1+ arg))))
        (vertical-motion (or arg 1))
        (unless (eq col (save-excursion (goto-char (point-at-bol)) (current-column)))
          (line-move-to-column col))
        (when (or (> (current-column) col)
                  (not (mct--completion-at-point-p)))
          (next-completion -1)))
    (next-completion (or arg 1))))

(defun mct-next-completion-or-mini (&optional arg)
  "Move to the next completion or switch to the minibuffer.
This performs a regular motion for optional ARG candidates, but
when point can no longer move in that direction it switches to
the minibuffer."
  (interactive "p" mct-minibuffer-mode)
  (let ((count (or arg 1)))
    (if (mct--bottom-of-completions-p count)
        (mct-focus-minibuffer)
      (mct--next-completion count))))

(defun mct--motion-below-point-min-p (arg)
  "Return non-nil if backward ARG motion exceeds `point-min'."
  (let ((line (- (line-number-at-pos) arg)))
    (or (< line 1)
        (= (save-excursion (previous-completion arg) (point)) (point-min)))))

(defun mct--top-of-completions-p (arg)
  "Test if point is at the notional top of the Completions.
ARG is a numeric argument for `previous-completion', as described in
`mct-previous-completion-or-mini'."
  (or (bobp)
      (mct--completions-line-boundary (mct--first-completion-point))
      (mct--motion-below-point-min-p arg)
      ;; FIXME 2021-12-27: Why do we need this now?  Regression upstream?
      (eq (line-number-at-pos) 1)))

(defun mct--previous-completion (arg)
  "Routine to move to the previous ARGth completion candidate."
  (if (not (mct--one-column-p))
      ;; Retaining the column number ensures that things work
      ;; intuitively in a grid view.
      (let ((col (current-column)))
        ;; The `when' is meant to skip past lines that do not
        ;; contain completion candidates, such as those with
        ;; `completions-group-format'.
        (when (mct--completions-no-completion-line-p (or (- arg) -1))
          (if arg
              (setq arg 2)
            (setq arg (1+ arg))))
        (vertical-motion (or (- arg) -1))
        (unless (eq col (save-excursion (goto-char (point-at-bol)) (current-column)))
          (line-move-to-column col))
        (when (or (> (current-column) col)
                  (not (mct--completion-at-point-p)))
          (next-completion -1)))
    (previous-completion (or (abs arg) 1))))

(defun mct-previous-completion-or-mini (&optional arg)
  "Move to the previous completion or switch to the minibuffer.
This performs a regular motion for optional ARG candidates, but
when point can no longer move in that direction it switches to
the minibuffer."
  (interactive "p" mct-minibuffer-mode)
  (let ((count (if (natnump arg) arg 1)))
    (if (mct--top-of-completions-p count)
        (mct-focus-minibuffer)
      (mct--previous-completion count))))

(defun mct-next-completion-group (&optional arg)
  "Move to the next completion group.
If ARG is supplied, move that many completion groups at a time."
  (interactive "p" mct-minibuffer-mode mct-region-mode)
  (dotimes (_ (or arg 1))
    (when-let (group (save-excursion
                       (text-property-search-forward 'face
                                                     'completions-group-separator
                                                     t nil)))
      (let ((pos (prop-match-end group)))
        (unless (eq pos (point-max))
          (goto-char pos)
          (next-completion 1))))))

(defun mct-previous-completion-group (&optional arg)
  "Move to the previous completion group.
If ARG is supplied, move that many completion groups at a time."
  (interactive "p" mct-minibuffer-mode mct-region-mode)
  (dotimes (_ (or arg 1))
    ;; skip back, so if we're at the top of a group, we go to the previous one...
    (forward-line -1)
    (if-let (group (save-excursion
                     (text-property-search-backward 'face
                                                    'completions-group-separator
                                                    t nil)))
        (let ((pos (prop-match-beginning group)))
          (unless (eq pos (point-min))
            (goto-char pos)
            (next-completion 1)))
      ;; ...and if there was a match, go back down, so the point doesn't
      ;; end in the group separator
      (forward-line 1))))

;;;;; Candidate selection

;; The difference between this and choose-completion is that it will
;; exit even if a directory is selected in find-file, whereas
;; choose-completion expands the directory and continues the session.
(defun mct-choose-completion-exit ()
  "Run `choose-completion' in the Completions buffer and exit."
  (interactive nil mct-minibuffer-mode)
  (choose-completion)
  (when (active-minibuffer-window)
    (minibuffer-force-complete-and-exit)))

(defun mct-choose-completion-no-exit ()
  "Run `choose-completion' in the Completions without exiting."
  (interactive nil mct-minibuffer-mode)
  (let ((completion-no-auto-exit t))
    (choose-completion)))

(defvar crm-completion-table)
(defvar crm-separator)

(defun mct--regex-to-separator (regex)
  "Parse REGEX of `crm-separator' in `mct-choose-completion-dwim'."
  (save-match-data
    (cond
     ;; whitespace-delimited, like default & org-set-tag-command
     ((string-match (rx
                     bos "[" (1+ blank) "]*"
                     (group (1+ any))
                     "[" (1+ blank) "]*" eos)
                    regex)
      (match-string 1 regex))
     ;; literal character
     ((string= regex (regexp-quote regex))
      regex))))

(defun mct-choose-completion-dwim ()
  "Append to minibuffer when at `completing-read-multiple' prompt.
In any other prompt use `mct-choose-completion-no-exit'."
  (interactive nil mct-minibuffer-mode)
  (when-let* ((mini (active-minibuffer-window))
              (window (mct--get-completion-window))
              (buffer (window-buffer window)))
    (mct-choose-completion-no-exit)
    (with-current-buffer (window-buffer mini)
      (when crm-completion-table
        (let ((separator (or (mct--regex-to-separator crm-separator)
                             ",")))
          (insert separator))
        (let ((inhibit-message t))
          (switch-to-completions))))))

(defun mct-edit-completion ()
  "Edit the current completion candidate inside the minibuffer.

The current candidate is the one at point while inside the
Completions' buffer.

When point is in the minibuffer, the current candidate is
determined as follows:

+ The one at the last known position in the Completions'
  window (if the window is deleted and produced again, this value
  is reset).

+ The first candidate in the Completions' buffer.

A candidate is recognised for as long as point is not past its
last character."
  (interactive nil mct-minibuffer-mode)
  (when-let ((window (mct--get-completion-window))
             ((active-minibuffer-window)))
    (with-current-buffer (window-buffer window)
      (let* ((old-point (save-excursion
                          (select-window window)
                          (window-old-point)))
             (pos (if (= old-point (point-min))
                      (mct--first-completion-point)
                    old-point)))
        (goto-char pos)
        (mct-choose-completion-no-exit)))))

(defun mct-complete-and-exit ()
  "Complete current input and exit.

This has the same effect as with
\\<mct-minibuffer-local-completion-map>\\[mct-edit-completion],
followed by exiting the minibuffer with that candidate."
  (interactive nil mct-minibuffer-mode)
  (mct-edit-completion)
  (exit-minibuffer))

;;;;; Miscellaneous commands

;; This is needed to circumvent `mct--setup-clean-completions' with regard to
;; `cursor-sensor-functions'.
(defun mct-beginning-of-buffer ()
  "Go to the top of the Completions buffer."
  (interactive nil mct-minibuffer-mode mct-region-mode)
  (goto-char (mct--first-completion-point)))

(defun mct-keyboard-quit-dwim ()
  "Control the exit behaviour for completions' buffers.

If in a completions' buffer and unless the region is active, run
`abort-recursive-edit'.  Otherwise run `keyboard-quit'.

If the region is active, deactivate it.  A second invocation of
this command is then required to abort the session."
  (interactive nil mct-minibuffer-mode mct-region-mode)
  (when (derived-mode-p 'completion-list-mode)
    (cond
     ((null (active-minibuffer-window))
      (minibuffer-hide-completions))
     ((use-region-p) (keyboard-quit))
     (t (abort-recursive-edit)))))

;;;; Global minor mode setup

;;;;; Stylistic tweaks and refinements

;; Note that this solves bug#45686:
;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=45686>
;; TODO review that stealthily does not affect the region mode, it seems intrusive.
(defun mct--stealthily (&rest app)
  "Prevent minibuffer default from counting as a modification.
Apply APP while inhibiting modification hooks."
  (let ((inhibit-modification-hooks t))
    (apply app)))

(defun mct--setup-appearance ()
  "Set up variables for the appearance of the Completions' buffer."
  (when mct-hide-completion-mode-line
    (setq-local mode-line-format nil))
  (if mct-apply-completion-stripes
      (mct--add-stripes)
    (mct--remove-stripes)))

;;;;; Shadowed path

;; Adapted from icomplete.el
(defun mct--shadow-filenames (&rest _)
  "Hide shadowed file names."
  (let ((saved-point (point)))
    (when (and
           mct-remove-shadowed-file-names
           (eq (mct--completion-category) 'file)
           rfn-eshadow-overlay (overlay-buffer rfn-eshadow-overlay)
           (eq (mct--this-command) 'self-insert-command)
           (= saved-point (point-max))
           (or (>= (- (point) (overlay-end rfn-eshadow-overlay)) 2)
               (eq ?/ (char-before (- (point) 2)))))
      (delete-region (overlay-start rfn-eshadow-overlay)
                     (overlay-end rfn-eshadow-overlay)))))

(defun mct--setup-shadow-files ()
  "Set up shadowed file name deletion."
  (add-hook 'after-change-functions #'mct--shadow-filenames nil t))

;;;;; Highlight current candidate

(defvar-local mct--highlight-overlay nil
  "Overlay to highlight candidate in the Completions' buffer.")

(defvar mct--overlay-priority -50
  "Priority used on the `mct--highlight-overlay'.
This value means that it takes precedence over lines that have
the `mct-stripe' face, while it is overriden by the active
region.")

;; This is for Emacs 27 which does not have a completion--string text
;; property.
(defun mct--completions-text-property-search ()
  "Search for text property of completion candidate."
  (or (text-property-search-forward 'completion--string)
      (text-property-search-forward 'mouse-face)))

;; The `if-let' is to prevent highlighting of empty space, such as by
;; clicking on it with the mouse.
(defun mct--completions-completion-beg ()
  "Return point of completion candidate at START and END."
  (if-let ((string (mct--completion-at-point-p)))
      (save-excursion
        (prop-match-beginning (mct--completions-text-property-search)))
    (point)))

;; Same as above for the `if-let'.
(defun mct--completions-completion-end ()
  "Return end of completion candidate."
  (if-let ((string (mct--completion-at-point-p)))
      (save-excursion
        (if (mct--one-column-p)
            (1+ (point-at-eol))
          (prop-match-end (mct--completions-text-property-search))))
    (point)))

(defun mct--overlay-make ()
  "Make overlay to highlight current candidate."
  (let ((ol (make-overlay (point) (point))))
    (overlay-put ol 'priority mct--overlay-priority)
    (overlay-put ol 'face 'mct-highlight-candidate)
    ol))

(defun mct--overlay-move (overlay)
  "Highlight the candidate at point with OVERLAY."
  (let* ((beg (mct--completions-completion-beg))
         (end (mct--completions-completion-end)))
	(move-overlay overlay beg end)))

(defun mct--completions-candidate-highlight ()
  "Activate `mct--highlight-overlay'."
  (unless (overlayp mct--highlight-overlay)
    (setq mct--highlight-overlay (mct--overlay-make)))
  (mct--overlay-move mct--highlight-overlay))

(defun mct--setup-highlighting ()
  "Highlight the current completion in the Completions' buffer."
  (add-hook 'post-command-hook #'mct--completions-candidate-highlight nil t))

;;;;; Keymaps

(defvar mct-minibuffer-completion-list-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap keyboard-quit] #'mct-keyboard-quit-dwim)
    (define-key map [remap next-line] #'mct-next-completion-or-mini)
    (define-key map (kbd "n") #'mct-next-completion-or-mini)
    (define-key map [remap previous-line] #'mct-previous-completion-or-mini)
    (define-key map (kbd "p") #'mct-previous-completion-or-mini)
    (define-key map [remap backward-paragraph] #'mct-previous-completion-group)
    (define-key map [remap forward-paragraph] #'mct-next-completion-group)
    (define-key map (kbd "M-p") #'mct-previous-completion-group)
    (define-key map (kbd "M-n") #'mct-next-completion-group)
    (define-key map (kbd "e") #'mct-focus-minibuffer)
    (define-key map (kbd "M-e") #'mct-edit-completion)
    (define-key map (kbd "TAB") #'mct-choose-completion-no-exit)
    (define-key map (kbd "RET") #'mct-choose-completion-exit)
    (define-key map (kbd "M-RET") #'mct-choose-completion-dwim)
    (define-key map [remap beginning-of-buffer] #'mct-beginning-of-buffer)
    map)
  "Derivative of `completion-list-mode-map'.")

(defvar mct-minibuffer-local-completion-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-j") #'exit-minibuffer)
    (define-key map [remap next-line] #'mct-switch-to-completions-top)
    (define-key map [remap next-line-or-history-element] #'mct-switch-to-completions-top)
    (define-key map [remap previous-line] #'mct-switch-to-completions-bottom)
    (define-key map [remap previous-line-or-history-element] #'mct-switch-to-completions-bottom)
    (define-key map (kbd "M-e") #'mct-edit-completion)
    (define-key map (kbd "C-<return>") #'mct-complete-and-exit)
    (define-key map (kbd "C-l") #'mct-list-completions-toggle)
    map)
  "Derivative of `minibuffer-local-completion-map'.")

(defun mct--setup-completion-list-keymap ()
  "Set up completion list keymap."
  (use-local-map
   (make-composed-keymap mct-minibuffer-completion-list-map
                         (current-local-map))))

(defun mct--setup-minibuffer-keymap ()
  "Set up minibuffer keymap."
  (use-local-map
   (make-composed-keymap mct-minibuffer-local-completion-map
                         (current-local-map))))

(defun mct--setup-completion-list ()
  "Set up the completion-list for Mct."
  (when (mct--minibuffer-p)
    (setq-local completion-show-help nil
                completion-wrap-movement nil ; Emacs 29
                truncate-lines t)
    (mct--setup-clean-completions)
    (mct--setup-appearance)
    (mct--setup-completion-list-keymap)
    (mct--setup-highlighting)
    (cursor-sensor-mode)))

;;;;; Dynamic completion

;; TODO 2022-01-29: Research how things work for relevant cases in
;; completion-in-region and adapt accordingly.
(defun mct--persist-dynamic-completion (&rest _)
  "Persist completion, per `mct-persist-dynamic-completion'."
  (when (and (not (mct--symbol-in-list mct-completion-blocklist))
             mct-persist-dynamic-completion
             (memq (mct--completion-category) mct--dynamic-completion-categories)
             mct-live-completion)
    (mct-focus-minibuffer)
    (mct--show-completions)))

(defun mct--setup-dynamic-completion-persist ()
  "Set up `mct-persist-dynamic-completion'."
  (let ((commands '(choose-completion minibuffer-complete minibuffer-force-complete)))
    (if (bound-and-true-p mct-minibuffer-mode)
        (dolist (fn commands)
          (advice-add fn :after #'mct--persist-dynamic-completion))
      (dolist (fn commands)
        (advice-remove fn #'mct--persist-dynamic-completion)))))

;;;;; mct-minibuffer-mode declaration

(declare-function minibuf-eldef-setup-minibuffer "minibuf-eldef")

;;;###autoload
(define-minor-mode mct-minibuffer-mode
  "Set up opinionated default completion UI."
  :global t
  :group 'mct
  (if mct-minibuffer-mode
      (progn
        (add-hook 'completion-list-mode-hook #'mct--setup-completion-list)
        (add-hook 'minibuffer-setup-hook #'mct--setup-passlist)
        (advice-add #'completing-read-default :around #'mct--completing-read-advice)
        (advice-add #'completing-read-multiple :around #'mct--completing-read-advice)
        (advice-add #'minibuffer-completion-help :around #'mct--minibuffer-completion-help-advice)
        (advice-add #'minibuf-eldef-setup-minibuffer :around #'mct--stealthily))
    (remove-hook 'completion-list-mode-hook #'mct--setup-completion-list)
    (remove-hook 'minibuffer-setup-hook #'mct--setup-passlist)
    (advice-remove #'completing-read-default #'mct--completing-read-advice)
    (advice-remove #'completing-read-multiple #'mct--completing-read-advice)
    (advice-remove #'minibuffer-completion-help #'mct--minibuffer-completion-help-advice)
    (advice-remove #'minibuf-eldef-setup-minibuffer #'mct--stealthily))
  (mct--setup-dynamic-completion-persist)
  (mct--setup-shared))

(define-obsolete-function-alias 'mct-mode 'mct-minibuffer-mode "0.4.0")

;;;;; mct-region-mode declaration

;;;;;; Live completions

(defun mct--region-current-buffer ()
  "Return current buffer of completion in region."
  (and completion-in-region--data
       (marker-buffer (nth 0 completion-in-region--data))))

(defun mct--region-live-completions (&rest _)
  "Update the *Completions* buffer.
Meant to be added to `after-change-functions'."
  (when-let (buf (mct--region-current-buffer))
    (while-no-input
      (condition-case nil
          (save-match-data
            (mct--show-completions))
        (quit (keyboard-quit))))))

(defun mct--region-live-update ()
  "Hook up `mct--region-live-completions'."
  (add-hook 'after-change-functions #'mct--region-live-completions nil t))

;;;;;; Minor mode specification

(defvar mct-region-buffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap next-line] #'mct-switch-to-completions-top)
    (define-key map [remap previous-line] #'mct-switch-to-completions-bottom)
    ;; TODO: Either keep the TAB=completion-at-point binding or add our own
    ;; command which is compatible with orderless completion.
    (define-key map (kbd "TAB") #'ignore)
    map)
  "Derivative of `completion-in-region-map'.")

(defun mct--region-setup-completion-in-region ()
  "Set up Mct for `completion-in-region'."
  (if completion-in-region-mode
      (progn
        (setcdr (assq #'completion-in-region-mode minor-mode-overriding-map-alist)
                (make-composed-keymap mct-region-buffer-map completion-in-region-mode-map))
        (mct--region-current-buffer)
        ;; NOTE: Ignore the predicate in order to support orderless style.
        ;; TODO: This override should be guarded by a customizable variable,
        ;; since it is intrusive. See also `corfu-quit-at-boundary'.
        (setq completion-in-region-mode--predicate (lambda () t))
        (mct--region-live-update))
    ;; Teardown
    (remove-hook 'after-change-functions #'mct--region-live-completions t)))

(defun mct-next-completion-or-quit (&optional arg)
  "Move to next completion or bury the Completions' buffer.

This performs a regular motion for optional ARG candidates, but
when point can no longer move in that direction it buries the
Completions' buffer.

This is a counterpart of `mct-next-completion-or-mini' that is
meant for the case of completion in region (i.e. not in the
minibuffer)."
  (interactive nil mct-region-mode)
  (let ((count (or arg 1)))
    (cond
     ((mct--bottom-of-completions-p count)
      (minibuffer-hide-completions))
     (t
      (mct--next-completion count)))))

(defun mct-previous-completion-or-quit (&optional arg)
  "Move to previous completion or bury the Completions' buffer.

This performs a regular motion for optional ARG candidates, but
when point can no longer move in that direction it buries the
Completions' buffer.

This is a counterpart of `mct-previous-completion-or-mini' that
is meant for the case of completion in region (i.e. not in the
minibuffer)."
  (interactive nil mct-region-mode)
  (let ((count (if (natnump arg) arg 1)))
    (cond
     ((mct--top-of-completions-p count)
      (minibuffer-hide-completions))
     (t
      (mct--previous-completion count)))))

(defvar mct-region-completion-list-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap next-line] #'mct-next-completion-or-quit)
    (define-key map [remap previous-line] #'mct-previous-completion-or-quit)
    (define-key map (kbd "n") #'mct-next-completion-or-quit)
    (define-key map (kbd "p") #'mct-previous-completion-or-quit)
    (define-key map [remap backward-paragraph] #'mct-previous-completion-group)
    (define-key map [remap forward-paragraph] #'mct-next-completion-group)
    (define-key map (kbd "M-n") #'mct-next-completion-group)
    (define-key map (kbd "M-p") #'mct-previous-completion-group)
    (define-key map (kbd "TAB") #'choose-completion)
    (define-key map (kbd "RET") #'choose-completion)
    (define-key map [remap beginning-of-buffer] #'mct-beginning-of-buffer)
    map)
  "Derivative of `completion-list-mode-map'.")

(defun mct--region-setup-completion-list-keymap ()
  "Set up completion list keymap."
  (use-local-map
   (make-composed-keymap mct-region-completion-list-map
                         (current-local-map))))

(defun mct--region-setup-completion-list ()
  "Set up the completion-list for Mct."
  (when (mct--region-p)
    (setq-local completion-show-help nil
                completion-wrap-movement nil ; Emacs 29
                truncate-lines t)
    (mct--setup-clean-completions)
    (mct--setup-appearance)
    (mct--region-setup-completion-list-keymap)
    (mct--setup-highlighting)
    (cursor-sensor-mode)))

(defun mct--region-completion-done (&rest app)
  "Apply APP before disabling completion in region."
  (apply app)
  (completion-in-region-mode -1))

;; UPDATE 2022-01-01 13:12 +0200: Actually this is not related to mct.
;; I can reproduce it in emacs -Q with just those:
;;
;; (electric-indent-mode 1)
;; (setq-default tab-always-indent 'complete)
;;
;; FIXME 2022-01-01: I experienced a bug which is as follows:
;;
;; + (electric-indent-mode 1)
;; + (setq-default tab-always-indent 'complete)
;; + visit an Org file with an elisp src block that includes comments
;; + go to the end of a comment's line and hit RET
;; + the block temporarily changes background to secondary-selection
;;   (same as when you type C-c '), seems to be trying to perform
;;   completion, and then ultimately does what RET is supposed to do.
;;
;; Disabling electric-indent-mode fixes the issue, though that is beside
;; the point.

;; FIXME 2022-01-03: This does not work with either M-x shell or M-x
;; eshell.  Not on Emacs 29, not on Emacs 27.  Try to tab-complete with
;; 'cd' and it will not be possible to switch to the Completions'
;; buffer, even if it works the first time.

;;;###autoload
(define-minor-mode mct-region-mode
  "Set up interactivity over the default `completion-in-region'."
  :global t
  (if mct-region-mode
      (progn
        (advice-add #'completion--done :around #'mct--region-completion-done)
        (advice-add #'minibuffer-completion-help :around #'mct--region-completion-help-advice)
        (add-hook 'completion-list-mode-hook #'mct--region-setup-completion-list)
        (add-hook 'completion-in-region-mode-hook #'mct--region-setup-completion-in-region))
    (advice-remove #'completion--done #'mct--region-completion-done)
    (advice-remove #'minibuffer-completion-help #'mct--region-completion-help-advice)
    (remove-hook 'completion-list-mode-hook #'mct--region-setup-completion-list)
    (remove-hook 'completion-in-region-mode-hook #'mct--region-setup-completion-in-region))
  (mct--setup-shared))

;; Adapted from Omar Antolín Camarena's live-completions library:
;; <https://github.com/oantolin/live-completions>.
(defun mct--shared-honor-inhibit-message (&rest app)
  "Honor `inhibit-message' while applying APP."
  (unless (and (or (mct--region-p) (mct--minibuffer-p)) inhibit-message)
    (apply app)))

;; Thanks to Omar Antolín Camarena for providing the messageless and
;; stealthily.  Source: <https://github.com/oantolin/emacs-config>.
(defun mct--shared-messageless (&rest app)
  "Set `minibuffer-message-timeout' to 0 while applying APP."
  (if (or (mct--region-p) (mct--minibuffer-p))
      (let ((minibuffer-message-timeout 0))
        (apply app))
    (apply app)))

(defun mct--setup-shared ()
  "Silence the minibuffer and the Completions."
  (if (or mct-region-mode mct-minibuffer-mode)
      (progn
        ;; NOTE 2022-01-09: Only `choose-completion' is relevant for
        ;; completion-in-region.
        (dolist (fn '(exit-minibuffer
                      choose-completion
                      minibuffer-force-complete
                      minibuffer-complete-and-exit
                      minibuffer-force-complete-and-exit))
          (advice-add fn :around #'mct--shared-messageless))
        (advice-add #'minibuffer-message :around #'mct--shared-honor-inhibit-message))
    (dolist (fn '(exit-minibuffer
                  choose-completion
                  minibuffer-force-complete
                  minibuffer-complete-and-exit
                  minibuffer-force-complete-and-exit))
      (advice-remove fn #'mct--shared-messageless))
    (advice-remove #'minibuffer-message #'mct--shared-honor-inhibit-message)))

(provide 'mct)
;;; mct.el ends here
