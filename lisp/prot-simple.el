;;; prot-simple.el --- Common commands for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
;; Version: 0.1.0
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
;; Common commands for my Emacs: <https://protesilaos.com/emacs/dotemacs/>.
;;
;; Remember that every piece of Elisp that I write is for my own
;; educational and recreational purposes.  I am not a programmer and I
;; do not recommend that you copy any of this if you are not certain of
;; what it does.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'prot-common)

(defgroup prot-simple ()
  "Generic utilities for my dotemacs."
  :group 'editing)

;; Got those numbers from `string-to-char'
(defcustom prot-simple-insert-pair-alist
  '(("' Single quote"        . (39 39))     ; ' '
    ("\" Double quotes"      . (34 34))     ; " "
    ("` Elisp quote"         . (96 39))     ; ` '
    ("‘ Single apostrophe"   . (8216 8217)) ; ‘ ’
    ("“ Double apostrophes"  . (8220 8221)) ; “ ”
    ("( Parentheses"         . (40 41))     ; ( )
    ("{ Curly brackets"      . (123 125))   ; { }
    ("[ Square brackets"     . (91 93))     ; [ ]
    ("< Angled brackets"     . (60 62))     ; < >
    ("« Εισαγωγικά Gr quote" . (171 187))   ; « »
    ("= Equals signs"        . (61 61))     ; = =
    ("~ Tilde"               . (126 126))   ; ~ ~
    ("* Asterisks"           . (42 42))     ; * *
    ("/ Forward Slash"       . (47 47))     ; / /
    ("_ underscores"         . (95 95)))    ; _ _
  "Alist of pairs for use with `prot-simple-insert-pair-completion'."
  :type 'alist
  :group 'prot-simple)

(defcustom prot-simple-date-specifier "%F"
  "Date specifier for `format-time-string'.
Used by `prot-simple-inset-date'."
  :type 'string
  :group 'prot-simple)

(defcustom prot-simple-time-specifier "%R %z"
  "Time specifier for `format-time-string'.
Used by `prot-simple-inset-date'."
  :type 'string
  :group 'prot-simple)

(defcustom prot-simple-focusable-help-commands
  '( describe-symbol describe-function
     describe-variable describe-key
     view-lossage)
  "Commands whose buffers should be focused when displayed.
This makes it easier to dismiss them at once.

Also see `prot-simple-focus-help-buffers'."
  :type '(repeat symbol)
  :group 'prot-simple)

(defcustom prot-simple-scratch-buffer-default-mode 'markdown-mode
  "Default major mode for `prot-simple-scratch-buffer'."
  :type 'symbol
  :group 'prot-simple)

;;; Generic setup

;;;; Scratch buffers
;; The idea is based on the `scratch.el' package by Ian Eure:
;; <https://github.com/ieure/scratch-el>.

;; Adapted from the `scratch.el' package by Ian Eure.
(defun prot-simple--scratch-list-modes ()
  "List known major modes."
  (cl-loop for sym the symbols of obarray
           for name = (symbol-name sym)
           when (and (functionp sym)
                     (not (member sym minor-mode-list))
                     (string-match "-mode$" name)
                     (not (string-match "--" name)))
           collect name))

(defun prot-simple--scratch-buffer-setup (region &optional mode)
  "Add contents to `scratch' buffer and name it accordingly.

REGION is added to the contents to the new buffer.

Use the current buffer's major mode by default.  With optional
MODE use that major mode instead."
  (let* ((major (or mode major-mode))
         (string (format "Scratch buffer for: %s\n\n" major))
         (text (concat string region))
         (buf (format "*Scratch for %s*" major)))
    (with-current-buffer (get-buffer-create buf)
      (funcall major)
	  (save-excursion
        (insert text)
        (goto-char (point-min))
        (comment-region (point-at-bol) (point-at-eol)))
	  (vertical-motion 2))
    (pop-to-buffer buf)))

;;;###autoload
(defun prot-simple-scratch-buffer (&optional arg)
  "Produce a bespoke scratch buffer matching current major mode.

With optional ARG as a prefix argument (\\[universal-argument]),
use `prot-simple-scratch-buffer-default-mode'.

With ARG as a double prefix argument, prompt for a major mode
with completion.

If region is active, copy its contents to the new scratch
buffer."
  (interactive "P")
  (let* ((default-mode prot-simple-scratch-buffer-default-mode)
         (modes (prot-simple--scratch-list-modes))
         (region (with-current-buffer (current-buffer)
                   (if (region-active-p)
                       (buffer-substring-no-properties
                        (region-beginning)
                        (region-end))
                     "")))
         (m))
    (pcase (prefix-numeric-value arg)
      (16 (progn
            (setq m (intern (completing-read "Select major mode: " modes nil t)))
            (prot-simple--scratch-buffer-setup region m)))
      (4 (prot-simple--scratch-buffer-setup region default-mode))
      (_ (prot-simple--scratch-buffer-setup region)))))

;; ;; DEPRECATED 2021-10-15: Just set `help-window-select' to non-nil.
;;
;; ;;;; Focus auxiliary buffers
;;
;; ;; TODO 2021-08-27: Is there a more general way to do this without
;; ;; specifying the BUF?  That way we would only need one function.
;; (defmacro prot-simple--auto-focus-buffer (fn doc buf)
;;   "Produce FN with DOC for focusing BUF."
;;   `(defun ,fn (&rest _)
;;     ,doc
;;     (when-let ((window (get-buffer-window ,buf)))
;;       (select-window window))))
;;
;; (prot-simple--auto-focus-buffer
;;  prot-simple--help-focus
;;   "Select window with Help buffer.
;; Intended as :after advice for `describe-symbol' and friends."
;;   (help-buffer))
;;
;; (prot-simple--auto-focus-buffer
;;  prot-simple--messages-focus
;;   "Select window with Help buffer.
;; Intended as :after advice for `view-echo-area-messages'."
;;   (messages-buffer))
;;
;; ;;;###autoload
;; (define-minor-mode prot-simple-focus-help-buffers
;;   "Add advice to focus `prot-simple-focusable-help-commands'."
;;   :lighter nil
;;   (if prot-simple-focus-help-buffers
;;       (progn
;;         (dolist (fn prot-simple-focusable-help-commands)
;;           (advice-add fn :after 'prot-simple--help-focus))
;;         (advice-add 'view-echo-area-messages :after 'prot-simple--messages-focus))
;;     (dolist (fn prot-simple-focusable-help-commands)
;;       (advice-remove fn 'prot-simple--help-focus))
;;     (advice-remove 'view-echo-area-messages 'prot-simple--messages-focus)))

;; ;;;; Rename Help buffers (EXPERIMENTAL)
;;
;; (defvar prot-simple-help-mode-post-render-hook nil
;;   "Hook that runs after Help is rendered (via `advice-add').")
;;
;; (defun prot-simple--help-mode-post-render (&rest _)
;;   "Run `prot-simple-help-mode-post-render-hook'."
;;   (run-hooks 'prot-simple-help-mode-post-render-hook))
;;
;; (defconst prot-simple--help-symbol-regexp
;;   ;; TODO 2021-10-12: Avoid duplication in regexp.
;;   (concat
;;    "^\\(.*?\\)\s\\(is an?\\|runs the\\)\s\\(command\\|function\\|variable\\|keymap variable"
;;    "\\|native compiled Lisp function\\|interactive native compiled Lisp function"
;;    "\\|built-in function\\|interactive built-in function\\|Lisp closure\\)\s"
;;    "\\(\\_<.*?\\_>\\)\\( (found in .*)\\)?")
;;   "Regexp to match Help buffer description.")
;;
;; (defconst prot-simple--help-symbol-false-positives
;;   "\\(in\\|defined\\)"
;;   "False positives for `prot-simple--help-symbol-regexp'.")
;;
;; (defun prot-simple--rename-help-buffer ()
;;   "Rename the current Help buffer."
;;   (with-current-buffer (help-buffer)
;;     (goto-char (point-min))
;;     (when (re-search-forward prot-simple--help-symbol-regexp nil t)
;;       (let* ((thing (match-string 1))
;;              (symbol (match-string 4))
;;              (scope (match-string 5))
;;              (description (cond
;;                            (scope
;;                             (concat symbol scope))
;;                            ((and (not (string-match-p prot-simple--help-symbol-false-positives symbol))
;;                                  (symbolp (intern symbol)))
;;                             symbol)
;;                            ((match-string 3)))))
;;         (rename-buffer
;;          (format "*%s (%s) # Help*" thing description)
;;          t)))))
;;
;; ;;;###autoload
;; (define-minor-mode prot-simple-rename-help-buffers
;;   "Rename Help buffers based on their contents."
;;   :init-value nil
;;   :global t
;;   (if prot-simple-rename-help-buffers
;;       (progn
;;         (advice-add #'help-window-setup :after #'prot-simple--help-mode-post-render)
;;         (add-hook 'prot-simple-help-mode-post-render-hook #'prot-simple--rename-help-buffer))
;;     (advice-remove #'help-window-setup #'prot-simple--help-mode-post-render)
;;     (remove-hook 'prot-simple-help-mode-post-render-hook #'prot-simple--rename-help-buffer)))

;;; Commands

;;;; General commands

(autoload 'symbol-at-point "thingatpt")

;;;###autoload
(defun prot-simple-describe-symbol ()
  "Run `describe-symbol' for the `symbol-at-point'."
  (interactive)
  (describe-symbol (symbol-at-point)))

;;;; Commands for lines

;;;###autoload
(defun prot-simple-new-line-below (&optional arg)
  "Create an empty line below the current one.
Move the point to the absolute beginning.  Adapt indentation by
passing optional prefix ARG (\\[universal-argument]).  Also see
`prot-simple-new-line-above'."
  (interactive "P")
  (end-of-line)
  (if arg
      (newline-and-indent)
    (newline)))

;;;###autoload
(defun prot-simple-new-line-above (&optional arg)
  "Create an empty line above the current one.
Move the point to the absolute beginning.  Adapt indentation by
passing optional prefix ARG (\\[universal-argument])."
  (interactive "P")
  (let ((indent (or arg nil)))
    (if (or (bobp)
            (line-number-at-pos (point-min)))
        (progn
          (beginning-of-line)
          (newline)
          (forward-line -1))
      (forward-line -1)
      (prot-simple-new-line-below indent))))

;;;###autoload
(defun prot-simple-copy-line-or-region (&optional arg)
  "Kill-save the current line or active region.
With optional ARG (\\[universal-argument]) duplicate the target
instead.  When region is active, also apply context-aware
indentation while duplicating."
  (interactive "P")
  (unless mark-ring                  ; needed when entering a new buffer
    (push-mark (point) t nil))
  (let* ((rbeg (region-beginning))
         (rend (region-end))
         (pbol (point-at-bol))
         (peol (point-at-eol))
         (indent (if (eq (or rbeg rend) pbol) nil arg)))
    (cond
     ((use-region-p)
      (if arg
          (let ((text (buffer-substring rbeg rend)))
            (when (eq (point) rbeg)
              (exchange-point-and-mark))
            (prot-simple-new-line-below indent)
            (insert text))
        (copy-region-as-kill rbeg rend)
        (message "Current region copied")))
     (t
      (if arg
          (let ((text (buffer-substring pbol peol)))
            (goto-char (point-at-eol))
            (newline)
            (insert text))
        (copy-region-as-kill pbol peol)
        (message "Current line copied"))))))

;;;###autoload
(defun prot-simple-yank-replace-line-or-region ()
  "Replace line or region with latest kill.
This command can then be followed by the standard
`yank-pop' (default is bound to \\[yank-pop])."
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-region (point-at-bol) (point-at-eol)))
  (yank))

;;;###autoload
(defun prot-simple-multi-line-next ()
  "Move point 15 lines down."
  (interactive)
  (forward-line 15))

;;;###autoload
(defun prot-simple-multi-line-prev ()
  "Move point 15 lines up."
  (interactive)
  (forward-line -15))

;;;###autoload
(defun prot-simple-kill-line-backward ()
  "Kill from point to the beginning of the line."
  (interactive)
  (kill-line 0))

;;;; Commands for text insertion or manipulation

(defvar prot-simple--character-hist '()
  "History of inputs for `prot-simple-insert-pair-completion'.")

(defun prot-simple--character-prompt (chars)
  "Helper of `prot-simple-insert-pair-completion' to read CHARS."
  (let ((def (car prot-simple--character-hist)))
    (completing-read
     (format "Select character [%s]: " def)
     chars nil t nil 'prot-simple--character-hist def)))

(define-obsolete-function-alias
  'prot-simple-insert-pair-completion
  'prot-simple-insert-pair "2021-07-30")

;;;###autoload
(defun prot-simple-insert-pair (pair &optional count)
  "Insert PAIR from `prot-simple-insert-pair-alist'.
Operate on the symbol at point.  If the region is active, use it
instead.

With optional COUNT (either as a natural number from Lisp or a
universal prefix argument (\\[universal-argument]) when used
interactively) prompt for the number of delimiters to insert."
  (interactive
   (list
    (prot-simple--character-prompt prot-simple-insert-pair-alist)
    current-prefix-arg))
  (let* ((data prot-simple-insert-pair-alist)
         (left (cadr (assoc pair data)))
         (right (caddr (assoc pair data)))
         (n (cond
             ((and count (natnump count))
              count)
             (count
              (read-number "How many delimiters?" 2))
             (1)))
         (beg)
         (end))
    (cond
     ((region-active-p)
      (setq beg (region-beginning)
            end (region-end)))
     ((when (thing-at-point 'symbol)
        (let ((bounds (bounds-of-thing-at-point 'symbol)))
          (setq beg (car bounds)
                end (cdr bounds)))))
     (t (setq beg (point)
              end (point))))
    (save-excursion
      (goto-char end)
      (dotimes (_ n)
        (insert right))
      (goto-char beg)
      (dotimes (_ n)
        (insert left)))))

;;;###autoload
(defun prot-simple-delete-pair-dwim ()
  "Delete pair following or preceding point.
For Emacs version 28 or higher, the feedback's delay is
controlled by `delete-pair-blink-delay'."
  (interactive)
  (if (eq (point) (cdr (bounds-of-thing-at-point 'sexp)))
      (delete-pair -1)
    (delete-pair 1)))

;;;###autoload
(defun prot-simple-insert-date (&optional arg)
  "Insert the current date as `prot-simple-date-specifier'.

With optional prefix ARG (\\[universal-argument]) also append the
current time understood as `prot-simple-time-specifier'.

When region is active, delete the highlighted text and replace it
with the specified date."
  (interactive "P")
  (let* ((date prot-simple-date-specifier)
         (time prot-simple-time-specifier)
         (format (if arg (format "%s %s" date time) date)))
    (when (use-region-p)
      (delete-region (region-beginning) (region-end)))
    (insert (format-time-string format))))


(autoload 'ffap-url-at-point "ffap")
(defvar ffap-string-at-point-region)

;;;###autoload
(defun prot-simple-escape-url ()
  "Wrap URL (or email address) in angled brackets."
  (interactive)
  (when-let ((url (ffap-url-at-point)))
    (let* ((reg ffap-string-at-point-region)
           (beg (car reg))
           (end (cadr reg))
           (string (if (string-match-p "^mailto:" url)
                       (substring url 7)
                     url)))
      (delete-region beg end)
      (insert (format "<%s>" string)))))

;;;###autoload
(defun prot-simple-cite-region (beg end &optional arg)
  "Cite text in region lines between BEG and END.

Region lines are always understood in absolute terms, regardless
of whether the region boundaries coincide with them.

With optional prefix ARG (\\[universal-argument]) prompt for a
description that will be placed on a new line at the top of the
newly formatted text."
  (interactive "*r\nP")
  (let* ((absolute-beg (if (< beg end)
                           (progn (goto-char beg) (point-at-bol))
                         (progn (goto-char end) (point-at-eol))))
         (absolute-end (if (< beg end)
                           (progn (goto-char end) (point-at-eol))
                         (progn (goto-char beg) (point-at-bol))))
         (prefix-text (if (< beg end)
                          (buffer-substring-no-properties absolute-beg beg)
                        (buffer-substring-no-properties absolute-end end)))
         (prefix (if (string-match-p "\\`[\t\s]+\\'" prefix-text)
                     prefix-text
                   (replace-regexp-in-string "\\`\\([\t\s]+\\).*" "\\1" prefix-text)))
         (description (if arg
                          (format "+----[ %s ]\n"
                                  (read-string "Add description: "))
                        "+----\n"))
         (marked-text (buffer-substring-no-properties absolute-beg absolute-end))
         (marked-text-new (replace-regexp-in-string "^.*?" (concat prefix "|") marked-text))
         (text (with-temp-buffer
                 (insert marked-text-new)
                 (save-excursion
                   (goto-char (point-min))
                   (re-search-forward "^\\(^[\s\t]+\\)?.*?")
                   (forward-line -1)
                   (insert (concat prefix description))
                   (goto-char (point-max))
                   (forward-line 1)
                   (insert "\n")
                   (insert (concat prefix "+----")))
                 (buffer-substring-no-properties (point-min) (point-max)))))
    (delete-region absolute-beg absolute-end)
    (insert text)))

;; `prot-simple-insert-undercaret' was offered to me by Gregory
;; Heytings: <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=45068#250>.
;;;###autoload
(defun prot-simple-insert-undercaret (&optional arg)
  "Draw carets below the characters on the current line or region."
  (interactive "p")
  (let* ((begin (if (region-active-p) (region-beginning) (line-beginning-position)))
         (end (if (region-active-p) (region-end) (line-end-position)))
         (lines (- (line-number-at-pos end) (line-number-at-pos begin) -1))
         (comment (and (/= arg 1) (= lines 1)))
         (final-forward-line -1))
    (goto-char begin)
    (dotimes (i lines)
      (let* ((line-begin (if (zerop i) begin (line-beginning-position)))
             (line-end (if (= (1+ i) lines) end (line-end-position)))
             (begin-column (progn (goto-char line-begin) (current-column)))
             (end-column (progn (goto-char line-end) (current-column)))
             (prefix-begin (line-beginning-position))
             (prefix-end (progn (beginning-of-line-text) (point)))
             (prefix-end-column (progn (goto-char prefix-end) (current-column)))
             (delta (if (< begin-column prefix-end-column) (- prefix-end-column begin-column) 0))
             (prefix-string (buffer-substring-no-properties prefix-begin prefix-end))
             (prefix (if (string-match-p "\\` *\\'" prefix-string) "" prefix-string))
             (whitespace (make-string (- (+ begin-column delta) (string-width prefix)) ?\ ))
             (do-under (< delta (- line-end line-begin)))
             (under (if do-under (make-string (- end-column begin-column delta) ?^) ""))
             (under-string (concat prefix whitespace under "\n")))
        (forward-line 1)
        (if do-under (insert under-string) (setq final-forward-line -2))
        (setq end (+ end (length under-string)))
        (when comment (insert prefix whitespace "\n"))))
    (forward-line final-forward-line)
    (goto-char (line-end-position))))

;;;; Commands for object transposition

(defmacro prot-simple-transpose (name scope &optional doc)
  "Macro to produce transposition functions.
NAME is the function's symbol.  SCOPE is the text object to
operate on.  Optional DOC is the function's docstring.

Transposition over an active region will swap the object at
mark (region beginning) with the one at point (region end)"
  `(defun ,name (arg)
     ,doc
     (interactive "p")
     (let ((x (format "%s-%s" "transpose" ,scope)))
       (if (use-region-p)
           (funcall (intern x) 0)
         (funcall (intern x) arg)))))

(prot-simple-transpose
 prot-simple-transpose-lines
 "lines"
 "Transpose lines or swap over active region.")

(prot-simple-transpose
 prot-simple-transpose-paragraphs
 "paragraphs"
 "Transpose paragraphs or swap over active region.")

(prot-simple-transpose
 prot-simple-transpose-sentences
 "sentences"
 "Transpose sentences or swap over active region.")

(prot-simple-transpose
 prot-simple-transpose-sexps
 "sexps"
 "Transpose balanced expressions or swap over active region.")

;;;###autoload
(defun prot-simple-transpose-chars ()
  "Always transposes the two characters before point.
There is no 'dragging' the character forward.  This is the
behaviour of `transpose-chars' when point is at the end of the
line."
  (interactive)
  (transpose-chars -1)
  (forward-char))

;;;###autoload
(defun prot-simple-transpose-words (arg)
  "Transpose ARG words.

If region is active, swap the word at mark (region beginning)
with the one at point (region end).

Otherwise, and while inside a sentence, this behaves as the
built-in `transpose-words', dragging forward the word behind the
point.  The difference lies in its behaviour at the end or
beginnning of a line, where it will always transpose the word at
point with the one behind or ahead of it (effectively the
last/first two words)."
  (interactive "p")
  (cond
   ((use-region-p)
    (transpose-words 0))
   ((eq (point) (point-at-eol))
    (transpose-words -1))
   ((eq (point) (point-at-bol))
    (forward-word 1)
    (transpose-words 1))
   (t
    (transpose-words arg))))

;;;; Commands for marking syntactic constructs

(defmacro prot-simple-mark (name object &optional docstring)
  "Produce function for marking small syntactic constructs.
NAME is how the function should be called.  OBJECT is its scope.
Optional DOCSTRING describes the resulting function.

This is a slightly modified version of the built-in `mark-word'."
  `(defun ,name (&optional arg allow-extend)
     ,docstring
     (interactive "P\np")
     (let ((x (format "%s-%s" "forward" ,object)))
       (cond ((and allow-extend
                   (or (and (eq last-command this-command) (mark t))
                       (region-active-p)))
              (setq arg (if arg (prefix-numeric-value arg)
                          (if (< (mark) (point)) -1 1)))
              (set-mark
               (save-excursion
                 (goto-char (mark))
                 (funcall (intern x) arg)
                 (point))))
             (t
              (let ((bounds (bounds-of-thing-at-point (intern ,object))))
                (unless (consp bounds)
                  (user-error "No %s at point" ,object))
                (if (>= (prefix-numeric-value arg) 0)
                    (goto-char (car bounds))
                  (goto-char (cdr bounds)))
                (push-mark
                 (save-excursion
                   (funcall (intern x) (prefix-numeric-value arg))
                   (point)))
                (activate-mark)))))))

(prot-simple-mark
 prot-simple-mark-word
 "word"
 "Mark the whole word at point.
This function is a slightly modified version of the built-in
`mark-word', that I intend to use only in special circumstances,
such as when recording a keyboard macro where precision is
required.  For a general purpose utility, use `prot-simple-mark-symbol'
instead.")

(prot-simple-mark
 prot-simple-mark-symbol
 "symbol"
 "Mark the whole symbol at point.
With optional ARG, mark the current symbol and any remaining
ARGth symbols away from point.  A negative argument moves
backward. Repeated invocations of this command mark the next
symbol in the direction originally specified.

In the absence of a symbol and if a word is present at point,
this command will operate on it as described above.")

;;;###autoload
(defun prot-simple-mark-sexp-backward (&optional arg)
  "Mark previous or ARGth balanced expression[s].
Just a convenient backward-looking `mark-sexp'."
  (interactive "P")
  (if arg
      (mark-sexp (- arg) t)
    (mark-sexp (- 1) t)))

;;;###autoload
(defun prot-simple-mark-construct-dwim (&optional arg)
  "Mark symbol or balanced expression at point.
A do-what-I-mean wrapper for `prot-simple-mark-sexp-backward',
`mark-sexp', and `prot-simple-mark-symbol'.

When point is over a symbol, mark the entirety of it.  Regular
words are interpreted as symbols when an actual symbol is not
present.

For balanced expressions, a backward match will happen when point
is to the right of the closing delimiter.  A forward match is the
fallback condition and should work when point is before a
balanced expression, with or without whitespace in between it an
the opening delimiter.

Optional ARG will mark a total of ARGth objects while counting
the current one (so 3 would be 1+2 more).  A negative count moves
the mark backward (though that would invert the backward-moving
sexp matching of `prot-simple-mark-sexp-backward', so be mindful of
where the point is).  Repeated invocations of this command
incrementally mark objects in the direction originally
specified."
  (interactive "P")
  (cond
   ((symbol-at-point)
    (prot-simple-mark-symbol arg t))
   ((eq (point) (cdr (bounds-of-thing-at-point 'sexp)))
    (prot-simple-mark-sexp-backward arg))
   (t
    (mark-sexp arg t))))

;;;; Commands for code navigation (work in progress)

;;;###autoload
(defun prot-simple-downward-list (&optional arg)
  "Like `backward-up-list' but defaults to a forward motion.
With optional ARG, move that many times in the given
direction (negative is forward due to this being a
'backward'-facing command)."
  (interactive "P")
  (backward-up-list (or arg -1)))

;;;; Commands for paragraphs

(defvar-local prot-simple--auto-fill-cycle-state 1
  "Representation of `prot-simple-auto-fill-cycle' state.")

;; Based on gungadin-cylocal.el (private communication with Christopher
;; Dimech---disclosed with permission).
;;;###autoload
(defun prot-simple-auto-fill-cycle ()
  "Cycles auto fill for comments, everything, nothing."
  (interactive)
  (let ((n prot-simple--auto-fill-cycle-state))
    (pcase n
      (2
       (message "Auto fill %s" (propertize "buffer" 'face 'warning))
       (setq-local comment-auto-fill-only-comments nil)
       (setq-local prot-simple--auto-fill-cycle-state (1+ n)))
      (3
       (message "Disable auto fill")
       (auto-fill-mode 0)
       (setq-local prot-simple--auto-fill-cycle-state (1+ n)))
      (_
       (message "Auto fill %s" (propertize "comments" 'face 'success))
       (setq-local comment-auto-fill-only-comments t)
       (auto-fill-mode 1)
       (setq-local prot-simple--auto-fill-cycle-state 2)))))

;;;###autoload
(defun prot-simple-unfill-region-or-paragraph (&optional beg end)
  "Unfill paragraph or, when active, the region.
Join all lines in region delimited by BEG and END, if active,
while respecting any empty lines (so multiple paragraphs are not
joined, just unfilled).  If no region is active, operate on the
paragraph.  The idea is to produce the opposite effect of both
`fill-paragraph' and `fill-region'."
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (if (use-region-p)
        (fill-region beg end)
      (fill-paragraph))))

;;;; Commands for windows and pages

;;;###autoload
(defun prot-simple-narrow-visible-window ()
  "Narrow buffer to wisible window area.
Also check `prot-simple-narrow-dwim'."
  (interactive)
  (let* ((bounds (prot-common-window-bounds))
         (window-area (- (cadr bounds) (car bounds)))
         (buffer-area (- (point-max) (point-min))))
    (if (/= buffer-area window-area)
        (narrow-to-region (car bounds) (cadr bounds))
      (user-error "Buffer fits in the window; won't narrow"))))

;;;###autoload
(defun prot-simple-narrow-dwim ()
  "Do-what-I-mean narrowing.
If region is active, narrow the buffer to the region's
boundaries.

If pages are defined by virtue of `prot-common-page-p', narrow to
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
   ((prot-common-page-p)
    (narrow-to-page))
   ((null (buffer-narrowed-p))
    (prot-simple-narrow-visible-window))
   ((widen))))

(defun prot-simple--narrow-to-page (count &optional back)
  "Narrow to COUNTth page with optional BACK motion."
  (if back
      (narrow-to-page (or (- count) -1))
    (narrow-to-page (or (abs count) 1)))
  ;; Avoids the problem of skipping pages while cycling back and forth.
  (goto-char (point-min)))

;;;###autoload
(defun prot-simple-forward-page-dwim (&optional count)
  "Move to next or COUNTth page forward.
If buffer is narrowed to the page, keep the effect while
performing the motion.  Always move point to the beginning of the
narrowed page."
  (interactive "p")
  (if (buffer-narrowed-p)
      (prot-simple--narrow-to-page count)
    (forward-page count)
    (setq this-command 'forward-page)))

;;;###autoload
(defun prot-simple-backward-page-dwim (&optional count)
  "Move to previous or COUNTth page backward.
If buffer is narrowed to the page, keep the effect while
performing the motion.  Always move point to the beginning of the
narrowed page."
  (interactive "p")
  (if (buffer-narrowed-p)
      (prot-simple--narrow-to-page count t)
    (backward-page count)
    (setq this-command 'backward-page)))

;;;###autoload
(defun prot-simple-delete-page-delimiters (&optional beg end)
  "Delete lines with just page delimiters in the current buffer.
When region is active, only operate on the region between BEG and
END, representing the point and mark."
  (interactive "r")
  (let (b e)
    (if (use-region-p)
        (setq b beg
              e end)
      (setq b (point-min)
            e (point-max)))
  (widen)
  (flush-lines (format "%s$" page-delimiter) b e)
  (setq this-command 'flush-lines)))

;; Inspired by Pierre Neidhardt's windower:
;; https://gitlab.com/ambrevar/emacs-windower/-/blob/master/windower.el
(defvar prot-simple--windows-current nil
  "Current window configuration.")

;;;###autoload
(define-minor-mode prot-simple-monocle
  "Toggle between multiple windows and single window.
This is the equivalent of maximising a window.  Tiling window
managers such as DWM, BSPWM refer to this state as 'monocle'."
  :lighter " -M-"
  :global nil
  (let ((win prot-simple--windows-current))
    (if (one-window-p)
        (when win
          (set-window-configuration win))
      (setq prot-simple--windows-current (current-window-configuration))
      (delete-other-windows))))

(defun prot-simple--monocle-disable ()
  "Set variable `prot-simple-monocle' to nil, when appropriate.
To be hooked to `window-configuration-change-hook'."
  (when (and prot-simple-monocle (not (one-window-p)))
    (delete-other-windows)
    (prot-simple-monocle -1)
    (set-window-configuration prot-simple--windows-current)))

(add-hook 'window-configuration-change-hook #'prot-simple--monocle-disable)

;;;; Commands for buffers

;;;###autoload
(defun prot-simple-kill-buffer-current (&optional arg)
  "Kill current buffer or abort recursion when in minibuffer.
With optional prefix ARG (\\[universal-argument]) delete the
buffer's window as well."
  (interactive "P")
  (if (minibufferp)
      (abort-recursive-edit)
    (kill-buffer (current-buffer)))
  (when (and arg
             (not (one-window-p)))
    (delete-window)))

;;;###autoload
(defun prot-simple-rename-file-and-buffer (name)
  "Apply NAME to current file and rename its buffer.
Do not try to make a new directory or anything fancy."
  (interactive
   (list (read-string "Rename current file: " (buffer-file-name))))
  (let ((file (buffer-file-name)))
    (if (vc-registered file)
        (vc-rename-file file name)
      (rename-file file name))
    (set-visited-file-name name t t)))

(defun prot-simple--buffer-major-mode-prompt ()
  "Prompt of `prot-simple-buffers-major-mode'."
  (let ((major major-mode))
    (read-buffer
     (format "Buffer for %s: " major)
     nil t
     (lambda (pair) ; pair is (name-string . buffer-object)
       (with-current-buffer (cdr pair) (derived-mode-p major))))))

;;;###autoload
(defun prot-simple-buffers-major-mode (buffer)
  "Select BUFFER matching the current one's major mode."
  (interactive
   (list (prot-simple--buffer-major-mode-prompt)))
  (switch-to-buffer buffer))


(defun prot-simple--buffer-vc-root-prompt ()
  "Prompt of `prot-simple-buffers-vc-root'."
  (let ((root (or (vc-root-dir)
                   (locate-dominating-file "." ".git"))))
    (read-buffer
     (format "Buffers in %s: " root)
     nil t
     (lambda (pair) ; pair is (name-string . buffer-object)
       (with-current-buffer (cdr pair) (string-match-p root default-directory))))))

;;;###autoload
(defun prot-simple-buffers-vc-root (buffer)
  "Select BUFFER matching the current one's VC root."
  (interactive
   (list (prot-simple--buffer-vc-root-prompt)))
  (switch-to-buffer buffer))

(provide 'prot-simple)
;;; prot-simple.el ends here
