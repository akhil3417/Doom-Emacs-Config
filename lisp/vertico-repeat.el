;;; vertico-repeat.el --- Repeat Vertico sessions -*- lexical-binding: t -*-

;; Copyright (C) 2021, 2022  Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2021
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (vertico "0.21"))
;; Homepage: https://github.com/minad/vertico

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is a Vertico extension, which enables repetition of the
;; Vertico sessions via the `vertico-repeat-last' and
;; `vertico-repeat-select' commands. It is necessary to register a
;; minibuffer setup hook, which saves the Vertico state for repetition.
;;
;; (global-set-key "\M-r" #'vertico-repeat-last)
;; (global-set-key "\M-R" #'vertico-repeat-select)
;; (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

;;; Code:

(require 'vertico)
(eval-when-compile (require 'cl-lib))

(defcustom vertico-repeat-filter
  '(vertico-repeat-select
    execute-extended-command
    execute-extended-command-for-buffer)
  "List of commands to filter out from the history."
  :type '(repeat symbol)
  :group 'vertico)

(defvar vertico-repeat--history nil)
(defvar-local vertico-repeat--command nil)
(defvar-local vertico-repeat--input nil)

(defun vertico-repeat--save-input ()
  "Save current minibuffer input."
  (setq vertico-repeat--input (minibuffer-contents)))

(defun vertico-repeat--save-exit ()
  "Save command session in `vertico-repeat--history'."
  (add-to-history
   'vertico-repeat--history
   (list
    vertico-repeat--command
    vertico-repeat--input
    (and vertico--lock-candidate
         (>= vertico--index 0)
         (nth vertico--index vertico--candidates)))))

(defun vertico-repeat--restore (session)
  "Restore Vertico SESSION for `vertico-repeat'."
  (delete-minibuffer-contents)
  (insert (cadr session))
  (when (caddr session)
    (vertico--exhibit)
    (when-let (idx (seq-position vertico--candidates (caddr session)))
      (setq vertico--index idx
            vertico--lock-candidate t)
      (vertico--exhibit))))

;;;###autoload
(defun vertico-repeat-save ()
  "Save Vertico session for `vertico-repeat'.
This function must be registered as `minibuffer-setup-hook'."
  (when (and vertico--input (not (memq this-command vertico-repeat-filter)))
    (setq vertico-repeat--command this-command)
    (add-hook 'post-command-hook #'vertico-repeat--save-input nil 'local)
    (add-hook 'minibuffer-exit-hook #'vertico-repeat--save-exit nil 'local)))

;;;###autoload
(defun vertico-repeat-last (&optional session)
  "Repeat last Vertico completion SESSION."
  (interactive
   (list (or (car vertico-repeat--history)
             (user-error "No repeatable Vertico session"))))
  (minibuffer-with-setup-hook
      (apply-partially #'vertico-repeat--restore session)
    (command-execute (setq this-command (car session)))))

(defun vertico-repeat-select ()
  "Select a session from the last Vertico sessions and repeat it."
  (interactive)
  (let* ((trimmed
          (delete-dups
           (or
            (cl-loop
             for session in vertico-repeat--history collect
             (list
              (symbol-name (car session))
              (replace-regexp-in-string
               "\\s-+" " "
               (string-trim (cadr session)))
              (if (caddr session)
                  (replace-regexp-in-string
                   "\\s-+" " "
                   (string-trim (caddr session)))
                "")
              session))
            (user-error "No repeatable Vertico session"))))
         (max-cmd (cl-loop for (cmd . _) in trimmed
                           maximize (string-width cmd)))
         (max-input (cl-loop for (_cmd input . _) in trimmed
                             maximize (string-width input)))
         (formatted (cl-loop
                     for (cmd input cand session) in trimmed collect
                     (cons
                      (concat
                       (propertize cmd 'face 'font-lock-function-name-face)
                       (make-string (- max-cmd (string-width cmd) -4) ?\s)
                       (propertize input 'face 'font-lock-string-face)
                       (make-string (- max-input (string-width input) -4) ?\s)
                       (and cand (propertize cand 'face 'font-lock-comment-face)))
                      session)))
         (selected (or (cdr (assoc (completing-read
                                    "History: "
                                    (lambda (str pred action)
                                      (if (eq action 'metadata)
                                          '(metadata (display-sort-function . identity)
                                                     (cycle-sort-function . identity))
                                        (complete-with-action action formatted str pred)))
                                    nil t nil t)
                                   formatted))
                       (user-error "No session selected"))))
    (vertico-repeat-last selected)))

(provide 'vertico-repeat)
;;; vertico-repeat.el ends here
