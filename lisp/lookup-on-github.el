;;; lisp/lookup-on-github.el -*- lexical-binding: t; -*-

(defconst +lookup-github-programming-languages
  '("Emacs Lisp"
    "Nix"
    "Python"
    "Haskell"
    "Shell"
    "JavaScript"))

(defun +lookup--online-backend-github-code-search (query)
  (funcall +lookup-open-url-fn
           (let* ((name "GitHub code search")
                  (language (cl-case major-mode
                              ((emacs-lisp-mode inferior-emacs-lisp-mode)
                               "Emacs Lisp")
                              (nix-mode "Nix")
                              (javascript-mode "JavaScript")
                              (python-mode "Python")
                              (haskell-mode "Haskell")
                              (sh-mode "Shell")
                              ;; more here ...
                              (t (completing-read
                                  (concat name " language: ")
                                  ;; List of programming languages
                                  +lookup-github-programming-languages))))
                  (symbol (completing-read
                           (concat name ": ")
                           (pcase language
                             ("Emacs Lisp" obarray)
                             (_ nil))
                           nil
                           nil
                           query)))
             (cond (language (concat "https://github.com/search?l="
                                     (replace-regexp-in-string "[[:space:]]+" "+" language)
                                     "&q=" symbol "&type=Code"))
                   (t (format "https://github.com/search?l=%s&type=Code&q="
                              symbol)))))
  ;; Do not ask for further input.
  "")

(defun +lookup--online-backend-github-repo-search (query)
  (funcall +lookup-open-url-fn
           (let* ((name "GitHub repository search")
                  (language (completing-read (concat name " language: ")
                                             ;; List of programming languages
                                             +lookup-github-programming-languages))
                  (symbol (completing-read
                           (concat name ": ")
                           nil
                           nil
                           nil
                           query)))
             (cond (language (concat "https://github.com/search?l="
                                     (replace-regexp-in-string "[[:space:]]+" "+" language)
                                     "&q=" symbol "&type=repositories"))
                   (t (format "https://github.com/search?l=%s&type=repositories&q="
                              symbol)))))
  ;; Do not ask for further input
  "")

(when (boundp '+lookup-provider-url-alist)
  (add-to-list '+lookup-provider-url-alist
               (list "GitHub code search" #'+lookup--online-backend-github-code-search))
  (add-to-list '+lookup-provider-url-alist
               (list "GitHub repository search" #'+lookup--online-backend-github-repo-search)))

(provide 'lookup-on-github)
