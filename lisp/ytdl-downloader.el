;;; ytdl-downloader.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Akhil Pratap Singh
;;
;; Author: Akhil Pratap Singh <akhilpratapsingh3417@gmail.com>
;; Maintainer: Akhil Pratap Singh <akhilpratapsingh3417@gmail.com>
;; Created: November 22, 2022
;; Modified: November 22, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/akhil3417/
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defun ytdl-downloader (&optional link)
  (interactive)
  (let ((link (or link (current-kill 0)))
        (buffer (generate-new-buffer "*ytd-formats*")))
    (make-process :name "ytd-formats"
                  :buffer buffer
                  :command (list "youtube-dl" "--list-formats" link)
                  :connection-type 'pipe
                  :sentinel `(lambda (p e)
                               (set-buffer ',buffer)
                               (goto-char (point-min))
                               (unless (search-forward "format code" nil t)
                                 (kill-buffer)
                                 (error "url not supported"))
                               (forward-line 1)
                               (let (list)
                                 (while (not (eobp))
                                   (setq list (cons
                                               (split-string
                                                (buffer-substring-no-properties
                                                 (point)
                                                 (point-at-eol)) "\n" t nil)
                                               list))
                                   (forward-line 1))
                                 (setq list (nreverse list))
                                 (kill-buffer "*ytd-formats*")
                                 (ivy-read "youtube-dl formats (vid+aud): " list
                                           :action (lambda (x)
                                                     (youtube-dl
                                                      (substring-no-properties
                                                       (format "%s" x)
                                                       (if (string-match "(" (format "%s" x))
                                                           (match-end 0)
                                                         nil)
                                                       (string-match "[[:space:]]" (format "%s" x))) ',link))
                                           :sort nil
                                           :history 'youtube-dl
                                           :re-builder #'regexp-quote
                                           :preselect "best"))))))
(defun youtube-dl (fmt link)
  (let ((buffer (generate-new-buffer "*youtube-dl*")))
    (with-current-buffer buffer
      (ansi-color-for-comint-mode-on)
      (comint-mode))
    (make-process :name "youtube-dl"
                  :buffer buffer
                  :command (list
                            "youtube-dl"
                            "--flat-playlist"
                            "--format" fmt link)
                  :connection-type 'pty
                  :filter 'comint-output-filter
                  :sentinel (lambda (p e)
                              (make-process :name "notify"
                                            :connection-type 'pipe
                                            :command (list
                                                      "notify-send"
                                                      (format "%s %s" p e)
                                                      "download complete"))
                              (message
                               "Process %s %s" p (replace-regexp-in-string "\n\\'" "" e))))))

(provide 'ytdl-downloader)
;;; ytdl-downloader.el ends here
