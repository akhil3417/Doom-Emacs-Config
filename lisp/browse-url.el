;;; lisp/browse-url.el -*- lexical-binding: t; -*-


;;;----------------------------------------------------------------
;; ** BROWSE-URL
;;;----------------------------------------------------------------
(use-package! browse-url
  :commands (browse-url-at-point-umpv browse-url-umpv)
  :config
  (when IS-LINUX
    (defun browse-url-umpv (url &optional single)
      (start-process "mpv" nil (if single "mpv" "mpv")
                     (shell-quote-wildcard-pattern url)))

    (defun browse-url-mpv (url)
      (browse-url-umpv url t))

    (defun browse-url-at-point-umpv (&optional single)
      "Open link in mpv"
      (interactive "P")
      (let ((browse-url-browser-function
             (if single
                 (lambda (url &optional _new-window) (browse-url-umpv url t))
               #'browse-url-umpv)))
        (browse-url-at-point)))

    ))


(defun +open-link-with-mpv ()
  "Open the link under the point using mpv."
  (interactive)
  (save-excursion
    (let (url)
      (when (or (thing-at-point-looking-at "\\(https?\\):\\/\\/[^[:space:]]+")
                (thing-at-point-looking-at "\\(ftp\\):\\/\\/[^[:space:]]+"))
        (setq url (match-string-no-properties 0)))
      (when url
        (start-process "mpv" nil "mpv" url)))))
