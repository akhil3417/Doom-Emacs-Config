;;; lisp/exwm-paste.el -*- lexical-binding: t; -*-

(defun paste/discord ()
  ;; Surround by three backticks and newlines.
  (interactive)
  (kill-new (concat "```\n"
                    (car kill-ring)
                    "```\n"))
  (exwm-input--fake-key ?\C-v))

(defun paste/teams ()
  ;; Type three backticks and a space, then insert with indentation levels
  ;; adjusted.
  (interactive)
  (+exwm-send-string
   "``` "
   (let* ((lines (split-string (s-trim-right (car kill-ring)) "\n"))
          (indentation
           (cl-loop for line in lines
                    minimize
                    (if (string-match "^[\s\t]+" line)
                        (length (match-string-no-properties 0 line))
                      0))))
     (kill-new (mapconcat (lambda (s)
                            (substring-no-properties s indentation))
                          lines
                          "\n")))
   (run-at-time 0.2 nil #'exwm-input--fake-key ?\C-v)
   (run-at-time 0.27 nil #'exwm-input--fake-key 'return)
   (run-at-time 0.34 nil #'exwm-input--fake-key 'return)))

(defun paste/reddit ()
  ;; Add 4 spaces of at the beginning of each sentence.
  (interactive)
  (kill-new (replace-regexp-in-string "^" "    " (car kill-ring)))
  (exwm-input--fake-key ?\C-v))

(defun +exwm-paste ()
  (interactive)
  (cond ((string-prefix-p "Microsoft Teams" exwm-class-name) (paste/teams))
        ((string= exwm-class-name "discord") (paste/discord))
        ((string-match-p "reddit.com" exwm-title) (paste/reddit))
        ((t (exwm-input--fake-key ?\C-v)))))

(map! :map exwm-mode-map
      :localleader
      "p" #'+exwm-paste)

(provide 'exwm-paste)
