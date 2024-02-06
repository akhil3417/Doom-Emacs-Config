;;; lisp/auto-git-clone.el -*- lexical-binding: t; -*-


;; Misc git functions
(use-package! emacs
  :config
  ;; From http://xenodium.com/emacs-clone-git-repo-from-clipboard/
  ;; Auto-git-clone url in clipboard
  (defun my/git-clone-clipboard-url ()
    "Clone git URL in clipboard asynchronously and open in dired when finished."
    (interactive)
    (cl-assert (or (string-match-p "^git@github.com" (current-kill 0))
                   (string-match-p "^git@.*\\.org" (current-kill 0))
                   (string-match-p "^\\(http\\|https\\|ssh\\)://" (current-kill 0)))
               nil
               "No URL in clipboard")
    (let* ((url (current-kill 0))
           (download-dir (expand-file-name "~/gitclones/"))
           (project-dir (concat (file-name-as-directory download-dir)
                                (file-name-base url)))
           (default-directory download-dir)
           (command (format "git clone %s" url))
           (buffer (generate-new-buffer (format "*%s*" command)))
           (proc))
      (when (file-exists-p project-dir)
        (if (y-or-n-p (format "%s exists. delete?" (file-name-base url)))
            (delete-directory project-dir t)
          (user-error "Bailed")))
      (switch-to-buffer buffer)
      (setq proc (start-process-shell-command
                  (shell-quote-argument (nth 0 (split-string command)))
                  buffer command))
      (with-current-buffer buffer
        (setq default-directory download-dir)
        (shell-command-save-pos-or-erase)
        (require 'shell)
        (shell-mode)
        (view-mode +1))
      (set-process-sentinel proc (lambda (process state)
                                   (let ((output (with-current-buffer (process-buffer process)
                                                   (buffer-string))))
                                     (kill-buffer (process-buffer process))
                                     (if (= (process-exit-status process) 0)
                                         (progn
                                           (message "finished: %s" command)
                                           (dired project-dir))
                                       (user-error (format "%s\n%s" command output))))))
      (set-process-filter proc #'comint-output-filter))))
