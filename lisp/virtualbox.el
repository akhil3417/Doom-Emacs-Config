;;; startup/virtualbox.el -*- lexical-binding: t; -*-

(defvar startup/virtualbox-process nil)
(defvar startup/virtualbox-workspace "VirtualBox")
(defvar startup/virtualbox-uuid "81145bf6-b18d-4d6e-b965-14fcfa440a65")
(defvar startup/virtualbox-executable (executable-find "VirtualBox"))
(defvar startup/virtualbox--timer nil)

(defun startup/start-virtualbox (&optional arg)
  (when startup/virtualbox-executable
    (setq startup/virtualbox-process
          (start-process-shell-command "virtualbox"
                                       " *startup/virtualbox*"
                                       (format "vboxmanage startvm --type gui %s"
                                               startup/virtualbox-uuid)))
    (add-hook 'exwm-manage-finish-hook #'startup/manage-virtualbox)
    (when arg (+workspace-switch startup/virtualbox-workspace t))))

(defun startup/kill-virtualbox (&optional arg)
  (interactive "p")
  (when (process-live-p startup/virtualbox-process)
    (shell-command-to-string (format "vboxmanage controlvm %s savestate"
                                     startup/virtualbox-uuid)))
  (when (and arg (+workspace-exists-p startup/virtualbox-workspace))
    (when (string= startup/virtualbox-workspace
                   (+workspace-current-name))
      (+workspace/other))
    (+workspace-delete startup/virtualbox-workspace)))

(defun startup/restart-virtualbox (&optional arg)
  (interactive "p")
  (startup/kill-virtualbox)
  (startup/start-virtualbox arg))

(defun startup/manage-virtualbox ()
  (when (and (stringp exwm-class-name)
             (string-match-p "VirtualBox" exwm-class-name))
    (hide-mode-line-mode +1)
    (unless (string= (+workspace-current-name) startup/virtualbox-workspace)
      (previous-buffer))
    (unless (+workspace-exists-p startup/virtualbox-workspace)
      (+workspace-new startup/virtualbox-workspace))))

(defun startup/select-virtualbox ()
  (interactive)
  (unless (process-live-p startup/virtualbox-process)
    (startup/start-virtualbox))
  (+workspace-switch startup/virtualbox-workspace t)
  (setq startup/virtualbox--timer
        (run-at-time 1 0.05
                     (lambda ()
                       (if (and (stringp exwm-class-name)
                                (string-match-p "VirtualBox" exwm-class-name))
                           (cancel-timer startup/virtualbox--timer)
                         (+workspace-switch-to-exwm-buffer-maybe)))))
  (advice-add #'+workspace-switch :before
              (defun tmp/cancel-discord-timer-a (&rest _)
                (cancel-timer startup/virtualbox--timer)
                (advice-remove #'+workspace-switch #'tmp/cancel-discord-timer-a))))

(when (modulep! :completion vertico)
  ;; Do not grab input from Consult.
  (advice-add #'consult--buffer-preview :around
              (defun +consult--buffer-preview-a (oldfun &rest args)
                (-orfn (lambda (action cand)
                         (and cand
                              (string-match-p " \\[Running\\] - Oracle VM VirtualBox"
                                              cand)
                              t))
                       (apply oldfun args)))))

(map! :leader
      :prefix +startup-prefix
      (:prefix ("v" . "VirtualBox")
       :desc "Restart VirtualBox" "r" #'startup/restart-virtualbox
       :desc "Select VirtualBox" "s" #'startup/select-virtualbox
       :desc "Kill VirtualBox" "x" #'startup/kill-virtualbox))

;; (if (process-live-p startup/virtualbox-process)
;;     (startup/restart-virtualbox)
;;   (startup/start-virtualbox))