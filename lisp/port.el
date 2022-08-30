;;; startup/port.el -*- lexical-binding: t; -*-

(defvar startup/port-process nil)
(defvar startup/port-executable (executable-find "port"))
(defvar startup/port-workspace "Port")

(defun startup/start-port (&optional arg)
  (interactive "p")
  (when startup/port-executable
    (setq startup/port-process
          (start-process "port"
                         " *startup/port*"
                         startup/port-executable
                         (if (eq (user-uid) 0) "--no-sandbox" "")))
    (add-hook 'exwm-manage-finish-hook #'startup/manage-port)
    (when arg (+workspace-switch startup/port-workspace))))

(defun startup/kill-port (&optional arg)
  (interactive "p")
  (when (process-live-p startup/port-process)
    (kill-process startup/port-process))
  (when (and arg (+workspace-exists-p startup/port-workspace))
    (when (string= startup/signal-workspace
                   (+workspace-current-name))
      (+workspace/other))
    (+workspace-delete startup/port-workspace)))

(defun startup/restart-port (&optional arg)
  (interactive "p")
  (startup/kill-port)
  (startup/start-port arg))

(defun startup/manage-port ()
  (when (and (stringp exwm-class-name)
             (string-match-p "port" exwm-class-name))
    (unless (+workspace-exists-p startup/port-workspace)
      (+workspace-new startup/port-workspace)))
  (when (persp-get-by-name startup/port-workspace)
    (persp-add-buffer
     (cl-remove-if-not
      (lambda (buf)
        (string= "Port" (buffer-local-value 'exwm-class-name buf)))
      (buffer-list))
     (persp-get-by-name startup/port-workspace))))

(defun startup/select-port ()
  (interactive)
  (unless (process-live-p startup/port-process)
    (startup/restart-port))
  (+workspace-switch startup/port-workspace t)
  (+workspace-switch-to-exwm-buffer-maybe))

(map! :leader
      :prefix +startup-prefix
      (:prefix ("p" . "Port")
       :desc "Restart Port" "r" #'startup/restart-port
       :desc "Select Port" "s" #'startup/select-port
       :desc "Kill Port" "x" #'startup/kill-port))

;; (if (process-live-p startup/port-process)
;;     (startup/restart-port)
;;   (startup/start-port))