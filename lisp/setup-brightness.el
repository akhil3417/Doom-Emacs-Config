;;; lisp/setup-brightness.el -*- lexical-binding: t; -*-

(setq my/brightness-min 1)
(setq my/brightness-max 100)
(setq my/brightness-step 5)

(defun my/get-brightness ()
  (* my/brightness-step (round (string-to-number
                                (shell-command-to-string "xbacklight -get"))
                               my/brightness-step)))

(defun my/set-brightness (level)
  (interactive "nBrightness level: ")
  (let ((safe-level
         (cond ((< level my/brightness-min) my/brightness-min)
               ((> level my/brightness-max) my/brightness-max)
               (t level))))
    (save-window-excursion
      (shell-command
       (format "xbacklight -set %s &" safe-level) nil nil))))

(defun my/brightness-step-change (delta)
  (my/set-brightness (+ delta (my/get-brightness))))

(defun my/brightness-increase ()
  (interactive)
  (my/brightness-step-change my/brightness-step))

(defun my/brightness-decrease ()
  (interactive)
  (my/brightness-step-change (- my/brightness-step)))

;; (map! "<XF86MonBrightnessDown>" 'my/brightness-decrease)
;; (map! "<XF86MonBrightnessUp>" 'my/brightness-increase)


;; (defun my/set-brightness-lg-5k (level)
;;   (interactive "nBrightness level: ")
;;   (save-window-excursion
;;     (shell-command
;;      (format "echo \"0i%s\n\" | sudo /repos/LG-ultrafine-brightness/build/LG_ultrafine_brightness
