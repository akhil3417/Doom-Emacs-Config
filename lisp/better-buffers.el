;; -*- lexical-binding: t -*-
;;######################################################################
;; * BETTER BUFFERS
;;######################################################################

;;----------------------------------------------------------------------
;; ** KEYBINDINGS
;;----------------------------------------------------------------------

;; (global-set-key (kbd "C-x k") 'my/kill-this-buffer)
(global-set-key (kbd "C-x k") 'my-kill-current-buffer)
;; (global-set-key (kbd "C-a")
;;                 (defun my/switch-to-other-buffer (&optional _arg)
;;                   (interactive)
;;                   (switch-to-buffer (other-buffer))))

(global-set-key (kbd "C-a")
(defun my-switch-to-previous-buffer ()
  "Switch to previously open buffer.
      Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1))))

;; Keys to traverse buffers
;; (global-set-key (kbd "<C-M-return>") 'ido-display-buffer)
(global-set-key (kbd "C-<prior>") 'previous-buffer) ; Ctrl+PageDown
(global-set-key (kbd "C-<next>") 'next-buffer) ; Ctrl+PageUp

;; (global-set-key (kbd "<C-tab>") 'other-window)  ; Ctrl+Tab
;; (global-set-key (kbd "<C-S-iso-lefttab>")
;;                 (lambda () (interactive) (other-window -1)))
                                        ;Ctrl+Shift+Tab

;;; Cycle buffers forward. (Backward with prefix arg)
;; (global-set-key (kbd "M-`")
;;                 (lambda (&optional arg)
;;                   (interactive "P")
;;                   (if arg (next-user-buffer) (previous-user-buffer))))

;;; Set keys to scroll buffer while centering
;;; location on screen.
(global-set-key "\M-]" 'scroll-buffer-down)
;; ;; (global-set-key "\M-[" 'scroll-buffer-up)


;;; Toggle window split between horizontal and vertical
(define-key ctl-x-4-map "t" 'toggle-window-split)
;; (define-key ctl-x-4-map "|" 'toggle-window-split)

(defun my/split-window-right (&optional size)
  "Split the selected window into two windows, one above the other.
The selected window is below.  The newly split-off window is
below and displays the same buffer.  Return the new window."
  (interactive "P")
  (split-window-right size)
  (other-window 1)
  (when (interactive-p)
    (if (featurep 'consult)
        (consult-buffer)
      (call-interactively #'switch-to-buffer))))

(defun my/split-window-below (&optional size)
  "Split the selected window into two side-by-side windows.
The selected window is on the left.  The newly split-off window
is on the right and displays the same buffer.  Return the new
window."
  (interactive "P")
  (split-window-below size)
  (other-window 1)
  (when (interactive-p)
    (if (featurep 'consult)
        (consult-buffer)
      (call-interactively #'switch-to-buffer))))

(defun my/delete-window-or-delete-frame (&optional window)
      "Delete WINDOW using `delete-window'.
If this is the sole window run `delete-frame' instead. WINDOW
must be a valid window and defaults to the selected one. Return
nil."
      (interactive)
      (condition-case nil
          (delete-window window)
        (error (if (and tab-bar-mode
                        (> (length (funcall tab-bar-tabs-function)) 1))
                   (tab-bar-close-tab)
                 (delete-frame)))))

(global-set-key [remap split-window-below] 'my/split-window-below) ;: C-x 2
(global-set-key [remap split-window-right] 'my/split-window-right);: C-x 3
(global-set-key [remap delete-window] 'my/delete-window-or-delete-frame) ;;C-x 0
;; (global-set-key (kbd "s-Q") 'my/delete-window-or-delete-frame)
;; (global-set-key (kbd "s-q") 'delete-other-windows);; SPC-w C-o
;; (global-set-key (kbd "s-z") 'my/split-window-below)
;; (global-set-key (kbd "s-v") 'my/split-window-right)
(global-set-key (kbd "s-o") ctl-x-4-map)
(global-set-key (kbd "s-d") ctl-x-5-map)
;; (global-set-key (kbd "H-k") 'my/kill-this-buffer) C-x k
;; (global-set-key (kbd "H-q") 'kill-buffer-and-window) C-x q
(global-set-key (kbd "<f7>") '+make-frame-floating-with-current-buffer)
;;----------------------------------------------------------------------
;; ** UTILITY FUNCTIONS
;;----------------------------------------------------------------------

;; (defun iswitchb-local-keys ()
;;   (mapc (lambda (K)
;;           (let* ((key (car K)) (fun (cdr K)))
;;             (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
;;         '(("C-n" . iswitchb-next-match)
;;           ("C-p"  . iswitchb-prev-match)
;;           ("<up>"    . ignore             )
;;           ("<down>"  . ignore             ))))

;; (add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

;;;###autoload
(defun my/kill-this-buffer (&optional arg)
  (interactive "P")
  (pcase arg
    ('4 (call-interactively #'kill-buffer))
    (_ (kill-buffer (current-buffer)))))



;;;###autoload
(defun toggle-window-split ()
  (interactive)
  "Toggles the window split between horizontal and vertical when
the fram has exactly two windows."
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

;; Swap windows if there are two of them
;;;###autoload
(defun swap-windows ()
 "If you have 2 windows, it swaps them." (interactive) (cond ((not (= (count-windows) 2)) (message "You need exactly 2 windows to do this."))
 (t
 (let* ((w1 (first (window-list)))
	 (w2 (second (window-list)))
	 (b1 (window-buffer w1))
	 (b2 (window-buffer w2))
	 (s1 (window-start w1))
	 (s2 (window-start w2)))
 (set-window-buffer w1 b2)
 (set-window-buffer w2 b1)
 (set-window-start w1 s2)
 (set-window-start w2 s1)))))

;; Skip to next/previous user buffer
;;;###autoload
(defun next-user-buffer ()
  "Switch to the next user buffer in cyclic order.\n
User buffers are those not starting with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    ;; (cond ((and (string-equal "*scratch*" (buffer-name))
    ;;             (buffer-modified-p))
    ;;        (setq i (1+ i))
    ;;        (next-buffer)))
    (while (and (not (and (string-equal "*scratch*" (buffer-name))
                          (buffer-modified-p)))
                (string-match "^*" (buffer-name))
                (< i 50))
      (setq i (1+ i)) (next-buffer) )))

;;;###autoload
(defun previous-user-buffer ()
  "Switch to the previous user buffer in cyclic order.\n
User buffers are those not starting with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (not (string-equal "*scratch*" (buffer-name)))
                (string-match "^*" (buffer-name))
                (< i 50))
      (setq i (1+ i)) (previous-buffer) )))

;; (global-set-key (kbd "s-n") 'next-buffer)
;; (global-set-key (kbd "s-p") 'previous-buffer)
;;; scroll-buffer: Functions to do exactly that.

;;;###autoload
(defun scroll-buffer-down (&optional arg)
  "Scroll buffer by (optional) ARG paragraphs."
  (interactive "p")
  (forward-paragraph arg)
  (recenter))

;;;###autoload
(defun scroll-buffer-up (&optional arg)
  "Scroll buffer by (optional) ARG paragraphs."
  (interactive "p")
  (backward-paragraph arg)
  (recenter))

;;;###autoload
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (if (vc-registered filename)
            (vc-rename-file name new-name)
          (rename-file name new-name 1))
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)))))

;;;###autoload
(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn (copy-file filename newname 1)
             (delete-file filename)
             (set-visited-file-name newname)
             (set-buffer-modified-p nil) t))))

;;;###autoload
(defun +make-frame-floating-with-current-buffer ()
  "Display the current buffer in a new floating frame.

This passes certain parameters to the newly created frame:

- use a different name than the default;
- use a graphical frame;
- do not display the minibuffer.

The name is meant to be used by the external rules of a tiling
window manager to present the frame in a floating state."
  (interactive)
  (let ((buf (current-buffer)))
    (if (not (one-window-p t))
        (delete-window))
    (make-frame '((name . "dropdown_emacs-buffer")
                  (window-system . x)
                  (minibuffer . nil)))
    (with-selected-frame (get-other-frame)
      (switch-to-buffer buf))))

(provide 'better-buffers)
;; better buffers.ui:1 ends here
