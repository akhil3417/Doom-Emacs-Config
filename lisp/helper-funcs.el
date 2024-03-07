;;; lisp/new/helper-funcs.el -*- lexical-binding: t; -*-

(defun buffer-to-side-window (place)
  "Place the current buffer in the side window at PLACE."
  (interactive (list (intern
                      (completing-read "Which side: "
                                       '(top left right bottom)))))
  (let ((buf (current-buffer)))
    (display-buffer-in-side-window
     buf `((window-height . 0.15)
           (side . ,place)
           (slot . -1)
           (window-parameters . ((no-delete-other-windows . t)
                                 (no-other-window t)))))
    (delete-window)))


(defun create-scratch-buffer nil
  "create a new scratch buffer to work in. (could be *scratch* - *scratchX*)"
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (emacs-lisp-mode)
    ))

(defun split-window-right-and-move-there-dammit ()
  (interactive)
  (split-window-right)
  (windmove-right))


(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))


(defun kill-dired-buffers ()
  "Kill all open dired buffers."
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

(defun my/copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;; [[file:config.org::*open current file with external program][open current file with external program:1]]
(defun my/open-with (arg)
  "Open visited file in default external program.

      With a prefix ARG always prompt for command to use."
  (interactive "P")
  (when buffer-file-name
    (shell-command (concat
                    (cond
                     ((and (not arg) (eq system-type 'darwin)) "open")
                     ((and (not arg) (member system-type '(gnu gnu/linux gnu/kfreebsd))) "xdg-open")
                     (t (read-shell-command "Open current file with: ")))
                    " "
                    (shell-quote-argument buffer-file-name)))))

;; [[file:config.org::*Helper function to measure the running time of a function][Helper function to measure the running time of a function:1]]
(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(defun generate-password-non-interactive ()
  (string-trim (shell-command-to-string "pwgen -A 24")))

(defun generate-password ()
  "Generates and inserts a new password"
  (interactive)
  (insert
   (shell-command-to-string
    (concat "pwgen -A " (read-string "Length: " "24") " 1"))))

(defun save-macro (name)
  "save a macro. Take a name as argument
     and save the last defined macro under
     this name at the end of your .emacs"
  (interactive "SName of the macro: ")  ; ask for the name of the macro
  (kmacro-name-last-macro name)         ; use this name for the macro
  (find-file custom-file)            ; open ~/.emacs or other user init file
  (goto-char (point-max))               ; go to the end of the .emacs
  (newline)                             ; insert a newline
  (insert-kbd-macro name)               ; copy the macro
  (newline)                             ; insert a newline
  (save-buffer)
  (switch-to-buffer nil))               ; return to the initial buffer

(defvar counsel-network-manager-history nil
  "Network manager history.")

(defun counsel-network-manager (&optional initial-input)
  "Connect to wifi network."
  (interactive)
  (shell-command "nmcli device wifi rescan")
  (let ((networks-list (s-split "\n" (shell-command-to-string "nmcli device wifi list"))))
    (ivy-read "Select network" networks-list
              :initial-input initial-input
              :require-match t
              :history counsel-network-manager-history
              :sort nil
              :caller 'counsel-network-manager
              :action (lambda (line)
                        (let ((network (car (s-split " " (s-trim (s-chop-prefix "*" line)) t))))
                          (message "Connecting to \"%s\".." network)
                          (async-shell-command
                           (format "nmcli device wifi connect %s" (shell-quote-argument network))))))))
;; Network Manager:1 ends here

(defun my/launch-firefox-private (&optional arg)
  "Launch Firefox.
  With `\\[universal-argument]' prefix argument ARG, create private
  window."
  (interactive "P")
  (make-process
   :name "firefox"
   :command `("firefox" ,(if arg "--private-window" "--new-window"))))

;; [[file:config.org::*Google Translate][Google Translate:2]]
;; (use-package! google-translate
;;   :demand t
;;   :init
;;   (require 'google-translate)
;;   :functions (my-google-translate-at-point google-translate--search-tkk)
;;   :custom
;;   (google-translate-backend-method 'curl)
;;   :config
;;   (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))
;;   (defun my-google-translate-at-point()
;;     "reverse translate if prefix"
;;     (interactive)
;;     (if current-prefix-arg
;;         (google-translate-at-point)
;;       (google-translate-at-point-reverse)))
;;   :bind
;;   ("C-c t". my-google-translate-at-point))
;; (use-package! emacs
;;   :config
;;   (defvar google-search-history nil
;;     "List of queries to google-search-string.")
;;   (defun google-search-string (search-string)
;;     "Read SEARCH-STRING from the minibuffer and call the shell
;; command tuxi on it."
;;     (interactive (list (read-string "Google: " nil
;;                                     google-search-history
;;                                     (thing-at-point 'sexp))))
;;     (unless (executable-find "ls");; m dumb,
;;       (user-error "Cannot find shell command: tuxipy"))
;;     (let ((search-output (string-trim-right
;;                           (shell-command-to-string
;;                            (concat
;;                             "python3 -m tuxipy "
;;                             (shell-quote-argument search-string))))))
;;       (with-current-buffer (get-buffer-create "*Tuxi Output*")
;;         (goto-char (point-max))
;;         (unless (bobp) (insert "\n\n* * *\n"))
;;         (insert (capitalize search-string) ":\n\n")
;;         (push-mark)
;;         (insert search-output)
;;         (let ((lines (count-lines (or (mark) (point-min)) (point-max))))
;;           (if (<= lines 1)
;;               (message search-output)
;;             (let ((win (display-buffer (current-buffer))))
;;               (set-window-start win (mark))
;;               (set-window-parameter win 'window-height (min lines 10))
;;               (goto-address-mode 1)))))))
;;   (defun google-search-at-point (&optional beg end)
;;     "Call the shell command tuxi on the symbol at point. With an
;; active region use it instead."
;;     (interactive "r")
;;     (if-let ((search-string (if (use-region-p)
;;                                 (buffer-substring-no-properties beg end)
;;                               (thing-at-point 'symbol))))
;;         (google-search-string search-string)
;;       ;; (message "No symbol to search for at point!")
;;       (call-interactively #'google-search-string)))
;;   :bind (:map help-map
;;               ("g" . google-search-string)
;;               ("C-=" . google-search-at-point)))
;; [[file:config.org::*simple httpd][simple httpd:1]]

;; [[file:config.org::*CALENDAR][CALENDAR:1]]
;; https://stackoverflow.com/questions/9547912/emacs-calendar-show-more-than-3-months
(defun dt/year-calendar (&optional year)
  (interactive)
  (require 'calendar)
  (let* (
         (current-year (number-to-string (nth 5 (decode-time (current-time)))))
         (month 0)
         (year (if year year (string-to-number (format-time-string "%Y" (current-time))))))
    (switch-to-buffer (get-buffer-create calendar-buffer))
    (when (not (eq major-mode 'calendar-mode))
      (calendar-mode))
    (setq displayed-month month)
    (setq displayed-year year)
    (setq buffer-read-only nil)
    (erase-buffer)
    ;; horizontal rows
    (dotimes (j 4)
      ;; vertical columns
      (dotimes (i 3)
        (calendar-generate-month
         (setq month (+ month 1))
         year
         ;; indentation / spacing between months
         (+ 5 (* 25 i))))
      (goto-char (point-max))
      (insert (make-string (- 10 (count-lines (point-min) (point-max))) ?\n))
      (widen)
      (goto-char (point-max))
      (narrow-to-region (point-max) (point-max)))
    (widen)
    (goto-char (point-min))
    (setq buffer-read-only t)))

(defun dt/scroll-year-calendar-forward (&optional arg event)
  "Scroll the yearly calendar by year in a forward direction."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (unless arg (setq arg 0))
  (save-selected-window
    (if (setq event (event-start event)) (select-window (posn-window event)))
    (unless (zerop arg)
      (let* (
             (year (+ displayed-year arg)))
        (dt/year-calendar year)))
    (goto-char (point-min))
    (run-hooks 'calendar-move-hook)))

(defun dt/scroll-year-calendar-backward (&optional arg event)
  "Scroll the yearly calendar by year in a backward direction."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     last-nonmenu-event))
  (dt/scroll-year-calendar-forward (- (or arg 1)) event))

(map! :leader
      :desc "Scroll year calendar backward" "<left>" #'dt/scroll-year-calendar-backward
      :desc "Scroll year calendar forward" "<right>" #'dt/scroll-year-calendar-forward)

(defalias 'year-calendar 'dt/year-calendar)
;; CALENDAR:1 ends here

(defun my-kill-current-buffer ()
  (interactive)
  (catch 'quit
    (save-window-excursion
      (let (done)
        (when (and buffer-file-name (buffer-modified-p))
          (while (not done)
            (let ((response (read-char-choice
                             (format "Save file %s? (y, n, d, q) " (buffer-file-name))
                             '(?y ?n ?d ?q))))
              (setq done (cond
                          ((eq response ?q) (throw 'quit nil))
                          ((eq response ?y) (save-buffer) t)
                          ((eq response ?n) (set-buffer-modified-p nil) t)
                          ((eq response ?d) (diff-buffer-with-file) nil))))))
        (kill-buffer (current-buffer))))))

(after! general
  (map! [remap kill-current-buffer] #'my-kill-current-buffer))

(defun ak/mpc-invidious-grabber (arg)
  (interactive "P")
  (let* ((query (replace-regexp-in-string " " "+" (read-string "Enter query: ")))
         (url (shell-command-to-string (format "curl -s \"https://vid.puffyan.us/search?q=%s\" | grep -Eo \"watch\\?v=.{11}\" | head -n 1 | xargs -I {} echo \"https://youtube.com/{}\"" query))))
    (if arg
        (start-process-shell-command "yt" nil (format "mpv --no-video %s" url))
      (start-process-shell-command "yt" nil (format "mpv %s" url))))
  (message "Streaming started."))

;; tts
(defun tts-piper (&optional arg)
  "Send the text after point or the given TEXT to piper for tts.
If a region is active, send the marked text. If TEXT is provided, that text is used.
If a non-numeric prefix argument is provided, prompt for text input.
If a numeric prefix argument is provided, send the number of lines.
also filter the special chars that break the tts"
  (interactive "P")
  (let* ((text (cond
                ((region-active-p) (buffer-substring-no-properties (region-beginning) (region-end)))
                ((consp arg) (read-string "Enter text: "))
                (arg (buffer-substring-no-properties (point) (save-excursion (forward-line arg) (point))))
                (t (buffer-substring-no-properties (point) (point-max)))))
         (escaped-text (replace-regexp-in-string "\\([a-z]\\)'\\([a-z]\\)" "\\1 \\2" text)))
    (start-process "piper" "*piper*" "sh" "-c"
                   (format "echo '%s' | ~/myrepos/linux-assistant/extensions/piper/piper --model ~/myrepos/linux-assistant/extensions/piper/models/en_US-hfc_female-medium.onnx  --output-raw 2>/dev/null | aplay -r 22050 -f S16_LE -t raw - 2>/dev/null" escaped-text))))


(defun jarvis (text)
  "Send the given TEXT to spgt and pipe the response to the piper for tts."
  (interactive "sEnter text: ")
  (let ((cleaned-text (replace-regexp-in-string "[\"\'()]" "\\\\\\&" text)))
    (start-process "piper" "*piper*" "sh" "-c"
                   (format "sgpt --top-p '0.01' --temperature '0.32' --no-cache --chat jarvis  '%s' | tee /dev/tty | ~/myrepos/linux-assistant/extensions/piper/piper --model ~/myrepos/linux-assistant/extensions/piper/models/en_US-hfc_female-medium.onnx  --output-raw 2>/dev/null | aplay -r 22050 -f S16_LE -t raw - 2>/dev/null" text))))
