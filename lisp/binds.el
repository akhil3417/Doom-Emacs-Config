;;; lisp/new/binds.el -*- lexical-binding: t; -*-

;; Visually selects the paragraph. Execute multiple times to expand the selection or move the cursor:
(map! :leader :desc "Visually mark paragraph" "v p" 'er/mark-paragraph)
(global-set-key (kbd "C-@") 'er/expand-region)

;; (evil-global-set-key 'motion "j" 'evil-next-visual-line)
;; (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

(global-set-key (kbd "C-?") #'execute-extended-command)

(global-set-key (kbd "<C-escape>") 'consult-buffer)
(setq display-line-numbers-type nil)
(map! :leader
      :desc "Comment or uncomment lines" "TAB TAB" #'comment-line
      (:prefix ("t" . "toggle")
       :desc "Toggle line numbers" "l" #'doom/toggle-line-numbers
       :desc "Toggle line highlight in frame" "h" #'hl-line-mode
       :desc "Toggle line highlight globally" "H" #'global-hl-line-mode
       :desc "Toggle truncate lines" "t" #'toggle-truncate-lines))

(setq dirvish-open-with-programs '((("ape" "stm" "s3m" "ra" "rm" "ram" "wma" "wax" "m3u" "med" "669" "mtm" "m15" "uni" "ult" "mka" "flac" "axa" "kar" "midi" "mid" "s1m" "smp" "smp3" "rip" "multitrack" "ecelp9600" "ecelp7470" "ecelp4800" "vbk" "pya" "lvp" "plj" "dtshd" "dts" "mlp" "eol" "uvva" "uva" "koz" "xhe" "loas" "sofa" "smv" "qcp" "psid" "sid" "spx" "opus" "ogg" "oga" "mp1" "mpga" "m4a" "mxmf" "mhas" "l16" "lbc" "evw" "enw" "evb" "evc" "dls" "omg" "aa3" "at3" "atx" "aal" "acn" "awb" "amr" "ac3" "ass" "aac" "adts" "726" "abs" "aif" "aifc" "aiff" "au" "mp2" "mp3" "mp2a" "mpa" "mpa2" "mpega" "snd" "vox" "wav") "/usr/bin/mpv" "--profile=builtin-pseudo-gui" "%f") (("f4v" "rmvb" "wvx" "wmx" "wmv" "wm" "asx" "mk3d" "mkv" "fxm" "flv" "axv" "webm" "viv" "yt" "s1q" "smo" "smov" "ssw" "sswf" "s14" "s11" "smpg" "smk" "bk2" "bik" "nim" "pyv" "m4u" "mxu" "fvt" "dvb" "uvvv" "uvv" "uvvs" "uvs" "uvvp" "uvp" "uvvu" "uvu" "uvvm" "uvm" "uvvh" "uvh" "ogv" "m2v" "m1v" "m4v" "mpg4" "mp4" "mjp2" "mj2" "m4s" "3gpp2" "3g2" "3gpp" "3gp" "avi" "mov" "ts" "movie" "mpe" "mpeg" "mpegv" "mpg" "mpv" "qt" "vbs") "/usr/bin/mpv" "%f")(("pdf" "epub")"sioyek" "%f")))
(setq! dirvish-quick-access-entries
       `(("h" "~/"                          "Home")
         ("e" ,user-emacs-directory         "Emacs user directory")
         ("c" "~/myrepos/"                     "My Repos")
         ("g" "~/gitclones/"                     "Git Clones")
         ("d" "~/Downloads/"                "Downloads")
         ("b" "~/Documents/books & pdf/"      "Books")
         ("w" "/media/New_Volume/Edu/Web Series/"      "Web Series")
         ("D" "~/Documents/"                "Documents")
         ("H" "/media/New_Volume/"                "Harddiskntfs")
         ("Z" "/media/hdd_home/"                "Harddiskhome")
         ("m" "/mnt/"                       "Mounted drives")
         ("t" "~/.local/share/Trash/files/" "Trash")))

(map! :leader
      :desc "Switch to perspective NAME" "DEL" #'persp-switch
      :desc "Switch to buffer in perspective" "," #'persp-switch-to-buffer
      :desc "Switch to next perspective" "]" #'persp-next
      :desc "Switch to previous perspective" "[" #'persp-prev
      :desc "Add a buffer current perspective" "+" #'persp-add-buffer
      :desc "Remove perspective by name" "-" #'persp-remove-by-name)
;; [[file:config.org::*Copy filename to clipboard][Copy filename to clipboard:1]]
;; Copy filename to clipboard:1 ends here

;; open current file with external program:1 ends here


;; [[file:config.org::*CLIPPY][CLIPPY:1]]
(map! :leader
      (:prefix ("c h" . "Help info from Clippy")
       :desc "Clippy describes function under point" "f" #'clippy-describe-function
       :desc "Clippy describes variable under point" "v" #'clippy-describe-variable))
;; CLIPPY:1 ends here
;;
;;
(use-package! lexic
  :commands lexic-search lexic-list-dictionary
  :config
  (map! :map lexic-mode-map
        :n "q" #'lexic-return-from-lexic
        :nv "RET" #'lexic-search-word-at-point
        :n "a" #'outline-show-all
        :n "h" (cmd! (outline-hide-sublevels 3))
        :n "o" #'lexic-toggle-entry
        :n "n" #'lexic-next-entry
        :n "N" (cmd! (lexic-next-entry t))
        :n "p" #'lexic-previous-entry
        :n "P" (cmd! (lexic-previous-entry t))
        :n "E" (cmd! (lexic-return-from-lexic) ; expand
                     (switch-to-buffer (lexic-get-buffer)))
        :n "M" (cmd! (lexic-return-from-lexic) ; minimise
                     (lexic-goto-lexic))
        :n "C-p" #'lexic-search-history-backwards
        :n "C-n" #'lexic-search-history-forwards
        :n "/" (cmd! (call-interactively #'lexic-search))))
;; Dictionary:2 ends here

(use-package! info-colors
  :commands (info-colors-fontify-node))

(add-hook 'Info-selection-hook 'info-colors-fontify-node)

;; Tools for manual pages (manpages):1 ends here
(use-package! man
  :ensure nil
  :config
  (let ((map Man-mode-map))
    (define-key map (kbd "i") #'Man-goto-section)
    (define-key map (kbd "g") #'Man-update-manpage)))
(evil-define-key 'normal Info-mode-map (kbd "n") 'Info-next)
(evil-define-key 'normal Info-mode-map (kbd "C-n") 'Info-scroll-up)
(evil-define-key 'normal Info-mode-map (kbd "C-p") 'Info-scroll-down)
(evil-define-key 'normal Info-mode-map (kbd "p") 'Info-prev)
(evil-define-key 'normal Info-mode-map (kbd "H") 'Info-up)
(evil-define-key 'normal Info-mode-map (kbd "f") 'Info-goto-node)
(evil-define-key 'normal Info-mode-map (kbd "C-f") 'Info-menu)

(map! :leader
      :desc "Eshell" "e s" #'eshell
      :desc "Eshell popup toggle" "e t" #'+eshell/toggle
      ;; :desc "Counsel eshell history" "e h" #'counsel-esh-history
      :desc "Counsel eshell history" "e h" #'+eshell/search-history
      :desc "Vterm popup toggle" "v t" #'+vterm/toggle)

(map! :leader
      (:prefix ("=" . "open file")
       :desc "Edit agenda file" "a" #'(lambda () (interactive) (find-file(concat org-directory "/org-agenda/agenda.org")))
       :desc "Edit doom config.org" "c" #'(lambda () (interactive) (find-file "~/.config/doom/config.org"))
       :desc "Edit doom init.el" "i" #'(lambda () (interactive) (find-file "~/.config/doom/init.el"))
       :desc "Edit doom packages.el" "p" #'(lambda () (interactive) (find-file "~/.config/doom/packages.el"))))
(map! :leader
      (:prefix ("= e" . "open eshell files")
       :desc "Edit eshell aliases" "a" #'(lambda () (interactive) (find-file "~/.config/doom/eshell/aliases"))
       :desc "Edit eshell profile" "p" #'(lambda () (interactive) (find-file "~/.config/doom/eshell/profile"))))

;; minor mode for video note taking
(define-minor-mode org-vid-minor-mode
  "Toggle video minor mode for video note taking in org-mode"
  :lighter " Video"
  :keymap
  `(
    (,(kbd "<up>")    . (lambda () (interactive) (mpv-speed-increase 1)))
    (,(kbd "<down>")  . (lambda () (interactive) (mpv-speed-decrease 1)))
    (,(kbd "<right>") . (lambda () (interactive) (mpv-seek-forward 1)))
    (,(kbd "<left>")  . (lambda () (interactive) (mpv-seek-backward 1)))
    (,(kbd "M-p")     . mpv-pause)
    (,(kbd "M-SPC")   . mpv-pause)
    (,(kbd "M-k")     . mpv-kill)
    (,(kbd "M--")     . (lambda () (interactive) (mpv-insert-playback-position t)))
    (,(kbd "M-s")     . (lambda () mpv-seek))
    (,(kbd "M-0")     . (lambda () (interactive) (mpv-speed-set 1)))
    (,(kbd "M-S")     . (lambda () (interactive) (mpv-seek-to-position-at-point)))
    ))
