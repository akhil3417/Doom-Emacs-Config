;;; lisp/setup-dired.el -*- lexical-binding: t; -*-

(use-package! dired
  :ensure nil
  ;; :straight nil
  :defer 1
  :commands (dired dired-jump)
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-AGFhlv --group-directories-first --time-style=long-iso")
  (setq dired-dwim-target t)
  (setq dired-auto-revert-buffer #'dired-directory-changed-p)
                                        ; also see `dired-do-revert-buffer'
  (advice-add 'dired-view-file :around
              (defun dired-view-other-buffer-a (orig-fn &rest args)
                (cl-letf (((symbol-function 'view-file) #'view-file-other-window))
                  (funcall orig-fn))))

  ;; And this is for Emacs 28
  (setq dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir))))
  (defun dired-find-file-other-window ()
  "In Dired, visit this file or directory in another window. If `ace-window' is
available, use it to select window for visiting this file.`"
  (interactive)
  (let ((file (dired-get-file-for-visit))
        (window
         (if (fboundp 'aw-select)
             (aw-select "Select Window")
           (next-window))))
    (select-window window)
    (find-file file)))

  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'hl-line-mode)
  (add-hook 'dired-mode-hook #'toggle-truncate-lines)
  ;; (autoload 'dired-omit-mode "dired-x")

  (add-hook 'dired-load-hook
            (lambda ()
              (interactive)
              (dired-collapse)))

  (add-hook 'dired-mode-hook
            (lambda ()
              (interactive)
              ;; (dired-omit-mode 1)
              (dired-hide-details-mode 1)
              ;; (unless (or dw/is-termux
                          ;; (s-equals? "/home/shiva/" (expand-file-name default-directory)
                (all-the-icons-dired-mode 1))
              (hl-line-mode 1))

(use-package! dired-aux
  :config
  (setq dired-isearch-filenames 'dwim)
  ;; The following variables were introduced in Emacs 27.1
  (setq dired-create-destination-dirs 'ask)
  (setq dired-vc-rename-file t)
  ;; And this is for Emacs 28
  (setq dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir)))))


(use-package! dired-x
  :config
  (setq dired-clean-up-buffers-too t)
  (setq dired-clean-confirm-killing-deleted-buffers t)
  (setq dired-x-hands-off-my-keys t)    ; easier to show the keys I use
  (setq dired-bind-man nil)
  (setq dired-bind-info nil))

(load "~/.config/doom/lisp/prot-dired.el")
(use-package! prot-dired
  :config
  (setq prot-dired-image-viewers '("feh" "sxiv"))
  (setq prot-dired-media-players '("mpv" "vlc"))
  (setq prot-dired-media-extensions
        "\\.\\(mp[34]\\|ogg\\|flac\\|webm\\|mkv\\|3gp\\)")
  (setq prot-dired-image-extensions
        "\\.\\(png\\|jpe?g\\|tiff\\)")
  (setq dired-guess-shell-alist-user ; those are the defaults for ! and & in Dired
        `((,prot-dired-image-extensions (prot-dired-image-viewer))
          (,prot-dired-media-extensions (prot-dired-media-player))))

  (add-hook 'dired-mode-hook #'prot-dired-setup-imenu))

(use-package! dired-subtree
  :ensure t
  :after dired
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package! wdired
  :config
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))

(use-package! async
  :ensure)

(use-package! dired-async
  :after (dired async)

  :hook (dired-mode . dired-async-mode))
(use-package! image-dired
  :config
  (setq image-dired-external-viewer "xdg-open")
  (setq image-dired-thumb-size 80)
  (setq image-dired-thumb-margin 2)
  (setq image-dired-thumb-relief 0)
  (setq image-dired-thumbs-per-row 4)
  (define-key image-dired-thumbnail-mode-map
    (kbd "<return>") #'image-dired-thumbnail-display-external))
  (use-package! dired-rainbow
    :defer 2
    :config
    (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
    (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
    (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
    (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
    (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
    (dired-rainbow-define media "#de751f" ("webm" "mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac" "3gp"))
    (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
    (dired-rainbow-define log "#c17d11" ("log"))
    (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
    (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
    (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
    (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
    (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))


  ;; Use parallel versions of comression programs.
  (setq dired-compress-file-alist
        '(("\\.gz\\'" . "pigz -9f %i")
          ("\\.bz2\\'" . "pbzip2 -9f %i")
          ("\\.xz\\'" . "pixz -9f %i")
          ("\\.zst\\'" . "zstd -qf -19 --rm -o %o %i"))
        dired-compress-files-alist
        '(("\\.tar\\.gz\\'" . "tar -cf - %i | pigz -c9 > %o")
          ("\\.tar\\.bz2\\'" . "tar -cf - %i | pbzip2 -c9 > %o")
          ("\\.tar\\.xz\\'" . "tar -cf - %i | pixz -c9 > %o")
          ("\\.tar\\.zst\\'" . "tar -cf - %i | zstd -19 -o %o")
          ("\\.tar\\.lz\\'" . "tar -cf - %i | plzip -c9 > %o")
          ("\\.tar\\.lzo\\'" . "tar -cf - %i | lzop -c9 > %o")
          ("\\.zip\\'" . "zip %o -r --filesync %i")
          ("\\.pax\\'" . "pax -wf %o %i"))
        dired-compress-file-suffixes
        '(("\\.tar\\.gz\\'" "" "pigz -dc %i | tar -xf -")
          ("\\.tar\\.xz\\'" "" "pixz -dc %i | tar -xf -")
          ("\\.tgz\\'" "" "pigz -dc %i | tar -xf -")
          ("\\.gz\\'" "" "pigz -d") ; For some reason using `pigz' here does not
                                        ; work properly.
          ("\\.lz\\'" "" "plzip -d")
          ("\\.Z\\'" "" "uncompress")
          ("\\.z\\'" "" "pigz -d")
          ("\\.dz\\'" "" "dictunzip")
          ("\\.tbz\\'" ".tar" "pbunzip2")
          ("\\.bz2\\'" "" "pbunzip2")
          ("\\.xz\\'" "" "pixz -d")
          ("\\.zip\\'" "" "unzip -o -d %o %i")
          ("\\.tar\\.zst\\'" "" "unzstd -c %i | tar -xf -")
          ("\\.tzst\\'" "" "unzstd -c %i | tar -xf -")
          ("\\.zst\\'" "" "unzstd --rm")
          ("\\.7z\\'" "" "7z x -aoa -o%o %i")
          ("\\.tar\\'" ".tgz" nil)))

;;; Add music file to playlist on '!', --lgfang
(add-to-list 'dired-guess-shell-alist-user
             (list "\\.\\(flac\\|mp3\\|ogg\\|wav\\)\\'"
                   '(if (y-or-n-p "Add to emms playlist?")
                        (progn (emms-add-file (dired-get-filename))
                               (keyboard-quit))
                      "mplayer")))

  (use-package! dired-single
    :defer t)

  (use-package! dired-ranger
    :defer t)

  (use-package! dired-collapse
    :defer t)

(use-package! gnus-dired
  :defer 5
  :after dired)

(evil-define-key 'normal dired-mode-map
  (kbd "M-RET") 'dired-display-file
  (kbd "h") 'dired-single-up-directory
  ;; (kbd "H") 'dired-omit-mode
  (kbd "l") 'dired-single-buffer ; use dired-find-file instead of dired-open.
  (kbd "m") 'dired-mark
  (kbd "RET") 'dired-open-file
  (kbd "t") 'dired-toggle-marks
  (kbd "u") 'dired-unmark
  (kbd "C") 'dired-do-copy
  (kbd "i") 'dired-info
  (kbd "I") 'prot-dired-insert-subdir
  (kbd "D") 'dired-do-delete
  (kbd "J") 'dired-goto-file
  (kbd "M") 'dired-do-chmod
  (kbd "O") 'dired-do-chown
  (kbd "P") 'dired-do-print
  (kbd "R") 'dired-do-rename
  (kbd "T") 'dired-do-touch
  (kbd "y") 'dired-ranger-copy
  (kbd "X") 'dired-ranger-move
  (kbd "p") 'dired-ranger-paste
  (kbd "Y") 'dired-copy-filename-as-kill ; copies filename to kill ring.
  (kbd "+") 'dired-create-directory
  (kbd "C-+") 'dired-create-empty-file
  (kbd "F") 'dired-do-find-marked-files ;;open all marked files at once
  (kbd "C-x v v") 'dired-vc-next-action ; Emacs 28
  ;; (kbd "/") 'prot-dired-limit-regexp
  (kbd "C-c C-l") 'prot-dired-limit-regexp
  (kbd "M-s G") 'prot-dired-grep-marked-files ;searches whole dir when one file is marked , provied C-u to search one marked file M-s g is `prot-search-grep'
  (kbd "<tab>") 'dired-subtree-toggle
  (kbd "<s-tab>") 'dired-subtree-cycle
  (kbd "<backtab>") 'dired-subtree-remove ; Shift-TAB
  (kbd "-") 'dired-up-directory
  (kbd "% l") 'dired-downcase
  (kbd "% u") 'dired-upcase
  (kbd "@") 'emms-play-dired
  (kbd "; d") 'epa-dired-do-decrypt
  (kbd "; e") 'epa-dired-do-encrypt))


(map! :leader
      (:prefix ("d" . "dired")
       :desc "Open dired" "d" #'dired
       :desc "Dired jump to current" "j" #'dired-jump)
      (:after dired
       (:map dired-mode-map
        :desc "Peep-dired image previews" "d p" #'peep-dired
        :desc "Dired view file" "d v" #'dired-view-file
        :desc "Find name dired " "d N" #'find-name-dired)))

(use-package! peep-dired
  :general
  (:states '(normal visual)
           :keymaps 'dired-mode-map
           "z p" 'peep-dired)
  (:keymaps 'dired-mode-map
            "P" 'peep-dired)
  :hook (peep-dired-display-file . auto-revert-mode)
  :config
  ;; (add-hook 'peep-dired-hook 'evil-normalize-keymaps)
  (ignore-errors
    (setq peep-dired-display-action-alist
          '(display-buffer-in-direction
            (direction . below)
            (window-height . (lambda (win) (fit-window-to-buffer
                                       win
                                       (floor (* 0.6 (frame-height))))))
            (window-parameters . ((dedicated . t))))))
  (general-def
    :states '(normal visual)
    :keymaps 'peep-dired-mode-map
    :prefix "SPC"
    "SPC" 'peep-dired-scroll-page-down
    "S-SPC" 'peep-dired-scroll-page-up)
  (general-def
    :states '(normal visual)
    :keymaps 'peep-dired-mode-map
    "<backspace>" 'peep-dired-scroll-page-up
    "j" 'peep-dired-next-file
    "k" 'peep-dired-prev-file)

(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

(setq peep-dired-cleanup-on-disable t)
(setq peep-dired-cleanup-eagerly nil)
(setq peep-dired-enable-on-directories nil)
(setq peep-dired-ignored-extensions
      '("mkv" "iso" "mp4" "pdf" "djvu" "one" "mat"
        "fig" "nb" "slx" "slxc" "r2016b" "onetoc2")))
(setq dired-open-extensions
 '(("gif" . "sxiv")
  ("jpg" . "sxiv")
  ("png" . "sxiv")
  ("mkv" . "mpv")
  ("avi" . "mpv")
  ("webm" . "mpv")
  ("3gp" . "mpv")
  ("pdf" . "sioyek")
  ("epub" . "sioyek")
  ("mp4" . "mpv")))

(setq delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files/")

(use-package! dired-sidebar
  :after dired
  :disabled
  :commands (dired-sidebar-toggle-sidebar)
  :general
  ("C-x D"  'list-directory
   "C-x C-d" 'dired-sidebar-toggle-sidebar
   :states '(normal visual)
   "C-w C-d" 'dired-sidebar-toggle-sidebar)

  (:keymaps 'dired-sidebar-mode-map
   :states  '(normal)
   "gO"     'dired-sidebar-find-file-alt
   "RET"    'dired-sidebar-find-file)

  (:keymaps 'dired-sidebar-mode-map
   "-" nil)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'ascii)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

(use-package! ibuffer-sidebar
  :disabled
  :commands +ibuffer-sidebar-toggle
  :general
  ("C-x C-d" '+ibuffer-sidebar-toggle)
  (:states '(normal visual)
   "C-x C-d" '+ibuffer-sidebar-toggle)
  (:keymaps 'space-menu-buffer-map
            :wk-full-keys nil
            "t" '(ibuffer-sidebar-toggle-sidebar :wk "Buffer sidebar"))
  :config
  (defun +ibuffer-sidebar-toggle ()
    "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
    (interactive)
    (when (featurep 'ibuffer)
      (ibuffer-sidebar-toggle-sidebar))
    (dired-sidebar-toggle-sidebar)))

(use-package! dired-rsync
  :ensure t
  :bind (:map dired-mode-map
         ("r" . dired-rsync))
  :hook (dired-rsync-failed . dired-rsync--pop-to-rsync-failed-buf)
  :config
  (setq dired-rsync-unmark-on-completion nil))

(use-package! dired-filter
  :ensure t
  :after dired)

(use-package! diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode))
(use-package! trashed
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

;; [[file:config.org::*open with][open with:1]]
(use-package! openwith
  :config
  (setq openwith-associations
        (list
          (list (openwith-make-extension-regexp
                '("webm" "mpg" "mpeg" "mp3" "mp4"
                  "avi" "wmv" "wav" "mov" "flv"
                  "ogm" "ogg" "3gp" "mkv"))
                "mpv"
                '(file))
          (list (openwith-make-extension-regexp
                '("xbm" "pbm" "pgm" "ppm" "pnm"
                  "png" "gif" "bmp" "tif" "jpeg")) ;; Removed jpg because Telega was
                  ;; causing feh to be opened...
                  "feh"
                  '(file))
          (list (openwith-make-extension-regexp
                '("pdf"))
                "zathura"
                '(file)))))
;; open with:1 ends here
