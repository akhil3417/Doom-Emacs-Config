;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; (package! mpv)
;; (package! emms-player-simple-mpv)
;; (package! yaml)
;; (package! aio)
;; (package! cape)
;; (package! visual-regexp)
;; (package! visual-regexp-steroids)
;; (package! org-roam-timestamps :recipe (:host github :repo "ThomasFKJorna/org-roam-timestamps"))
;; (package! clippy)
;; ;; (package! undo-tree)
(package! trashed)
;; (package! olivetti)
(package! openwith)
;; (package! expand-region)
;; ;; (package! telephone-line)
(package! elpher)
;; (package! all-the-icons-ivy-rich)
;; (package! bookmark)
(package! engine-mode)
;; (package! wolfram)
;; (package! dashboard)
(package! tldr)
;; (package! multi-vterm)
;; (package! cloc)
;; (package! yahtzee)
;; (package! sudoku)
;; (package! 2048-game)
;; (package! chess)
;; (package! pacmacs)
(package! smart-compile)
;; (package! emacsql-sqlite-builtin)
;;   (package! gitconfig-mode
;; 	    :recipe (:host github :repo "magit/git-modes"
;; 			   :files ("gitconfig-mode.el")))
;; ;; Packages in MELPA/ELPA/emacsmirror:1 ends here
;; (package! kbd-mode
;;   :recipe (:host github
;;            :repo "kmonad/kbd-mode"))
(package! tmr
  :recipe (:host github
           :repo "protesilaos/tmr.el"))
;; (package! gitignore-mode
;;     :recipe (:host github :repo "magit/git-modes"
;;        :files ("gitignore-mode.el")))
;; (package! logos
;;     :recipe (:host github :repo "protesilaos/logos"
;;        :files ("logos.el")))
;; (package! keycast :pin "72d9add8ba16e0cae8cfcff7fc050fa75e493b4e")
;; (package! devdocs)
(package! beframe)
(package! substitute)
(package! webpaste)
(package! telega)
(package! google-translate)
(package! info-colors :pin "2e237c301ba62f0e0286a27c1abe48c4c8441143")
(package! miniedit)
;; ;; Mini-buffer editing more space:1 ends here
;; (package! screenshot :recipe (:host github :repo"tecosaur/screenshot" :files ("*.el" "filters" "preprocessors")))
;; (package! org-modern :pin "7d037569bc4a05f40262ea110c4cda05c69b5c52")
(package! org-pandoc-import :recipe
  (:host github :repo "tecosaur/org-pandoc-import" :files ("*.el" "filters" "preprocessors")))
;; ;; Importing with Pandoc:1 ends here
;; (package! orgdiff :recipe (:host github :repo "tecosaur/orgdiff"))
;; ;; Document comparison:1 ends here
;; (package! cdlatex)
(package! org-super-agenda :pin "f4f528985397c833c870967884b013cf91a1da4a")
(package! org-fragtog :pin "c675563af3f9ab5558cfd5ea460e2a07477b0cfd")
;; ;; More eager rendering:1 ends here
;; 
(package! laas :recipe (:host github :repo "tecosaur/LaTeX-auto-activating-snippets"))

(package! eshell-bookmark)
;; (package! key-chord)
;; (package! org-appear :recipe (:host github :repo "awth13/org-appear")
;;   :pin "eb9f9db40aa529fe4b977235d86494b115281d17")
;; (package! org-ol-tree :recipe (:host github :repo "Townk/org-ol-tree")
;;   :pin "207c748aa5fea8626be619e8c55bdb1c16118c25")
;; (package! graphviz-dot-mode :pin "a1b7d66f5c20404a1e59c2ee5e841022622535b8")
(package! ox-gfm :pin "46faa67dbb3fb0cd7a76c3fe518f16e4195c22c7")
;; (package! org-roam-server)
(package! pdftotext :recipe (:host github :repo "tecosaur/pdftotext.el"))
(package! org-modern)
(package! org-appear :recipe (:host github :repo "awth13/org-appear")
  :pin "eb9f9db40aa529fe4b977235d86494b115281d17")
(package! doct
  :recipe (:host github :repo "progfolio/doct")
  :pin "9ed9b8c7f7e2ea2d2fb739d65ae4626a1cf16b9f")
(package! websocket :pin "82b370602fa0158670b1c6c769f223159affce9b") ; dependency of `org-roam-ui'
(package! key-chord)
(package! visual-regexp)
(package! visual-regexp-steroids)
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")) :pin "5ac74960231db0bf7783c2ba7a19a60f582e91ab")

(package! codeium
  :recipe (:host github :repo "Exafunction/codeium.el" :files ("*.el")))
(package! gptel)
(package! pcmpl-args)
(package! versuri)
;; (package! w3m)
(package! org-web-tools)
(package! simpleclip)
(package! lexic :recipe (:host github :repo"tecosaur/lexic" :files ("*.el" "filters" "preprocessors")))

(package! ef-themes
  :recipe (:host github
           :repo "protesilaos/ef-themes"))
(package! theme-magic)
(package! modus-themes)
(package! cape)
(package! nerd-icons)
(package! all-the-icons)
(package! eat)
(package! devdocs)
(package! impatient-mode)
(package! 0x0)
(package! diff-hl)
;; (package! vertico-posframe :recipe (:host github :repo"tumashu/vertico-posframe" :files ("*.el" "filters" "preprocessors")))
(package! treemacs-projectile)
(package! treemacs-persp)
