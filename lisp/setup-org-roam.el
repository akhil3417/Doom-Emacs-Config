;;; lisp/setup-org-roam.el -*- lexical-binding: t; -*-


(use-package! org-roam
  :after org-roam
  :init
  (setq org-roam-db-location (concat org-roam-directory "org-roam.db")
        org-roam-directory "~/org/org-roam2/"
        org-roam-todo-file (concat org-roam-directory "todo/todo.org")
        org-roam-completion-everywhere nil
        ;;Functions tags are special types of tags which tells what the node are for
        ;;In the future, this should probably be replaced by categories
        hp/org-roam-function-tags '("compilation" "argument" "journal" "concept" "tool" "data" "bio" "literature" "event" "website"))
  :config
  ;; Org-roam interface
  (cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
    "Return the node's TITLE, as well as it's HIERACHY."
    (let* ((title (org-roam-node-title node))
           (olp (mapcar (lambda (s) (if (> (length s) 10) (concat (substring s 0 10)  "...") s)) (org-roam-node-olp node)))
           (level (org-roam-node-level node))
           (filetitle (org-roam-get-keyword "TITLE" (org-roam-node-file node)))
           (filetitle-or-name (if filetitle filetitle (file-name-nondirectory (org-roam-node-file node))))
           (shortentitle (if (> (length filetitle-or-name) 20) (concat (substring filetitle-or-name 0 20)  "...") filetitle-or-name))
           (separator (concat " " (all-the-icons-material "chevron_right") " ")))
      (cond
       ((= level 1) (concat (propertize (format "=level:%d=" level) 'display (all-the-icons-material "insert_drive_file" :face 'all-the-icons-dyellow))
                            (propertize shortentitle 'face 'org-roam-olp) separator title))
       ((= level 2) (concat (propertize (format "=level:%d=" level) 'display (all-the-icons-material "insert_drive_file" :face 'all-the-icons-dsilver))
                            (propertize (concat shortentitle separator (string-join olp separator)) 'face 'org-roam-olp) separator title))
       ((> level 2) (concat (propertize (format "=level:%d=" level) 'display (all-the-icons-material "insert_drive_file" :face 'org-roam-olp))
                            (propertize (concat shortentitle separator (string-join olp separator)) 'face 'org-roam-olp) separator title))
       (t (concat (propertize (format "=level:%d=" level) 'display (all-the-icons-material "insert_drive_file" :face 'all-the-icons-yellow))
                  (if filetitle title (propertize filetitle-or-name 'face 'all-the-icons-dyellow)))))))

  (cl-defmethod org-roam-node-functiontag ((node org-roam-node))
    "Return the FUNCTION TAG for each node. These tags are intended to be unique to each file, and represent the note's function.
        journal data literature"
    (let* ((tags (seq-filter (lambda (tag) (not (string= tag "ATTACH"))) (org-roam-node-tags node))))
      (concat
       ;; Argument or compilation
       (cond
        ((member "argument" tags)
         (propertize "=f:argument=" 'display (all-the-icons-material "forum" :face 'all-the-icons-dred)))
        ((member "compilation" tags)
         (propertize "=f:compilation=" 'display (all-the-icons-material "collections" :face 'all-the-icons-dyellow)))
        (t (propertize "=f:empty=" 'display (all-the-icons-material "remove" :face 'org-hide))))
       ;; concept, bio, data or event
       (cond
        ((member "concept" tags)
         (propertize "=f:concept=" 'display (all-the-icons-material "blur_on" :face 'all-the-icons-dblue)))
        ((member "tool" tags)
         (propertize "=f:tool=" 'display (all-the-icons-material "build" :face 'all-the-icons-dblue)))
        ((member "bio" tags)
         (propertize "=f:bio=" 'display (all-the-icons-material "people" :face 'all-the-icons-dblue)))
        ((member "event" tags)
         (propertize "=f:event=" 'display (all-the-icons-material "event" :face 'all-the-icons-dblue)))
        ((member "data" tags)
         (propertize "=f:data=" 'display (all-the-icons-material "data_usage" :face 'all-the-icons-dblue)))
        (t (propertize "=f:nothing=" 'display (all-the-icons-material "format_shapes" :face 'org-hide))))
       ;; literature
       (cond
        ((member "literature" tags)
         (propertize "=f:literature=" 'display (all-the-icons-material "book" :face 'all-the-icons-dcyan)))
        ((member "website" tags)
         (propertize "=f:website=" 'display (all-the-icons-material "move_to_inbox" :face 'all-the-icons-dsilver)))
        (t (propertize "=f:nothing=" 'display (all-the-icons-material "book" :face 'org-hide))))
       ;; journal
       )))

  (cl-defmethod org-roam-node-othertags ((node org-roam-node))
    "Return the OTHER TAGS of each notes."
    (let* ((tags (seq-filter (lambda (tag) (not (string= tag "ATTACH"))) (org-roam-node-tags node)))
           (specialtags hp/org-roam-function-tags)
           (othertags (seq-difference tags specialtags 'string=)))
       (propertize
        (string-join
         (append '(" ") othertags)
         (propertize "#" 'display (all-the-icons-material "label" :face 'all-the-icons-dgreen)))
        'face 'all-the-icons-dgreen)))

  (cl-defmethod org-roam-node-backlinkscount ((node org-roam-node))
    (let* ((count (caar (org-roam-db-query
                         [:select (funcall count source)
                          :from links
                          :where (= dest $s1)
                          :and (= type "id")]
                         (org-roam-node-id node)))))
      (if (> count 0)
          (concat (propertize "=has:backlinks=" 'display (all-the-icons-material "link" :face 'all-the-icons-blue)) (format "%d" count))
        (concat (propertize "=not-backlinks=" 'display (all-the-icons-material "link" :face 'org-hide))  " "))))

  (cl-defmethod org-roam-node-directories ((node org-roam-node))
    (if-let ((dirs (file-name-directory (file-relative-name (org-roam-node-file node) org-roam-directory))))
        (concat
         (if (string= "journal/" dirs)
             (all-the-icons-material "edit" :face 'all-the-icons-dsilver)
           (all-the-icons-material "folder" :face 'all-the-icons-dsilver))
         (propertize (string-join (f-split dirs) "/") 'face 'all-the-icons-dsilver) " ")
      ""))

  (defun +marginalia--time-colorful (time)
    (let* ((seconds (float-time (time-subtract (current-time) time)))
           (color (doom-blend
                   (face-attribute 'marginalia-on :foreground nil t)
                   (face-attribute 'marginalia-off :foreground nil t)
                   (/ 1.0 (log (+ 3 (/ (+ 1 seconds) 345600.0)))))))
      ;; 1 - log(3 + 1/(days + 1)) % grey
      (propertize (marginalia--time time) 'face (list :foreground color :slant 'italic))))

  (setq org-roam-node-display-template
        (concat  "${backlinkscount:16} ${functiontag} ${directories}${hierarchy}${othertags} ")
        org-roam-node-annotation-function
        (lambda (node) (+marginalia--time-colorful (org-roam-node-file-mtime node))))
  )

(save-window-excursion
  (find-file org-roam-todo-file)
  (save-buffer))
(defface hp/org-roam-count-overlay-face
  '((t :inherit org-list-dt :height 0.8))
  "Face for Org Roam count overlay.")

(defun hp/org-roam--count-overlay-make (pos count)
  (let* ((overlay-value (propertize
                         (concat "·" (format "%d" count) " ")
                         'face 'hp/org-roam-count-overlay-face 'display '(raise 0.2)))
         (ov (make-overlay pos pos (current-buffer) nil t)))
    (overlay-put ov 'roam-backlinks-count count)
    (overlay-put ov 'priority 1)
    (overlay-put ov 'after-string overlay-value)))

(defun hp/org-roam--count-overlay-remove-all ()
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'roam-backlinks-count)
      (delete-overlay ov))))

(defun hp/org-roam--count-overlay-make-all ()
  (hp/org-roam--count-overlay-remove-all)
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (elem)
      (when (string-equal (org-element-property :type elem) "id")
        (let* ((id (org-element-property :path elem))
               (count (caar
                       (org-roam-db-query
                        [:select (funcall count source)
                         :from links
                         :where (= dest $s1)
                         :and (= type "id")]
                        id))))
          (when (< 0 count)
            (hp/org-roam--count-overlay-make
             (org-element-property :end elem)
             count)))))))

(define-minor-mode hp/org-roam-count-overlay-mode
  "Display backlink count for org-roam links."
  :after-hook
  (if hp/org-roam-count-overlay-mode
      (progn
        (hp/org-roam--count-overlay-make-all)
        (add-hook 'after-save-hook #'hp/org-roam--count-overlay-make-all nil t))
    (hp/org-roam--count-overlay-remove-all)
    (remove-hook 'after-save-hook #'hp/org-roam--count-overlay-remove-all t)))

(add-hook 'org-mode-hook #'hp/org-roam-count-overlay-mode)


(defun org-roam-buffer-setup ()
  "Function to make org-roam-buffer more pretty."
  (progn
    (setq-local olivetti-body-width 44)
    (variable-pitch-mode 1)
    (olivetti-mode 1)
    ;; (centaur-tabs-local-mode -1)

  (set-face-background 'magit-section-highlight (face-background 'default))))

(after! org-roam
(add-hook! 'org-roam-mode-hook #'org-roam-buffer-setup))
;;

;; [[file:config.org::*Modeline file name][Modeline file name:1]]
(defadvice! doom-modeline--buffer-file-name-roam-aware-a (orig-fun)
  :around #'doom-modeline-buffer-file-name ; takes no args
  (if (s-contains-p org-roam-directory (or buffer-file-name ""))
      (replace-regexp-in-string
       "\\(?:^\\|.*/\\)\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[0-9]*-"
       "🢔(\\1-\\2-\\3) "
       (subst-char-in-string ?_ ?  buffer-file-name))
    (funcall orig-fun)))
;; Modeline file name:1 ends here

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :commands org-roam-ui-open
  :hook (org-roam . org-roam-ui-mode)
  :config
  (require 'org-roam) ; in case autoloaded
  (defun org-roam-ui-open ()
    "Ensure the server is active, then open the roam graph."
    (interactive)
    (unless org-roam-ui-mode (org-roam-ui-mode 1))
    (browse-url-xdg-open (format "http://localhost:%d" org-roam-ui-port))))

(after! org-roam
  (setq org-roam-capture-templates
        `(("s" "standard" plain "%?"
           :if-new
           (file+head "standard/%<%Y%m%d%H%M%S>--${slug}.org"
                      "#+title: ${title}\n#+date\n#+filetags: \n\n ")
           :unnarrowed t)

          ("d" "definition" plain
           "%?"
           :if-new
           (file+head "definition/${slug}.org" "#+title: ${title}\n#+filetags: definition \n\n* Definition\n\n\n* Examples\n")
           :unnarrowed t)
          ("r" "ref" plain "%?"
           :if-new
           (file+head "ref/${citekey}.org"
                      "#+title: ${slug}: ${title}\n
                      \n#+date : %<%Y%m%d%H%M%S>
                      \n#+filetags: reference ${keywords} \n
                      \n* ${title}\n\n
                      \n* Summary
                      \n\n\n* Rough note space\n")
           :unnarrowed t)
          ("P" "person" plain "%?"
           :if-new
           (file+head "${slug}.org" "%^{relation|some guy|family|friend|colleague}p %^{birthday}p %^{address}p
,#+title:${slug}\n#+filetags: :person: \n")
           :unnarrowed t)
          ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
           :if-new (file+head "Projects/%<%Y%m%d%H%M%S>--${slug}.org" "#+title: ${title}\n#+filetags: Project")
           :unnarrowed t)
          ("b" "book notes" plain
           "\n* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
           :if-new (file+head "BookNotes/%<%Y%m%d%H%M%S>--${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ;; templates can be used as well, which is pretty cool
          ;; ("b" "book notes" plain (file "~/org/org-roam2/Templates/BookNotesTemplate.org")
          ;;  :if-new (file+head "BookNotes/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
          ;;  :unnarrowed t)
          ("l" "programming language" plain
           "* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
           :if-new (file+head "ProgLangs/%<%Y%m%d%H%M%S>--${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)

          ("n" "notes" plain "%?"
           :if-new
           (file+head "Notes/%<%Y%m%d%H%M%S>--${title}.org" "#+title: ${title}\n#+STARTUP: content\n#+date : %<%Y-%m-%d>\n#+filetags: :%^{tags|article:note|learn|info|posix|web|intresting|emacs|gnu_linux|health|food|shopping|pentesting|tv_shows|elfeed|void_linux|internet} ")
           :immediate-finish t
           :unnarrowed t)

          ("a" "article" plain "%?"
           :if-new
           (file+head "Articles/%<%Y%m%d%H%M%S>--${title}.org" "#+title: ${title}\n#+STARTUP: content\n#+date : %<%Y-%m-%d>\n#+filetags: :%^{tags|article:read|learn|info|posix|web|intresting|emacs|gnu_linux|health|food|shopping|pentesting|tv_shows|elfeed|void_linux|internet} ")

           ;; (file+head "Articles/%<%Y%m%d%H%M%S>-${title}.org" "#+title: ${title}\n#+date : %<%Y%m%d%H%M%S>\n#+filetags: :article:%^{tags|read|learn|info}
            ;; \n\n%i %a")
           :immediate-finish t
           :unnarrowed t))))
(setq org-roam-dailies-capture-templates
      '(("d" "default" entry "* %<%I:%M %p>: %?"
         :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))

;; ;; Creating the property “type” on my nodes.
(cl-defmethod org-roam-node-type ((node org-roam-node))
  "Return the TYPE of NODE."
  (condition-case nil
      (file-name-nondirectory
       (directory-file-name
        (file-name-directory
         (file-relative-name (org-roam-node-file node) org-roam-directory))))
    (error "")))

;; Modifying the display template to show the node “type”

;; (setq org-roam-node-display-template
;;        (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
;; #("${doom-type:15} ${doom-hierarchy:*}  ${doom-tags:42}" 20 35
;;   (face font-lock-keyword-face)
;;   36 51
;;   (face org-tag)))

(defun my/org-roam--backlinks-list (file)
  (if (org-roam--org-roam-file-p file)
      (--reduce-from
       (concat acc (format "- [[file:%s][%s]]\n"
                           (file-relative-name (car it) org-roam-directory)
                           (org-roam--get-title-or-slug (car it))))
       "" (org-roam-sql [:select [from]
                         :from links
                         :where (= to $s1)
                         :and from :not :like $s2] file "%private%"))
    ""))

(defun my/org-export-preprocessor (_backend)
  (let ((links (my/org-roam--backlinks-list (buffer-file-name))))
    (unless (string= links "")
      (save-excursion
        (goto-char (point-max))
        (insert (concat "\n* Backlinks\n" links))))))
