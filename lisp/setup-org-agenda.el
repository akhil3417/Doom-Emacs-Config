;;; lisp/setup-org-agenda.el -*- lexical-binding: t; -*-


;; [[file:config.org::*Habit Tracking][Habit Tracking:1]]
(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60)
;; Habit Tracking:1 ends here

;; [[file:config.org::*Super agenda][Super agenda:2]]
(use-package! org-super-agenda
  :commands org-super-agenda-mode)

;; (use-package! org-super-agenda
;;   :hook (org-agenda-mode . org-super-agenda-mode)
;; )
;; Super agenda:2 ends here

;; [[file:config.org::*Super agenda][Super agenda:3]]
(after! org
 ;; '(org-agenda-files
 ;;   '( "~/org/org-capture/todo.org" "~/org/org-capture/webnotes.org" "~/org/org-roam2/daily/" "~/org/org-roam2/todo/todo.org"))
 (setq org-agenda-files '("~/org/org-agenda/" "~/org/org-capture/todo.org")))
 (setq org-default-notes-file "~/org/notes/notes.org")
;; Super agenda:3 ends here

;; [[file:config.org::*Super agenda][Super agenda:5]]
;; (after! org-agenda
;; (org-super-agenda-mode)

(after! org-agenda
  (let ((inhibit-message t))
    (org-super-agenda-mode)))

(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-block-separator nil
      ;; org-agenda-tags-column 100 ;; from testing this seems to be a good value
      org-agenda-tags-column 0
      org-agenda-start-with-log-mode t
      org-agenda-block-separator ?─
      calendar-latitude 34.034520
      calendar-longitude -84.456010
      calendar-location-name "Marietta, GA"
      org-agenda-compact-blocks t)
      ;; org-agenda-include-diary t) ;; TODO fix diary
;; org-agenda-start-with-log-mode t)

(setq org-agenda-custom-commands
      '(("o" "Overview"
         ((agenda "" ((org-agenda-span 'day)
                      (org-agenda-start-day nil)
                      (org-super-agenda-groups
                       '((:name "Today"
                          :time-grid t
                          :date today
                          :todo "TODAY"
                          :scheduled today
                          :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '((:name "Ongoing"
                           :tag "ongoing"
                           :todo "[-]"
                           :order 1)
                          (:name "Due Today"
                           :deadline today
                           :order 2)
                          (:name "Important"
                           :tag "Important"
                           :priority "A"
                           :order 3)
                          (:name "Next to do"
                           :todo "NEXT"
                           :order 4)
                          (:name "Due Soon"
                           :deadline future
                           :order 8)
                          (:name "Overdue"
                           :deadline past
                           :face error
                           :order 7)
                          (:name "Assignments"
                           :tag "Assignment"
                           :order 10)
                          (:name "Issues"
                           :tag "Issue"
                           :todo ("FIXME")
                           :order 12)
                          (:name "Emacs"
                           :tag "Emacs"
                           :order 13)
                          (:name "Projects"
                           :tag "Project"
                           :todo "PROJ"
                           :order 14)
                          (:name "Research"
                           :tag "Research"
                           :todo "IDEA"
                           :order 15)
                          (:name "To read & Learn"
                           :tag ("Read" "learn")
                           :order 30)
                          (:name "Waiting"
                           :todo "WAIT"
                           :tag "waiting"
                           :order 20)
                          (:name "University"
                           :tag "uni"
                           :order 32)
                          (:name "Binge Watch"
                           :tag ("tvshow" "movie" "documentry")
                           :order 90)
                          (:name "Trivial"
                           :priority<= "E"
                           :tag ("Trivial" "Unimportant")
                           :todo ("SOMEDAY" )
                           :order 36)
                          (:discard (:tag ("Chore" "Routine" "Daily")))))))))
        ("d" "Dashboard"
          ((agenda "" ((org-deadline-warning-days 7)))
           (todo "NEXT"
                 ((org-agenda-overriding-header "Next Tasks")))
           (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

         ("n" "Next Tasks"
          ((todo "NEXT"
                 ((org-agenda-overriding-header "Next Tasks")))))


         ("w" "Work Tasks" tags-todo "+work")

         ;; Low-effort next actions
         ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
          ((org-agenda-overriding-header "Low Effort Tasks")
           (org-agenda-max-todos 20)
           (org-agenda-files org-agenda-files)))

         ("W" "Workflow Status"
          ((todo "WAIT"
                 ((org-agenda-overriding-header "Waiting on External")
                  (org-agenda-files org-agenda-files)))
           (todo "REVIEW"
                 ((org-agenda-overriding-header "In Review")
                  (org-agenda-files org-agenda-files)))
           (todo "PLAN"
                 ((org-agenda-overriding-header "In Planning")
                  (org-agenda-todo-list-sublevels nil)
                  (org-agenda-files org-agenda-files)))
           (todo "BACKLOG"
                 ((org-agenda-overriding-header "Project Backlog")
                  (org-agenda-todo-list-sublevels nil)
                  (org-agenda-files org-agenda-files)))
           (todo "READY"
                 ((org-agenda-overriding-header "Ready for Work")
                  (org-agenda-files org-agenda-files)))
           (todo "ACTIVE"
                 ((org-agenda-overriding-header "Active Projects")
                  (org-agenda-files org-agenda-files)))
           (todo "COMPLETED"
                 ((org-agenda-overriding-header "Completed Projects")
                  (org-agenda-files org-agenda-files)))
           (todo "CANC"
                 ((org-agenda-overriding-header "Cancelled Projects")
                  (org-agenda-files org-agenda-files)))))))


(let ((map global-map))
  (define-key map (kbd "C-c a") #'org-agenda)
  (define-key map (kbd "C-c c") #'org-capture)
  (define-key map (kbd "C-c l") #'org-store-link)
  (define-key map (kbd "C-c L") #'org-insert-link-global)
  (define-key map (kbd "C-c O") #'org-open-at-point-global))
(let ((map org-mode-map))
  (define-key map (kbd "C-c M-l") #'org-insert-last-stored-link)
  (define-key map (kbd "C-c C-M-l") #'org-toggle-link-display))

(defun my-org-check-agenda ()
  "Peek at agenda."
  (interactive)
  (cond
   ((derived-mode-p 'org-agenda-mode)
    (if (window-parent) (delete-window) (bury-buffer)))
   ((get-buffer "*Org Agenda*")
    (switch-to-buffer-other-window "*Org Agenda*"))
   (t (org-agenda nil "a"))))
;; Super agenda:5 ends here

;; [[file:config.org::*Basic settings][Basic settings:1]]
;;(after! org-roam
;; (setq
;;       org-roam-directory "~/org/org-roam2/"
;;       org-roam-db-location (concat org-roam-directory "org-roam.db")
;;       org-roam-todo-file (concat org-roam-directory "todo/todo.org"))
;; (save-window-excursion
;;   (find-file org-roam-todo-file)
;;   (save-buffer))
;; Basic settings:1 ends here
;;
;;
