
;;   ORG AGENDA SETTINGS
(after! org (setq org-agenda-files (list "~/Logseq/journals/PhD_diary.org" ; phd diary / meetings
                                         "~/Logseq/journals/PhD_tasks.org"
                                         "~/Logseq/pages/AR2_project.org"
                                         "~/Logseq/journals/Work.org"
                                         "~/Logseq/journals/life_admin.org"
                                         "~/Logseq/pages/capstone_marking_2023.org"))) ; work project notes

(after! org
(setq org-agenda-skip-scheduled-if-done t
      org-agenda-text-search-extra-files '(agenda-archives) ; archive search (s)
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-block-separator t
      org-agenda-compact-blocks t
      ;org-agenda-remove-tags t
      ;org-agenda-prefix-format "   %i"
      org-agenda-start-day nil ;; i.e. today
      org-agenda-start-on-weekday nil
      org-support-shift-select t
      org-agenda-span 7
      org-agenda-start-with-log-mode t
      org-agenda-dim-blocked-tasks nil
      org-agenda-compact-blocks t
      org-agenda-tags-column -100 ; take advantage of the screen width
      org-agenda-show-log t
      org-agenda-sticky t
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-deadline-warning-days 60
      org-enforce-todo-dependencies t ; you have to mark children as done before parent item
      org-log-done (quote time) ; mark when things got done
      org-log-redeadline (quote time) ; mark when you rescheduled deadline for something
      org-log-reschedule (quote time) ;mark when you changed scheduled date for something
      org-time-stamp-rounding-minutes (quote (0 5))
      org-agenda-time-grid (quote
       ((daily today remove-match)
        (900 1000 1100 1200 1300 1400 1500 1600 1700 1800)
        "......" "----------------"))
      ;org-blank-before-new-entry (quote ((heading) (plain-list-item)))
      ))


;; ==========================================================================

;(org-agenda nil "a") ;; automatically open atis startup

;; ========== Orgmode keywords for TODOs ========
(after! org (setq org-todo-keywords
                  (quote ((sequence "PAPER EDITS(e)" "READ(r)" "TODO(t)" "URGENT(u)" "NEXT(n)"
                                    "DOING(p)"
                                    "MEETING(me)" "|" "DONE(d)")
              (sequence "WAITING(w)" "|" "PHONE"))))) ; "CANCELLED(c)"



;; This adds a variable so you can have global tags in org-mode for GTD
(after! org (setq org-tag-list '(("@work" . ?w) ("@home" . ?h) ("@PhD" . ?l)  ("@computer" . ?l))))

;;(after! org (setq org-agenda-files (append (file-expand-wildcards "~/.org/gtd/*.org"))))

(after! org (setq org-columns-default-format
      "%TODO %30ITEM %6CLOCKSUM %20DEADLINE %20SCHEDULED %PRIORITY %TAGS %8Effort(Effort){:} "))
                                        ; Define Effort estimates for your work
(after! org (setq org-global-properties (quote (("EFFORT_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 8:00")))))

;; Define a function that switches project to done when subtasks are done
(after! org (defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO")))))

(after! org (add-hook 'org-after-todo-statistics-hook 'org-summary-todo))
(after! org (setq org-enforce-todo-dependencies t)) ;; requires subtask to be done before parent is done

(after! org(setq org-todo-keyword-faces
      (quote (

              ("TODO" :foreground "#6495ed" ) ; cornflour blue
              ("URGENT" :foreground "#d00909" :weight bold) ; blue red (like lipstick colour)
              ("NEXT" :foreground "#d26aff" ) ; royal purple
              ("DOING" :foreground "#ff9933" ) ; orange
              ("PAPER EDITS" :foreground "#5ae5b7" :weight bold)
              ("READ" :foreground "#ffad17" ) ; goldy
              ("MEETING" :foreground "#63cdb0")
              ("DONE" :foreground "#277e62" ) ; dark forest green
              ("WAITING" :foreground "#ffc986" )
              ;("CANCELLED" :foreground "forest green" ) ;there are
              ("PHONE" :foreground "forest green" )))))

;;===========  CLOCKING IN AND OUT OF TASKS ===============
;; clocking in: C-c C-x C-i = clocking in
;; clocking out: C-c C-x C-i = clocking out

;(after! org (setq org-agenda-time-grid
      ;(quote
       ;((daily today remove-match)
       ; (900 1000 1100 1200 1300 1400 1500 1600 1700 1800)
       ; "......" "----------------"))))

;; Transparent screen
;(set-frame-parameter (selected-frame)'alpha '(95 . 90))
;(add-to-list 'default-frame-alist'(alpha . (95 . 90)))

;;====================== OTHER SETTINGS ================================
;; Custom agenda command definitions
;; oA

;; ===============================================================
; ======= ORG AGENDA SUPER GROUP ======
; https://github.com/alphapapa/org-super-agenda/blob/master/examples.org#automatically-by-group

(after! org(setq org-columns-default-format
      "%TODO %30ITEM %6CLOCKSUM %20DEADLINE %20SCHEDULED %PRIORITY %TAGS %8Effort(Effort){:} "))
                                        ; Define Effort estimates for your work
(after! org (setq org-global-properties (quote (("EFFORT_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 8:00")))))


;; (after! org
;;   (setq svg-tag-tags
;;         '(
;;           ("DONE\\b" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))

;;           ("\\/\\/\\W?TODO\\b:" . ((lambda (tag) (svg-tag-make "TODO" :face 'font-lock-constant-face :inverse t :margin 0 :crop-right t))))
;;           ("TODO\\b:\\(.*\\)" . ((lambda (tag) (svg-tag-make tag :face 'font-lock-constant-face :crop-left t))))

;;           ("\\/\\/\\W?URGENT\\b:\\|URGENT\\b:" . ((lambda (tag) (svg-tag-make "URGENT" :face 'font-lock-doc-face :inverse t :margin 0 :crop-right t))))
;;           ("URGENT\\b:\\(.*\\)" . ((lambda (tag) (svg-tag-make tag :face 'font-lock-doc-face :crop-left t))))

;;           ("\\/\\/\\W?swiftlint:disable" . ((lambda (tag) (svg-tag-make "swiftlint:disable" :face 'org-level-1 :inverse t :margin 0 :crop-right t))))
;;           ("swiftlint:disable\\(.*\\)" . ((lambda (tag) (svg-tag-make tag :face 'org-level-1 :crop-left t))))

;;          ("\\/\\/\\W?swiftlint:enable" . ((lambda (tag) (svg-tag-make "swiftlint:enabled" :face 'org-level-2 :inverse t :margin 0 :crop-right t))))
;;           ("swiftlint:enable\\(.*\\)" . ((lambda (tag) (svg-tag-make tag :face 'org-level-2 :crop-left t))))

;;           ("\\/\\/\\W?FIXME\\b:\\|FIXME\\b:" . ((lambda (tag) (svg-tag-make "FIXME" :face 'org-todo :inverse t :margin 0 :crop-right t))))
;;           ("FIXME\\b:\\(.*\\)" . ((lambda (tag) (svg-tag-make tag :face 'org-todo :crop-left t))))
;;           )))

;; Define a function that switches project to done when subtasks are done
(after! org (defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO")))))

(after! org (add-hook 'org-after-todo-statistics-hook 'org-summary-todo))
(after! org (setq org-enforce-todo-dependencies t)) ;; requires subtask to be done before parent is done

;;===========  CLOCKING IN AND OUT OF TASKS ===============
;; clocking in: C-c C-x C-i = clocking in
;; clocking out: C-c C-x C-i = clocking out
(after! org (setq org-agenda-time-grid
      (quote
       ((daily today remove-match)
        (900 1000 1100 1200 1300 1400 1500 1600 1700 1800)
        "......" "----------------"))))

;; Addition as per Nicolas P Rougier
(setq org-agenda-custom-commands
      '(("g" "Get Things Done (GTD)"
         ((agenda ""
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline))
                   (org-deadline-warning-days 0)))
          (todo "NEXT"
                ((org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'deadline))
                 (org-agenda-prefix-format "  %i %-12:c [%e] ")
                 (org-agenda-overriding-header "\nTasks\n")))

          ;(todo "PHD DIARY"
                ;((org-agenda-skip-function
                  ;'(org-agenda-skip-entry-if 'deadline))
                 ;(org-agenda-prefix-format "  %i %-12:c [%e] ")
                ; (org-agenda-overriding-header "\nPhD\n")))

          (agenda nil
                  ((org-agenda-entry-types '(:deadline))
                   (org-agenda-format-date "")
                   (org-deadline-warning-days 7)
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
                   (org-agenda-overriding-header "\nDeadlines")))
          (tags-todo "inbox" ; looks for the inbox tag
                     ((org-agenda-prefix-format "  %?-12t% s")
                      ;(org-agenda-files (list "~/ORGMODE_GTD/inbox.org"))
                      (org-agenda-overriding-header "\nInbox\n")))
          (tags "CLOSED>=\"<today>\""
                ((org-agenda-overriding-header "\nCompleted today\n")))))))

;


(after! org
  (map! :map org-mode-map
        :n "M-j" #'org-metadown
        :n "M-k" #'org-metaup)
  )



(after! org
(customize-set-value
 'org-agenda-category-icon-alist
 `(
   ("Late marking" "~/.config/icons/fire.svg" nil nil :ascent center :mask heuristic :height 20)
   ("Teaching" "~/.config/icons/pencil.svg" nil nil :ascent center :mask heuristic :height 20)
   ("Consulting" "~/.config/icons/rocket.svg" nil nil :ascent center :mask heuristic :height 25)
   ("PhD" "~/.config/icons/tree.svg" nil nil :ascent center :mask heuristic :height 30)
   )))

; http://doc.endlessparentheses.com/Var/org-agenda-prefix-format.html
(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (timeline . "  % s")
        (todo . " %i %-12:c")
        (tags . " %i %-12:c")
        (search . " %i %-12:c")))

; ----- SVG TAG MODE -------
; https://github.com/rougier/svg-tag-mode
;; (add-to-list 'load-path "~/.config/doom/svg-tag-mode/svg-tag-mode/")
;; (load "~/.config/doom/svg-tag-mode/svg-tag-mode.el")
;; (use-package! svg-tag-mode
;;   :config
;;   (global-svg-tag-mode))
;; (setq svg-tag-tags
;;       '(("\\(:[A-Z]+:\\)" . ((lambda (tag)
;;                                (svg-tag-make tag :beg 1 :end -1))))))
