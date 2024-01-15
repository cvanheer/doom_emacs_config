; ==========================================================================
;;         ORG MODE NOTE CAPTURE
;;==========================================================================
(setq org-default-notes-file "~/PhD/PROJECTS/DIARY/PhD_diary.org")

(setq org-refile-targets
      '(("~/ORGMODE_GTF/general_archive.org" :maxlevel . 2)
        ("~/ORGMODE_GTD/PhD_reading_list.org" :maxlevel . 2)
        ("~/PhD/DIARY/PhD_diary.org" :maxlevel . 2)
        ("~/WORK/DIARY/WORK.org" :maxlevel . 2)
        ("~/WORK/DIARY/WORK_completed_projects.org" :maxlevel . 2)
        ("~/WORK/DIARY/WORK_completed_tasks.org" :maxlevel . 2)))

;; Capture templates
(setq org-capture-templates
      `(("i" "Inbox" entry  (file "~/ORGMODE_GTD/inbox.org") ,(concat "* TODO %?\n" "/Entered on/ %U"))
        ("d" "Doctorate" entry  (file "~/ORGMODE_GTD/PhD_life.org") ,(concat "* TODO %?\n" "/Entered on/ %U"))
        ("p" "Personal" entry  (file "~/ORGMODE_GTD/personal.org") ,(concat "* TODO %?\n" "/Entered on/ %U"))
        ("w" "Work" entry (file "~/WORK/DIARY/Work.org") ,(concat "* TODO %?\n" "/Entered on /%U"))))

; Map some useful keyboard shortcuts to commands
