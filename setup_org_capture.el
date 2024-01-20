; ==========================================================================
;;         ORG MODE NOTE CAPTURE
;;==========================================================================
(setq org-default-notes-file "~/PhD/PROJECTS/DIARY/PhD_diary.org")

(setq org-refile-targets
      '(("~/ORGMODE_GTD/general_archive.org" :maxlevel . 2)
        ("~/ORGMODE_GTD/PhD_reading_list.org" :maxlevel . 2)
        ("~/PhD/DIARY/PhD_diary.org" :maxlevel . 2)
        ("~/WORK/DIARY/WORK.org" :maxlevel . 2)
        ("~/WORK/DIARY/WORK_completed_projects.org" :maxlevel . 2)
        ("~/WORK/DIARY/WORK_completed_tasks.org" :maxlevel . 2)))

;; Capture templates
(setq org-capture-templates
      `(("t" "Thesis" entry  (file "~/.config/doom/orgmode_templates/ox_latex_templates.org") ,(concat "* TODO %?\n" "/Entered on/ %U"))
        ("i" "Inbox" entry  (file "~/Logseq/pages/AR2_project.org") ,(concat "* TODO %?\n" "/Entered on/ %U"))
        ("d" "Doctorate" entry  (file "~/Logseq/journals/PhD_tasks.org") ,(concat "* TODO %?\n" "/Entered on/ %U"))
        ("p" "Personal" entry  (file "~/Logseq/journals/life_admin.org") ,(concat "* TODO %?\n" "/Entered on/ %U"))))
