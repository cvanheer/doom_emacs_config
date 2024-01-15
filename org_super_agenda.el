(use-package org-super-agenda
  :after org
  :config
  (org-super-agenda-mode 1)
  (setq org-agenda-start-day nil
        org-agenda-span 5) ;; i.e. start today
  (setq org-super-agenda-groups
       '((:auto-category t))))
  (setq org-agenda-deadline-leaders '("DUE:       " "In %3d d.: " "%2d d. ago: "))
  (setq org-agenda-scheduled-leaders '("DO: " "Sched.%2dx: "))
