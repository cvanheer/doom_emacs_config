

;; Set the title

; https://github.com/emacs-dashboard/emacs-dashboard
(use-package dashboard
  :custom
  (dashboard-banner-logo-title "Hello Christina!")
  (dashboard-center-content t)
  (dashboard-items '((agenda . 10)
                     (recents . 5)
                     (projects . 5)
                     ))

  (dashboard-set-file-icons t)
  (dashboard-set-footer nil)
  (dashboard-set-heading-icons t)
  (dashboard-set-navigator t)
  (dashboard-week-agenda t)
  (dashboard-center-content t)
  (dashboard-set-file-icons t)
  (dashboard-startup-banner "~/.config/doom/this_is_fine.png")
  :config (dashboard-setup-startup-hook))
