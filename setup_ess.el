;;; ess_setup.el -*- lexical-binding: t; -*-


; R studio like settings
(setq display-buffer-alist
      '(("*R Dired"
     (display-buffer-reuse-window display-buffer-at-bottom)
     (window-width . 0.5)
     (window-height . 0.25)
     (reusable-frames . nil))
    ("*R"
     (display-buffer-reuse-window display-buffer-in-side-window)
     (side . right)
     (slot . -1)
     (window-width . 0.5)
     (reusable-frames . nil))
    ("*Help"
     (display-buffer-reuse-window display-buffer-in-side-window)
     (side . right)
     (slot . 1)
     (window-width . 0.5)
     (reusable-frames . nil))))

;; Activate global mode for parenthesis matching:
(show-paren-mode)

;; Remove Flymake support:
(setq ess-use-flymake nil)
;; Replace it (globally) by Flycheck:
(use-package flycheck
  :init
  (global-flycheck-mode t))

; allows variable vewing
(use-package ess-view-data)
(use-package! ess
  :config
  (set-popup-rules!
    '(("^\\*R:*\\*$" :side right :size 0.5 :ttl nil)))
  (setq ess-R-font-lock-keywords
        '((ess-R-fl-keyword:keywords . t)
          (ess-R-fl-keyword:constants . t)
          (ess-R-fl-keyword:modifiers . t)
          (ess-R-fl-keyword:fun-defs . t)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-R-fl-keyword:%op% . t)
          (ess-fl-keyword:fun-calls . t)
          (ess-fl-keyword:numbers . t)
          (ess-fl-keyword:operators . t)
          (ess-fl-keyword:delimiters . t)
          (ess-fl-keyword:= . t)
          (ess-R-fl-keyword:F&T . t)))
  (map! (:map (ess-mode-map inferior-ess-mode-map)
         :g ";" #'ess-insert-assign)))

; Rstudio like setup in ESS - interactive function so you can call with M-x
; https://www.reddit.com/r/emacs/comments/15ggow0/i_need_some_help_with_my_rstudio_layout_for_ess/
(defun my/rstudio-layout () ""
       (interactive)
       (add-to-list 'display-buffer-alist
                    '((derived-mode . ess-mode)
                      (display-buffer-reuse-window)
                      (side .  left)
                      (slot . -1)
                      (dedicated . t)
                      (tab-group . "rstudio-1")))


       (add-to-list 'display-buffer-alist
                    `("^\\*help\\[R\\]\\|^\\*xwidget-webkit"
                      (display-buffer-reuse-mode-window  display-buffer-in-side-window)
                      (mode . '(ess-help-mode xwidget-webkit-mode))
                      (side . right)
                      (slot . 1)
                      (window-width . 0.33)
                      (dedicated . nil)))


       (add-to-list 'display-buffer-alist
                    `("^\\*R.*\\*"
                      (display-buffer-reuse-mode-window display-buffer-at-bottom)
                      (mode . ess-mode)
                      (window-width . 0.5)
                      (dedicated . t)
                      (tab-group "rstudio-3")))

       (add-to-list 'display-buffer-alist
                    `("^\\*R dired\\*"
                      (display-buffer-reuse-mode-window display-buffer-in-side-window)
                      (mode . ess-rdired-mode)
                      (side . right)
                      (slot . -1)
                      (window-width . 0.33)
                      (dedicated . t)
                      (reusable-frames . nil)
                      (tab-group . "rstudio-2")))

       (let ((ess-startup-directory 'default-directory))
         (delete-other-windows)
         (ess-switch-to-ESS t)
         (ess-rdired)
         (ess-help "help")
         (tab-line-mode 1)
         (my-start-hdg)))
