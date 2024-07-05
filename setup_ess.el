;;; ess_setup.el -*- lexical-binding: t; -*-

; ------------------------------------------------------------------
; ESS - Emacs Speaks Statistics
; Description: this is what I use for coding in R
; ------------------------------------------------------------------

; GENERAL R RELATED SETTINGS for coding
(setq indent-guide-recursive t)
(setq inferior-R-args "--no-save --no-restore-data")

; Allows variable vewing
(use-package ess-view-data)

; ESS
(use-package! ess
  :config

  (require 'ess-site)
  (add-hook 'ess-mode-hook #'rainbow-delimiters-mode)
  (setq ess-style 'RStudio)  ;; Use RStudio's indentation style
  (setq ess-indent-offset 4)  ;; Indent code by 4 spaces
  (setq display-line-numbers-type 'relative)
  (setq ess-use-flymake nil)
  (setq ess-eval-visibly 'nowait)

  ;; (set-popup-rules!
  ;;   '(("^\\*R:*\\*$" :side right :size 0.5 :ttl nil)))

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
         :g ";" #'ess-insert-assign))

  ; DEFINE KEYS
  (define-key comint-mode-map [C-up] 'ess-readline)
  (define-key ess-mode-map (kbd "C-c C-r") 'ess-eval-region)
  (define-key ess-mode-map (kbd "C-c C-b") 'ess-eval-buffer)
  (define-key ess-mode-map (kbd "C-c C-n") 'ess-eval-line-and-step)
  (define-key ess-mode-map (kbd "<C-return>") 'ess-eval-paragraph-and-step)

    ;; Load company-mode only for ESS
  (use-package company
    :ensure t
    :config
    (add-hook 'ess-mode-hook 'company-mode)
    (add-hook 'inferior-ess-mode-hook 'company-mode)
    (add-hook 'R-mode-hook 'company-mode))

)

;; Activate global mode for parenthesis matching:
(show-paren-mode)

;; Replace flymake support in ESS with it (globally) by Flycheck:
(use-package flycheck
  :init
  (global-flycheck-mode t))

; Rstudio like setup in ESS - interactive function so you can call with M-x
;(use-package! ess-plot
  ;:defer t)

;https://www.reddit.com/r/emacs/comments/15ggow0/i_need_some_help_with_my_rstudio_layout_for_ess/
(defun my/rstudio-layout () ""
       (interactive)
       (add-to-list 'display-buffer-alist
                    `("^\\*help\\[R\\]"
                      (display-buffer-reuse-window display-buffer-in-side-window)
                      (side . right)
                      (slot . 1)
                      (window-width . 0.33)
                      (reusable-frames . nil)))
       (add-to-list 'display-buffer-alist
                    `("^\\*xwidget-webkit*."
                      (display-buffer-reuse-window display-buffer-in-side-window)
                      (side . right)
                      (slot . 1)
                      (window-width . 0.33)
                      (reusable-frames . nil)))
       (add-to-list 'display-buffer-alist
                    `("^\\*R.*\\*"
                      (display-buffer-reuse-window display-buffer-at-bottom)
                      (window-width . 0.5)
                      (dedicated . t)))
       (add-to-list 'display-buffer-alist
                    `("^\\*R dired\\*"
                      (display-buffer-reuse-window display-buffer-in-side-window)
                      (side . right)
                      (slot . -1)
                      (window-width . 0.33)
                      (reusable-frames . nil)))
       (let ((ess-startup-directory 'default-directory)
             (ess-ask-for-ess-directory nil))
         (delete-other-windows)
         (ess-switch-to-ESS t)
         (ess-help "help")
         (ess-rdired)))

(defun clear-shell ()
   (interactive)
   (let ((old-max comint-buffer-maximum-size))
     (setq comint-buffer-maximum-size 0)
     (comint-truncate-buffer)
     (setq comint-buffer-maximum-size old-max)))



(global-set-key  (kbd "\C-x c") 'clear-shell)

