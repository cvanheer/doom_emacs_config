; ess_setup.el
; ------------------------------------------------------------------
; ESS - Emacs Speaks Statistics
; Description: this is what I use for coding in R
; ------------------------------------------------------------------

; GENERAL R RELATED SETTINGS for coding
(setq indent-guide-recursive t)
(setq inferior-R-args "--no-save --no-restore-data")

; ESS
(use-package! ess
  :config
  (require 'ess-site)
  (setq ess-history-directory nil)
  (setq ess-style 'RStudio)  ;; Use RStudio's indentation style
  (setq ess-indent-offset 4)  ;; Indent code by 4 spaces
  (setq display-line-numbers-type 'relative)
  (setq ess-use-flymake nil)
  (setq ess-eval-visibly 'nowait)

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
  ;; Define keys
  (define-key comint-mode-map [C-up] 'ess-readline)
  (define-key ess-mode-map (kbd "C-c C-r") 'ess-eval-region)
  (define-key ess-mode-map (kbd "C-c C-b") 'ess-eval-buffer)
  (define-key ess-mode-map (kbd "C-c C-n") 'ess-eval-line-and-step)
  (define-key ess-mode-map (kbd "<C-return>") 'ess-eval-paragraph-and-step)
  (define-key ess-mode-map (kbd "C-c C-r") 'ess-eval-region-or-function-or-paragraph-and-step)
  (define-key ess-mode-map (kbd "C-c C-e") 'ess-eval-buffer-from-beg-to-here)
  (define-key ess-mode-map (kbd "<up>") 'comint-previous-input)
  (define-key ess-mode-map (kbd "<down>") 'comint-next-input)

  ;; Load company-mode only for ESS
  (use-package company
    :hook
    (ess-mode .  company-mode)
    (R-mode . company-mode)
    )
  )

(use-package! essgd
  :after ess
  :config
  ;; Keybindings for essgd plot viewer
  (define-key essgd-mode-map (kbd "p") 'essgd-previous-plot)
  (define-key essgd-mode-map (kbd "n") 'essgd-next-plot))

;; --- PLOTTING views / data views
(use-package ess-view-data
  :after ess
  :hook
  (ess-mode . ess-view-data-mode))

; Xwidget
(use-package xwidget
  :after ess)

;;https://www.reddit.com/r/emacs/comments/15ggow0/i_need_some_help_with_my_rstudio_layout_for_ess/
(require 'projectile)


+--------------------+--------------------+
|                    |                    |
|    R Script        |   R Console        |
|                    |--------------------|
|                    |  Other ESS Buffers |
+--------------------+--------------------+

(defun my/rstudio-layout ()
  "Layout which looks similar to my preferred setup in Rstudio"
       (interactive)
        (projectile-switch-project)  ; Prompt to select a Projectile project
  (let* ((project-root (projectile-project-root))
         (files (projectile-current-project-files))
         (default-file (completing-read "Choose a file: " files nil t))) ; Prompt to choose a file
    (find-file (expand-file-name default-file project-root)))

       (add-to-list 'display-buffer-alist
                     '("^\\*ess-help.*\\|^\\*essgd:.*\\|^\\*help\\[R\\]"
                      (display-buffer-reuse-window display-buffer-in-side-window)
                      (side . right)
                      (slot . 2)
                      (window-height . 0.25)
                      (window-width . 0.40)
                      (reusable-frames . nil)))

       (add-to-list 'display-buffer-alist
                    `("^\\*R.*\\*"
                      (display-buffer-reuse-window display-buffer-in-side-window)
                      (side . right)
                      (slot . 1)
                (window-height . 0.75)
                      (window-width . 0.40)
                      (dedicated . t)
                      ))

       (add-to-list 'display-buffer-alist
                    `("^\\*R dired\\*"
                      (display-buffer-reuse-window display-buffer-in-side-window)
                      (side . right)
                      (slot . 2)
                      (window-width . 0.40)
                      (reusable-frames . nil)))

   ; Optimise aesthetics for coding in R
   (set-frame-parameter (selected-frame) 'alpha (list 100 0))
   (load-theme 'doom-solarized-light)
   (global-display-line-numbers-mode t)

       (let ((ess-startup-directory 'default-directory)
             (ess-ask-for-ess-directory nil))

         (delete-other-windows)
         (ess-switch-to-ESS t) ; major mode
         (ess-help "help")
         (ess-rdired)
         )

       )


;; Optional: Define a keybinding for this function
(global-set-key (kbd "C-c r") 'my/rstudio-layout)


;; ------------------------------------------------------------------
;; PLOTTING
;; ------------------------------------------------------------------
;; PLotting
;; Install instrictions:
;; You need this in yourremotes::install_github("nx10/httpgd")


;; Activate global mode for parenthesis matching:
(show-paren-mode)

;; Replace flymake support in ESS with it (globally) by Flycheck:
(use-package flycheck
  :init
  (global-flycheck-mode t))

;; Rstudio like setup in ESS - interactive function so you can call with M-x
;;(use-package! ess-plot
 ;:defer t)

(defun clear-shell ()
  (interactive)
  (let ((old-max comint-buffer-maximum-size))
    (setq comint-buffer-maximum-size 0)
    (comint-truncate-buffer)
    (setq comint-buffer-maximum-size old-max)))

(global-set-key  (kbd "\C-x c") 'clear-shell)
