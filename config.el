; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to fPtr
;; refresh your font settings. If Emacs still can't find your font, it  fPtranlikely
;; wasn't installed correctly. Font issues are rarely Doom issues!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Christina Van Heer"
      user-mail-address "christinavanheer.com")

; ------------------------------------------------------------------
; THEMES
; ------------------------------------------------------------------
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
; `load-theme' function. This is the default:
;(setq doom-theme 'doom-earl-grey)


;; (use-package! doom-nano-modeline
;;   :config
;;   (doom-nano-modeline-mode 1)
;;   (global-hide-mode-line-mode 1))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-pine t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  ;(doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))
(customize-set-variable 'doom-themes-treemacs-theme "doom-colors")


; Make comments more visible in foom-pine
(custom-set-faces!
  '(font-lock-comment-face :foreground "#d3d3d3"))


;; Note: themes are stored in /Applications/Emacs.app/Contents/Resources/etc
(add-to-list 'load-path "~/.config/doom/packages/doom-nano-modeline/")
(add-to-list 'load-path "~/.config/doom/packages/doom-nano-testing/")
; ------------------------------------------------------------------
; TRANSPARENT SCREEN - https://kristofferbalintona.me/posts/202206071000/
; ------------------------------------------------------------------
(set-frame-parameter (selected-frame) 'alpha 70) ;
(set-frame-parameter (selected-frame) 'alpha-background 100)   ;
(add-to-list 'default-frame-alist '(alpha . 70))
(add-to-list 'default-frame-alist '(alpha-background . 100))

; This function needs a bit of work - does not toggle in a loop
(defun my/toggle-transparency ()
   (interactive)
   (let ((alpha (frame-parameter nil 'alpha)))
     (set-frame-parameter
      nil 'alpha
      (if (eql (cond ((numberp alpha) alpha)
                     ((numberp (cdr alpha)) (cdr alpha))
                     ;; Also handle undocumented (<active> <inactive>) form.
                     ((numberp (cadr alpha)) (cadr alpha)))
               100)
          '(100. 70) '(100 . 100)
          )
      )
     )
   )
 (global-set-key (kbd "C-c t") 'my/toggle-transparency)

; ------------------------------------------------------------------
; TEXT / LINE WRAPPING
; ------------------------------------------------------------------
; Description: Gets rid of those little arrows that you click and the
; text unpacks
(add-hook 'text-mode-hook 'visual-line-mode)
(global-visual-line-mode t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

; Aggressive indenting
(use-package aggressive-indent
  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'css-mode-hook #'aggressive-indent-mode)
                                        ;(global-aggressive-indent-mode 1)


  ;; Define a function to enable aggressive-indent-mode
  (defun enable-aggressive-indent ()
    "Enable aggressive-indent-mode."
    (interactive)
    (aggressive-indent-mode 1))

  ; Activate aggressive-indent-mode in ess-r-mode (R mode)
  (add-hook 'ess-r-mode-hook 'enable-aggressive-indent)
  (add-hook 'ess-lisp-mode-hook 'enable-aggressive-indent)

  )

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

                                        ; ------------------------------------------------------------------
                                        ; UNDO TREE
                                        ; ------------------------------------------------------------------
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history nil))

                                        ; ------------------------------------------------------------------
                                        ; FONTS
                                        ; ------------------------------------------------------------------
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
                                        ;(setq org-directory "~/org/"

                                        ; Other fonts I like if you wanna spice things up:
                                        ; - JetBrains Mono
                                        ; - Isoveska
(setq doom-font (font-spec :family "Ubuntu Mono" :size 15)
      doom-variable-pitch-font (font-spec :family "Ubuntu Mono" :size 15))

; FUNCTION TO CHANGE FONT
; Note: Emacs specifies font size in 1/10 pt units. Therefore, a font size of 12 pt is represented as 120, which
; means that the function below does a bit of a convert thing
(defun my/change-font (font-family font-size)
  "Change the default font to FONT-FAMILY with FONT-SIZE."
  (interactive
   (list (completing-read "Font family: " (font-family-list))
         (read-number "Font size (pt): " 14)))  ; Prompt for size in pt
  (set-face-attribute 'default nil
                      :family font-family
                      :height (* font-size 10)))  ; Convert pt to Emacs units

(global-set-key (kbd "C-c f") 'my/change-font)
; ------------------------------------------------------------------
; TREEMACS & PROJECTILE - manages projects
; ------------------------------------------------------------------
(customize-set-variable 'doom-themes-treemacs-theme "doom-colors")

(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (setq treemacs-width 70))

; Not in use at the moment because it is annoying me
;(use-package treemacs-projectile
  ;:after (treemacs projectile))
;(add-hook 'window-setup-hook #'treemacs 'append)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-nerd-icons
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

; Disable projectile tracking projects so you can add them manually using the known project function
(setq projectile-track-known-projects-automatically nil)

;; Projectile add main projects you are working on at the moment
;(projectile-add-known-project "~/Teaching/Capstone_2023/lab_report_marking/lab_reports/")
(projectile-add-known-project "~/PhD/WRITING/")
(projectile-add-known-project "~/Logseq/journals/") ; location of logseq files and org mode agenda files
(projectile-add-known-project "~/PhD/PROJECTS/")
(projectile-add-known-project "~/PhD/PROJECTS/1.CONFLEARN/")
(projectile-add-known-project "~/PhD/PROJECTS/3.BUCKET_AR2/")
(projectile-add-known-project "~/PhD/PROJECTS/5.CIRCULAR_BUCKET/")
(projectile-add-known-project "~/PhD/PROJECTS/DIARY/")


; ------------------------------------------------------------------
; VERTICO COMPLETION / CONSULT COMPLETION
; ------------------------------------------------------------------

(use-package consult
  :bind (
         ("C-s" . consult-line)
         ("M-y" . consult-yank-pop)
         ("C-x b" . consult-buffer)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ("C-c i" . consult-imenu)
         ("C-c I" . consult-imenu-multi)
         ("C-c o" . consult-outline)
         ("C-c g" . consult-grep)
         ("C-c f" . consult-find)
         ("C-c l" . consult-locate)
         ("C-c L" . consult-locate-library)
         ("C-c b" . consult-bookmark)
         ("C-c m" . consult-man)
         ("C-c k" . consult-key-sequence)
         ("C-c c" . consult-completion-in-region)
         ("C-c e" . consult-compile-error)
         ("C-c r" . consult-ripgrep)
         ("C-c C-l" . consult-line)
         ("C-c C-b" . consult-buffer)
         )
  :config
  (setq consult-preview-key '(:debounce 0.5 any))
  (consult-customize consult-theme :preview-key '(:debounce 0.5 any))
  (setq consult-narrow-key "<")
  (setq consult-line-start-from-top t))


(use-package vertico
  :init
  (vertico-mode 1)

  :config
  (setq vertico-multiform-commands
    '((consult-line buffer)
      (consult-line-thing-at-point buffer)
      (consult-recent-file buffer)
      (consult-mode-command buffer)
      (consult-complex-command buffer)
      (embark-bindings buffer)
      (consult-locate buffer)
      (consult-project-buffer buffer)
      (consult-ripgrep buffer)
      (consult-fd buffer)))
  :bind (:map vertico-map
          ("C-k" . kill-whole-line)
          ("C-u" . kill-whole-line)
          ("C-o" . vertico-next-group)
          ("<tab>" . minibuffer-complete)
          ("M-<return>" . minibuffer-force-complete-and-exit)))

(setq vertico-multiform t)

;; Enable Corfu for inline completion
(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous`
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-quit-no-match 'separator) ;; Don't quit if no match
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous))
  :init
  (global-corfu-mode))

;; Optional: Combine Company and Corfu so you have a good experience of both
(setq company-idle-delay 0.2
      company-minimum-prefix-length 1
      company-tooltip-align-annotations t)

;; Use Cape for additional completion sources
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;; Enable Orderless completion style
(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Enable Which-Key for displaying possible keybindings
(use-package which-key
  :config
  (which-key-mode))

; ------------------------------------------------------------------
; RANDOM SHIT
; ------------------------------------------------------------------
(require 'nerd-icons)
(add-to-list 'load-path "/Applications/Emacs.app/Contents/Resources/site-lisp")

; Expand and contract paragraphs
(use-package! unfill
  :defer t
  :bind
  ("M-q" . unfill-toggle)
  ("A-q" . unfill-paragraph))

; ------------------------------------------------------------------
; ORG-BABEL
; ------------------------------------------------------------------
; Description: org-babel allows you to use other language code snippets inside an org file, for example, R or python
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)))
(add-to-list 'org-structure-template-alist
             '("r" "#+BEGIN_SRC R :exports both :results graphics :file ./fig_1?.png\n\n#+END_SRC" "<src lang=\"?\">\n\n</src>"))

; ------------------------------------------------------------------
; CENTUAR TABS
; ------------------------------------------------------------------
(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-set-icons t) ; To display themed icons from all the icons
  (setq centaur-tabs-height 20)
  ; Underline for the tab you are on at the moment
  (setq centaur-tabs-set-bar 'under)
  ;; Note: If you're not using Spacmeacs, in order for the underline to display
  ;; correctly you must add the following line:
  (setq x-underline-at-descent-line t)
  (setq centaur-tabs-close-button "x") ; string for close button
  (setq centaur-tabs-gray-out-icons 'buffer) ; To gray out icons for the unselected tabs:
  (setq centaur-tabs-set-bar 'left) ; coloured bar on left side
  (setq centaur-tabs-set-modified-marker t)
  (setq centaur-tabs-modified-marker "*")
  ; Integrate projectile with
  (centaur-tabs-group-by-projectile-project) ; you can call "(centaur-tabs-group-buffer-groups)" if you need to perforn this action
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))

; ------------------------------------------------------------------
; YASSNIPPETS
; ------------------------------------------------------------------
;; Add an org mode template for thesis writing using yassnippet
;; https://www.youtube.com/watch?v=W-bRZlseNm0
;; The key in each yas snippet tells you what you need to press e.g. "<t" followed by the TAB key to get the snippet of text
(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.config/doom/yas-snippets"))
  (yas-global-mode 1))

; ------------------------------------------------------------------
; EMACS SETUP
; ------------------------------------------------------------------
(load "~/.config/doom/setup_calendar.el")
(load "~/.config/doom/setup_org_agenda.el")
(load "~/.config/doom/setup_latex_workflow.el")
(load "~/.config/doom/setup_ox_latex_classes.el")
;(load "~/.config/doom/setup_dashboard.el")
(load "~/.config/doom/setup_ess.el")
(load "~/.config/doom/setup_elfeed.el")
(load "~/.config/doom/setup_markdown.el") ; this includes quarto mode

; ------------------------------------------------------------------
; WHAT TO OPEN WHEN EMACS STARTS UP
; ------------------------------------------------------------------
(defun startup-window-doom ()
  "This is a lisp function to setup windows on emacs doom"
(delete-other-windows)
(split-window-right)
(org-agenda "a" "a"))

(add-hook 'emacs-startup-hook 'startup-window-doom)

(setq-default inhibit-startup-buffer-menu t)

; ------------------------------------------------------------------
; POWER THESAURUS
; ------------------------------------------------------------------
(use-package powerthesaurus
  :bind
  ("M-`" . powerthesaurus-lookup-word-dwim))
