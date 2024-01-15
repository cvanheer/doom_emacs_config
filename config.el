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
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Christina Van Heer"
      user-mail-address "christinavanheer.com")

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
; `load-theme' function. This is the default:
;(setq doom-theme 'doom-earl-grey)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;(setq org-directory "~/org/"

; Disable projectile tracking projects so you can add them manually using the known project function
(setq projectile-track-known-projects-automatically nil)

(setq doom-font (font-spec :family "Fantasque Sans Mono" :size 12)
      doom-variable-pitch-font (font-spec :family "Fantasque Sans Mono" :size 12))


(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

;; Projectile add main projects you are working on at the moment
(projectile-add-known-project "~/Teaching/Capstone_2023/lab_report_marking/lab_reports/")
(projectile-add-known-project "~/PhD/WRITING/")
(projectile-add-known-project "~/Logseq/journals/") ; location of logseq files and org mode agenda files
(projectile-add-known-project "~/PhD/PROJECTS/")
(projectile-add-known-project "~/PhD/PROJECTS/1.CONFLEARN/")
(projectile-add-known-project "~/PhD/PROJECTS/3.BUCKET_AR2/")
(projectile-add-known-project "~/PhD/PROJECTS/5.CIRCULAR_BUCKET/")
(projectile-add-known-project "~/PhD/PROJECTS/DIARY/")

; means that ESS does not hold up things while it is thinking
(setq ess-eval-visibly 'nowait)

; Vertico completion
(use-package vertico
  :init
  (vertico-mode)
  )

(add-hook 'window-setup-hook #'treemacs 'append)
(require 'nerd-icons)

;(setenv "PATH" (concat "/usr/texbin:/usr/local/bin:" (getenv "PATH")))
;(setq exec-path (append '("/usr/texbin" "/usr/local/bin") exec-path))
;(load "/Applications/Emacs.app/Contents/Resources/site-lisp/auctex.el" nil t t)
;(load "/Applications/Emacs.app/Contents/Resources/site-lisp/preview-latex.el" nil t t)

(add-to-list 'load-path "/Applications/Emacs.app/Contents/Resources/site-lisp")

; Org bullets
;load "~/.config/doom/org_bullets.el")
;(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

 ; Org babel for R coding
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)))
(add-to-list 'org-structure-template-alist '("r" "#+BEGIN_SRC R :exports both :results graphics :file ./fig_1?.png\n\n#+END_SRC" "<src lang=\"?\">\n\n</src>"))


(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))


; line wrapping - get rid of those little arrows
(add-hook 'text-mode-hook 'visual-line-mode)

(global-visual-line-mode t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))


;; ================= THEMES =======================
;; Note: themes are stored in /Applications/Emacs.app/Contents/Resources/etc
(add-to-list 'load-path "~/.config/doom/doom-nano-modeline/")
(add-to-list 'load-path "~/.config/doom/doom-nano-testing/")

 (use-package! doom-nano-modeline
  :config
  (doom-nano-modeline-mode 1)
  (global-hide-mode-line-mode 1))


(use-package doom-themes
  :ensure t 
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-nano-light)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))


(add-to-list 'load-path "~/.config/doom/doom-nano-testing-main/load-nano.el")


(load "~/.config/doom/org_agenda_settings.el")
(load "~/.config/doom/latex_workflow.el")
(load "~/.config/doom/dashboard.el")
(load "~/.config/doom/ess_setup.el")
(load "~/.config/doom/elfeed.el")
(load "~/.config/doom/ox_latex_templates.el")

;; Markdown mode
(use-package poly-markdown)
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
(define-hostmode poly-markdown-hostmode
  :mode 'markdown-mode)

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
