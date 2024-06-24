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

; ------------------------------------------------------------------
; THEMES
; ------------------------------------------------------------------
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
; `load-theme' function. This is the default:
;(setq doom-theme 'doom-earl-grey)

;; Note: themes are stored in /Applications/Emacs.app/Contents/Resources/etc
(add-to-list 'load-path "~/.config/doom/packages/doom-nano-modeline/")
(add-to-list 'load-path "~/.config/doom/packages/doom-nano-testing/")

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
  (load-theme 'doom-oceanic-next t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  ;(doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))


; ------------------------------------------------------------------
; TEXT / LINE WRAPPING
; ------------------------------------------------------------------
; Description: Gets rid of those little arrows that you click and the
; text unpacks
(add-hook 'text-mode-hook 'visual-line-mode)
(global-visual-line-mode t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;(setq org-directory "~/org/"


(setq doom-font (font-spec :family "Ubuntu Mono" :size 13)
      doom-variable-pitch-font (font-spec :family "Ubuntu Mono" :size 13))

; ------------------------------------------------------------------
; TREEMACS & PROJECTILE - manages projects
; ------------------------------------------------------------------
; Not in use at the moment because it is annoying me
(use-package treemacs-projectile
  :after (treemacs projectile))
;(add-hook 'window-setup-hook #'treemacs 'append)
;
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

; means that ESS does not hold up things while it is thinking
(setq ess-eval-visibly 'nowait)

; ------------------------------------------------------------------
; VERTICO COMPLETION
; ------------------------------------------------------------------
(use-package vertico
  :init
  (vertico-mode)
)

; ------------------------------------------------------------------
; RANDOM SHIT
; ------------------------------------------------------------------
(require 'nerd-icons)
(add-to-list 'load-path "/Applications/Emacs.app/Contents/Resources/site-lisp")

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
(load "~/.config/doom/setup_calendar.el") ; this includes quarto mod
(load "~/.config/doom/setup_org_agenda.el")
(load "~/.config/doom/setup_latex_workflow.el")
(load "~/.config/doom/setup_ox_latex_classes.el")
(load "~/.config/doom/setup_dashboard.el")
(load "~/.config/doom/setup_ess.el")
(load "~/.config/doom/setup_elfeed.el")
(load "~/.config/doom/setup_markdown.el") ; this includes quarto mod
