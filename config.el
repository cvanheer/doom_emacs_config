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

(setq doom-font (font-spec :family "Ubuntu Mono" :size 13)
      doom-variable-pitch-font (font-spec :family "Ubuntu Mono" :size 13))

; ------ TREEMACS ------
; Not in use at the moment because it is annoying me
(use-package treemacs-projectile
  :after (treemacs projectile))
;(add-hook 'window-setup-hook #'treemacs 'append)


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

; Vertico completion
(use-package vertico
  :init
  (vertico-mode)
)

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
  (load-theme 'doom-nova) ; tealy dark blue background and easy to read
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  ;(doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(add-to-list 'load-path "~/.config/doom/packages/doom-nano-testing-main/load-nano.el")

(load "~/.config/doom/setup_org_agenda.el")
(load "~/.config/doom/setup_latex_workflow.el")
(load "~/.config/doom/setup_ox_latex_classes.el")
(load "~/.config/doom/setup_dashboard.el")
(load "~/.config/doom/setup_ess.el")
(load "~/.config/doom/setup_elfeed.el")
(load "~/.config/doom/setup_markdown.el") ; this includes quarto mode
                                        ;
(setq max-lisp-eval-depth 160000)


;; Add an org mode template for thesis writing using yassnippet
;; https://www.youtube.com/watch?v=W-bRZlseNm0
;; The key in each yas snippet tells you what you need to press e.g. "<t" followed by the TAB key to get the snippet of text
(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.config/doom/yas-snippets"))
  (yas-global-mode 1))

; Remove preamble at start of document in org to latex
(setq org-latex-with-hyperref nil)


; run this everytime emacs starts up to get calendar events
;(call-process "/bin/bash" "~/.config/doom/calendar/get_ics_email")

; Create function to get calendar updating working
; This function runs get_ics_mail which is an executable script that imports my gmail calendar
; It then writes this to an ics file which is converted to an org file by ics2org which I have installed
; using the terminal. The ics2org package can be installed via npm see here:
; https://github.com/theophilusx/icsorg
; This can also be done manually using the terminal  and it has a shortcut in the bash profile of
; my mac
(defun my/get_cal()
  "An interactive function which runs scripts to get my gmail calendar to sync to my org agenda"
(interactive) ; allows you to use M-x
(shell-command "sh ~/.config/doom/calendar/get_ics_email")
(shell-command  "icsorg")
)

;(global-set-key (kbd "C-c c") "my/get_cal")

; Run the functionmy/get_cal ; can also use M-x get_cal as well
(my/get_cal)
