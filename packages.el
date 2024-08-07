;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)
(package! exec-path-from-shell)
(package! dashboard)
(package! pdf-tools)


                                        ; Treemacs
(package! treemacs-icons-dired)
(package! treemacs-magit)
(package! treemacs-projectile)
(package! treemacs-nerd-icons)

                                        ; Tabs
(package! centaur-tabs)
(package! theme-changer)
(package! marginalia)
(package! consult)
(package! orderless)
(package! cape)
(package! corfu)
(package! which-key)
(package! powerthesaurus)
(package! aggressive-indent)
(package! undo-tree)

; R / ESS
;; (package! ess-plot
;;   :recipe (:host github :repo "DennieTeMolder/ess-plot"))
(package! company) ; for completion only
(package! essgd
  :recipe (:host github :repo "sje30/essgd"))

                                        ; Latex and bibtex
(package! unfill) ; expanding and contracting paragraphs
(package! citar)
(package! bibtex-completion)
(package! helm-bibtex)
(package! latex-preview-pane)
(package! ts) ; for elgantt gantt chart package
(package! s) ; for elgantt gantt chart package
(package! dash) ; for elgantt gantt chart package
(package! deft)
(package! all-the-icons)
(package! nerd-icons)
(package! base16-theme)
(package! tron-legacy-theme
  :recipe (:host github :repo "ianyepan/tron-legacy-emacs-theme"))
(package! olivetti)
                                        ; Org roam related packages
(package! org-roam)
(package! org-roam-bibtex
  :recipe (:host github :repo "org-roam/org-roam-bibtex"))
(package! org-noter
  :recipe (:host github :repo "org-noter/org-noter"))
(package! org-pdftools)
(package! org-noter-pdftools)

; Emac
(package! webkit
  :recipe (:host github :repo "akirakyle/emacs-webkit"))

(package! cdlatex)
(package! citar)
(package! citar-embark)
(package! citar-org-roam)
(package! svg-lib)
(package! catppuccin-theme)
(package! auctex)
(package! latex-preview-pane)

; Elfeed for reading in RSS feeds
(package! elfeed)
(package! elfeed-org)
;(package! elfeed-score)
(package! elfeed-curate)
(package! ess-view-data)
(package! vertico)
(package! flycheck)
(package! yasnippet)

; Markdown / R / quarto
(package! polymode)
(package! poly-markdown)
(package! markdown-mode)
;(package! quarto-mode)
(package! request)

(package! doom-nano-modeline
  :recipe (:host github
  :repo "ronisbr/doom-nano-modeline"))
