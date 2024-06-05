; latex_workflow.el

; -----------------------------------------------------------
;                       REFERENCING
; -----------------------------------------------------------
; Bib files which have all of my references
(setq my/bib_files '("~/PhD/BIBTEX/PhD_betterbiblatex.bib"
                     "~/PhD/BIBTEX/PhD_betterbibtex.bib"))
                     ;"~/WORK/Consulting/SCYC/BIBTEX/SCYC_betterbiblatex.bib"))

; Org cite is the inbuilt ref management in emacs
(after! oc
  (setq org-cite-global-bibliography my/bib_files))

; Use for displaying things in citar library nicely
(use-package all-the-icons
  :if (display-graphic-p))

; -----------------------------------------------------------
;                      CITAR
; -----------------------------------------------------------
; Use the citar package for displaying reference library and inserting
; references into an org or latex document - works across both types
(use-package citar
  :custom
  (citar-bibliography my/bib_files)
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-file-note-extensions (list "org"))
  (citar-library-file-extensions (list "pdf" "jpg")
      citar-file-additional-files-separator "-")
  (citar-notes-paths '("~/org/notes/refs/PhD/"))
  ;; (citar-templates
  ;;     '((main . "${author editor:30%sn}     ${date year issued:4}     ${title:48}")
  ;;       (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords:*}")
  ;;       (preview . "${author editor:%etal} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
  ;;       (note . "Notes on ${author editor:%etal}, ${title}")))
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)
  :bind  (("C-c b" . citar-insert-citation)) ; this is the shortcut key for inserting citation
)

; -----------------------------------------------------------
;                     CITAR EMBARK
; -----------------------------------------------------------
; When using Embark, the Citar actions are generic, and work
; the same across org, markdown, and latex modes.
(use-package citar-embark
  :after citar embark
  :no-require
  :config (citar-embark-mode))

; -----------------------------------------------------------
;                      ORG ROAM
; -----------------------------------------------------------
; https://justin.abrah.ms/dotfiles/emacs.html
;; https://rgoswami.me/posts/org-note-workflow/
;; You dont need to do all setup here because you have doom emacs
 (use-package org-roam
   :custom
   (org-roam-directory (file-truename "~/org/notes/refs/PhD/"))
   :bind (("C-c n l" . org-roam-buffer-toggle)
          ("C-c n f" . org-roam-node-find)
          ("C-c n g" . org-roam-graph)
          ("C-c n i" . org-roam-node-insert)
          ("C-c n c" . org-roam-capture)
          ("C-c n j" . org-roam-dailies-capture-today))
   :config
   ;; If you're using a vertical completion framework, you might want a more informative completion interface
   (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
   (org-roam-db-autosync-mode)
   ;; If using org-roam-protocol
   (require 'org-roam-protocol)
)




(defadvice! riccardo/citar-file-trust-zotero (oldfun &rest r)
  "Leave Zotero-generated file paths alone, especially zotero://..."
  :around '(citar-file-open citar-file--find-files-in-dirs)
  (cl-letf (((symbol-function 'file-exists-p) #'always)
            ((symbol-function 'expand-file-name) (lambda (first &rest _) first)))
    (apply oldfun r)))

(after! citar
  (add-to-list 'citar-file-open-functions '("pdf" . citar-file-open-external)))

; -----------------------------------------------------------
;                      ORG ROAM BIBTEX
; -----------------------------------------------------------
(use-package org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq orb-preformat-keywords
      '("citekey" "title" "url" "author-or-editor" "keywords" "file" "date")
      orb-process-file-keyword t
      orb-attached-file-extensions '("pdf")))
(org-roam-bibtex-mode)

(setq org-roam-node-display-template
      (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

(setq orb-preformat-keywords
      '("citekey" "title" "url" "author-or-editor" "keywords" "file")
      orb-process-file-keyword t
      orb-attached-file-extensions '("pdf"))

(after! org-roam-bibtex
(setq org-roam-capture-templates
      '(
        ;; PhD literature review
        ("p" "PhD LR" plain
         (file "/Users/chris/.config/doom/org-templates/PhD_literature_review.org")
         :target
         (file+head "~/org/notes/refs/PhD/${citar-citekey}.org" "#+title:${title}\n#+ROAM_KEY:${citar-citekey}\n#+filetags::PhD:"))

         ;; Biblipgraphy referennce
        ("r" "bibliography reference" plain "%?"
        :target
        (file+head "~/org/notes/refs/PhD/${citekey}.org" "#+title:${title}\n")
        :unnarrowed t))
        )
(setq orb-preformat-keywords
      '("citekey" "title" "url" "author-or-editor" "keywords" "file")
      orb-process-file-keyword t
      orb-attached-file-extensions '("pdf"))
)

(use-package citar-org-roam
  :after (citar org-roam)
  :config (citar-org-roam-mode)
)

;(setq citar-org-roam-note-title-template "${citekey}")
(setq citar-org-roam-capture-template-key "p")

; CITAR DISPLAY CONFIGURATION
; See: https://github.com/emacs-citar/citar/wiki/Indicators
    (defvar citar-indicator-files-icons
      (citar-indicator-create
       :symbol (nerd-icons-faicon
                "nf-fa-file_o"
                :face 'nerd-icons-green
                :v-adjust -0.1)
       :function #'citar-has-files
       :padding "  " ; need this because the default padding is too low for these icons
       :tag "has:files"))
    (defvar citar-indicator-links-icons
      (citar-indicator-create
       :symbol (nerd-icons-faicon
                "nf-fa-link"
                :face 'nerd-icons-orange
                :v-adjust 0.01)
       :function #'citar-has-links
       :padding "  "
       :tag "has:links"))
    (defvar citar-indicator-notes-icons
      (citar-indicator-create
       :symbol (nerd-icons-codicon
                "nf-cod-note"
                :face 'nerd-icons-blue
                :v-adjust -0.3)
       :function #'citar-has-notes
       :padding "    "
       :tag "has:notes"))
    (defvar citar-indicator-cited-icons
      (citar-indicator-create
       :symbol (nerd-icons-faicon
                "nf-fa-circle_o"
                :face 'nerd-icon-green)
       :function #'citar-is-cited
       :padding "  "
       :tag "is:cited"))

(setq citar-indicators
  (list citar-indicator-files-icons
        citar-indicator-links-icons
        citar-indicator-notes-icons
        citar-indicator-cited-icons))

; -----------------------------------------------------------
;                       PDF TOOLS
; -----------------------------------------------------------
(use-package pdf-tools
  :config
  (pdf-tools-install)
  ;; This means that pdfs are fitted to width by default when you open them
  (setq-default pdf-view-display-size 'fit-width)
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

; -----------------------------------------------------------
;                       MARGINALIA
; -----------------------------------------------------------
; This program allows for descriptions in the minibuffer
(use-package marginalia
  :config
  (marginalia-mode 1))

(defmacro aif (cnd then else)
  "anaphoric if from paul graham's on lisp. bind the result of CND
to IT for use in the THEN and ELSE clauses"
  `(let ((it ,cnd))
     (if it ,then ,else)))

; -----------------------------------------------------------
;                       LATEX
; -----------------------------------------------------------
(use-package cdlatex)
(use-package latex-preview-pane)
(latex-preview-pane-enable)

; Remove preamble at start of document in org to latex
(setq org-latex-with-hyperref nil)

(defun my/latex-mode-hook()
;add advice for the function so that it saves everytime you compile the latex buffer
(advice-add #'Tex-command-master :before (lambda (&rest r)) (save-buffer))
(push (list 'output-pdf "PDF Tools") latex-preview-pane-mode); argument
)

(use-package auctex
  :defer t ; will not try to load auctex when you start up - you have to do it for this
  ; this is becasue there is not a package named latex and gets loads with other stuff
  :hook (LaTeX-mode . my/latex-mode-hook) ; mode funcion format
)

;(setq TeX-global-PDF-mode t)
(setq org-latex-pdf-process (list
                             "latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f"))

; -----------------------------------------------------------
;                      DEFT
; -----------------------------------------------------------
(use-package deft
  :config
  (setq deft-extensions '("org")
        deft-directory org-roam-directory
        deft-recursive t
        deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
        deft-use-filename-as-title t)
  :bind
  ("C-c n d" . deft))


