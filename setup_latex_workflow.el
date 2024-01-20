;;; latex_workflow.el -*- lexical-binding: t; -*-

(setq my/bib_files '("~/PhD/BIBTEX/PhD_betterbiblatex.bib"
                     "~/PhD/BIBTEX/PhD_betterbibtex.bib"
                     "~/WORK/Consulting/SCYC/BIBTEX/SCYC_betterbiblatex.bib"))

(use-package citar
  :custom
  (citar-bibliography my/bib_files)
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)
  :bind  (("C-c b" . citar-insert-citation))
  )

; When using Embark, the Citar actions are generic, and work the same across org, markdown, and latex modes.
(use-package citar-embark
  :after citar embark
  :no-require
  :config (citar-embark-mode))

(use-package pdf-tools
  :config
  (pdf-tools-install)
  ;; This means that pdfs are fitted to width by default when you open them
  (setq-default pdf-view-display-size 'fit-width)
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights"))


(use-package marginalia
  :config
  (marginalia-mode 1))

(defmacro aif (cnd then else)
  "anaphoric if from paul graham's on lisp. bind the result of CND
to IT for use in the THEN and ELSE clauses"
  `(let ((it ,cnd))
     (if it ,then ,else)))

(use-package cdlatex)

(use-package auctex
  :defer t ; will not try to load auctex when you start up - you have to do it for this
                                        ; this is becasue there is not a package named latex and gets loads with other stuff
  :hook (LaTeX-mode . my/latex-mode-hook) ; mode funcion format
  )

(defun my/latex-mode-hook()
  ; add advice for the function so that it saves everytime you compile the latex buffer
  (advice-add #'Tex-command-master :before (lambda (&rest r)) (save-buffer))
  ; update the tex view program selection
  (push (list 'output-pdf "PDF Tools") latex-preview-pane-mode); argument
  )

; DO NOT USE xelatex as the pdf process try latexmk instead - not sure why this does not work
; but it means it does not export properly
;(setq org-latex-pdf-process '("xelatex -shell-escape %f"
                                          ;"xelatex -shell-escape %f"
                                         ; "xelatex -shell-escape %f"))

(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))
