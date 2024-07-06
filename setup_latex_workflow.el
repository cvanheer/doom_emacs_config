; setup_latex_workflow.el
; This is my workflow for academia in emacs. The idea here is that I have my emacs setup for the following:
; 1. Writing papers in latex and org-mode. I have setup some .tex templates from classic thesis which is used in org-mode but I can also use the same templates in latex by opening a new template document.
; 2. Citations are done using citar - I prefer this package as it's more modern, but it's harder to link to things like org-roam (note taking) and org-noter (annotating pdfs)
; This workflow uses marginalia for useful notes about stuff in the margins, and vertico and embark for completion - these are again, more modern than helm/ivy etc but they require a bit more research in terms of getting the right configuration.
; PDFs - I have stored my PDFs in Zotero but after import I use something to get the PDFs renamed according to their citekey and then stored in ~/PhD/BIBTEX/{citekey}/{citekey.pdf}. org-noter links to the papers by having a property called :NOTER_DOCUMENT: which links to the pdf - so putting your cursor next to that and typing "org_noter" will bring up a note
;
; Remove preamble at start of document in org to latex
(setq org-latex-with-hyperref nil)

;; -----------------------------------------------------------
;;                       REFERENCING
;; -----------------------------------------------------------
                                        ;(add-to-list 'load-path "/Applications/Emacs.app/Contents/Resources/lisp/filenotify.el")

                                        ; Bib files which have all of my references
(setq my/bib_files '("~/PhD/BIBTEX/0.BIBFILES/PhD_betterbibtex.bib"
                     "~/WORK/Consulting/SCYC/BIBTEX/SCYC_betterbiblatex.bib"))
                                        ;"~/PhD/BIBTEX/PhD_betterbiblatex.bib"
(setq my/default_thesis_file "~/PhD/PROJECTS/3.BUCKET_AR2/3.doc/math_bridging_chapter.tex")

                                        ;(setq inhibit-startup-message t)
(require 'filenotify)

(defun reload-bibtex-file (bibtex-file)
  "Reloads bibtex file into the buffer."
  (when (file-exists-p bibtex-file)
    (find-file bibtex-file)
    (revert-buffer t t)
    (message "Reloaded BibTeX file: %s" bibtex-file)))

(defun handle-bibtex-file-change (event)
  "Callback function to handle BibTeX file change events."
  "Callback function to handle BibTeX file change events."
  ; NOTE: event from file-notify contains two elements, nth 0 is the path and nth 1 is the type of even
  ; i.e. changed, created, deleted, or attribute-change. This function receives the event input and
  ; then assigs these variable names of file and event-type respectively.
  (let ((event-type (nth 1 event))
        (bibtex-file (nth 2 event)))
    (message "Event type: %s, BibTeX file: %s" event-type bibtex-file)
    (when (eq event-type 'changed)
      (message "BibTeX file changed: %s" bibtex-file)
      (reload-bibtex-file bibtex-file))))

(defun bibtex-refresh ()
  "Watch BibTeX files for updating by Zotero and reload."
  (interactive)
  (dolist (bibtex-file my/bib_files)
    (when (file-exists-p bibtex-file)
      (file-notify-add-watch bibtex-file '(change) #'handle-bibtex-file-change)
      (message "Watching %s for changes" bibtex-file))))

;; Start watching the BibTeX files and refreshing where necessary
(bibtex-refresh)
; Notes: we are using file-notify-add-watch here.
; It has three arguments: FILE  FLAGS  CALLBACK
; - FILE: The path to the file or directory you want to watch. It can be either absolute or relative.
; - FLAGS: A list of symbols specifying the types of events to watch for. Common flags include:
; change / create / delete or attribute-change are the options here
; - CALLBACK: A function that gets called when one of the specified events occurs. The callback function receives a single argument, event, which is a list containing details about the event.

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
  (citar-library-file-extensions (list "pdf" "jpg"))
  (citar-notes-paths '("~/org/notes/refs/PhD/"))
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)
  :bind  (("C-c b" . citar-insert-citation)) ; this is the shortcut key for inserting citation
)

(after! citar
  (add-to-list 'citar-file-open-functions '("pdf" . citar-file-open)))

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



; -- Misc optional configurations
;Writing technical documents requires us to write in paragraphs, whereas org mode by default is intended to be used as an outliner,
;to get around this problem, setting up org-export to preserve line breaks is useful, there are two ways to achieve this,
;we can add \n:t to #+options: as a document specific setting, or we can set
(setq org-export-preserve-breaks t)

; -----------------------------------------------------------
;                    ORG ROAM BIBTEX
; -----------------------------------------------------------
; If you need more advice on template files have a look here:
; https://github.com/org-roam/org-roam-bibtex/issues/178

(use-package org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq orb-preformat-keywords
        '("citekey" "title" "url" "author" "keywords" "file" "date")
     ; orb-process-file-keyword "p"
      orb-attached-file-extensions '("pdf"))) ; only us pdf in file
(org-roam-bibtex-mode)

; -----------------------------------------------------------
;                      ORG NOTER
; -----------------------------------------------------------
; https://github.com/emacs-citar/citar/wiki/Notes-configuration
; To begin using org-noter you place your cursor next to the NOTER_DOCUMENT property in org-noter and then
(use-package org-noter
  :config
  (setq org-noter-notes-search-path '("/Users/chris/org/notes/refs/PhD/"))) ; path to citar-org-roam-subdir
;(pdf-loader-install)
;(org-noter-enable-org-roam-integration)

; If you highlight text this will put highlighted text in the .org roam file you are taking notes in
(setq org-noter-highlight-selected-text t)

; -----------------------------------------------------------
;                     CITAR ORG ROAM
; -----------------------------------------------------------
; HOW TO CONFIGURE NOTES: https://github.com/emacs-citar/citar/wiki/Notes-configuration
(use-package citar-org-roam
  :after (citar org-roam)
  :config
  (citar-org-roam-mode))
(setq citar-org-roam-capture-template-key "c")

; This allows pre-filling of some of the fields taking them from citar and giving them citar-field as a name
; so then we can use them in the org-roam note as :PROPERTY: drawers which are useful for later
(setq citar-org-roam-template-fields
        '((:citar-citekey "key")
          (:citar-title "title")
          (:citar-file "file")
          (:citar-author "author" "editor")
          (:citar-date "date" "year" "issued")
          (:citar-pages "pages")
          (:citar-type "=type="))   )

; This is to unfold the property drawer
(setq org-hide-block-startup nil
      org-startup-folded "fold")

(defun citar-add-org-noter-document-property(key &optional entry)
  "Set various properties PROPERTIES drawer when new Citar note is created."
  (interactive)
  (let* ((file-list-temp (list (citar--select-resource key :files t)))
         (file-path-temp (alist-get 'file file-list-temp))
         (cite-file (cdr (citar-get-field-with-value'(file) key)))
         (cite-author (cdr (citar-get-field-with-value'(author) key)))
         (cite-url (cdr (citar-get-field-with-value '(url) key))) )
    (org-set-property "NOTER_DOCUMENT" file-path-temp)
    (org-set-property "PAPER" cite-file) ; can also use org-insert-link but will insert down botton of page
    (org-set-property "Custom_ID" key)
    (org-set-property "AUTHOR" cite-author)
    (org-set-property "URL"    cite-url)
    (org-roam-ref-add (concat "@" key))
    (org-id-get-create)))

(advice-add 'citar-create-note :after #'citar-add-org-noter-document-property)

(add-to-list 'org-roam-capture-templates
     '("c" "citar literature note" plain (file "/Users/chris/.config/doom/org-templates/PhD_literature_review.org") ; OR "%?" instead of (file path_to_template)
       :target (file+head "%(expand-file-name org-roam-directory)/${citar-citekey}.org"
                              "#+title: Notes on: ${citar-title}\n#+subtitle: ${citar-author}, ${citar-date}\n#+ROAM_TAGS: :PhD:")
       :unnarrowed t))

(setq citar-org-roam-capture-template-key "c")

; CITAR DISPLAY CONFIGURATION See: https://github.com/emacs-citar/citar/wiki/Indicators
(defvar citar-indicator-files-icons
  (citar-indicator-create
   :symbol (all-the-icons-faicon
            "file-o"
            :face 'all-the-icons-green
            :v-adjust -0.1)
   :function #'citar-has-files
   :padding "  " ; need this because the default padding is too low for these icons
   :tag "has:files"))

(defvar citar-indicator-links-icons
  (citar-indicator-create
   :symbol (all-the-icons-octicon
            "link"
            :face 'all-the-icons-orange
            :v-adjust 0.01)
   :function #'citar-has-links
   :padding "  "
   :tag "has:links"))

(defvar citar-indicator-notes-icons
  (citar-indicator-create
   :symbol (all-the-icons-material
            "speaker_notes"
            :face 'all-the-icons-blue
            :v-adjust -0.3)
   :function #'citar-has-notes
   :padding "  "
   :tag "has:notes"))

(defvar citar-indicator-cited-icons
  (citar-indicator-create
   :symbol (all-the-icons-faicon
            "circle-o"
            :face 'all-the-icon-green)
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
  ;(pdf-tools-install)
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
;(setq TeX-global-PDF-mode t)
(setq org-latex-pdf-process (list "latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f"))
(setq TeX-engine 'luatex)

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
  :hook (LaTeX-mode-hook . my/latex-mode-hook) ; mode funcion format
  :config
   (setq TeX-engine 'xetex)
)

; -----------------------------------------------------------
;                      DEFT
; -----------------------------------------------------------
; Deft is a package which allows for nice viewing of all of your org-roam notes
; and it's a search engine of sorts
(use-package deft
  :config
  (setq deft-extensions '("org")
        deft-directory org-roam-directory
        deft-recursive t
        ;deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
        ; this controls what we see in the index for deft so that we don't see the title of the note as
        ; it is fully described by the filename which is the citekey
        deft-strip-summary-regexp
        (concat "\\("
	  "^:.+:.*\n" ; any line with a :SOMETHING:
	  "\\|^#\\+.*\n" ; anyline starting with a #+
	  "\\|^\\*.+.*\n" ; anyline where an asterisk starts the line
	  "\\)")
        deft-use-filename-as-title t)
  :bind
  ("C-c n d" . deft))

; https://github.com/jrblevin/deft/issues/75
(advice-add 'deft-parse-title :override
            (lambda (file contents)
              (if deft-use-filename-as-title
	          (deft-base-filename file)
	        (let* ((case-fold-search 't)
	               (begin (string-match "title: " contents))
	               (end-of-begin (match-end 0))
	               (end (string-match "\n" contents begin)))
	          (if begin
	              (substring contents end-of-begin end)
	            (format "%s" file))))))

;; -----------------------------------------------------------
;;                      DEFT
;; -----------------------------------------------------------
(use-package olivetti
  :after citar
  :hook ((text-mode . olivetti-mode)
         (latex-mode . olivetti-mode)
         (markdown-mode . olivetti-mode))
  :custom
  (olivetti-body-width 80)  ;; Set body width to 80 characters
  (olivetti-minimum-body-width 40)
  (olivetti-recall-visual-line-mode-entry-state t))

