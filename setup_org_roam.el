;; Setup org roam
;;
;;
;; https://rgoswami.me/posts/org-note-workflow/
;; You dont need to do all setup here because you have doom emacs
(setq org-roam-directory "~/PhD/PAPERS/Notes/")

;; (use-package org-roam
;;   :custom
;;   (org-roam-directory (file-truename "/path/to/org-files/"))
;;   :bind (("C-c n l" . org-roam-buffer-toggle)
;;          ("C-c n f" . org-roam-node-find)
;;          ("C-c n g" . org-roam-graph)
;;          ("C-c n i" . org-roam-node-insert)
;;          ("C-c n c" . org-roam-capture)
;;          ;; Dailies
;;          ("C-c n j" . org-roam-dailies-capture-today))
;;   :config
;;   ;; If you're using a vertical completion framework, you might want a more informative completion interface
;;   (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
;;   (org-roam-db-autosync-mode)
;;   ;; If using org-roam-protocol
;;   (require 'org-roam-protocol))

                                        ; This get org working with org-roam
                                        ; ;; org-roam-bibtex stuff
(use-package! org-roam-bibtex
  :hook (org-roam-mode . org-roam-bibtex-mode))

(setq orb-preformat-keywords '("citekey" "author" "date"))
(setq org-roam-capture-templates
      '(
        ; CAPTURE TEMPLATES FOR NOTES
        ("p" "PhD lit review" plain (file "~.config/doom/org-templates/PhD_literature_review.org")
 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
 :unnarrowed t)

                ; CAPTURE TEMPLATES FOR NOTES
        ("g" "Gantt chart" plain (file "~.config/doom/org-templates/gantt.org")
 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
 :unnarrowed t)

        ("r" "ref" plain(function org-roam-capture--get-point)
          ""
         :file-name "${citekey}"
         :head "#+TITLE: ${citekey}: ${title}\n#+ROAM_KEY: ${ref}
- tags ::
- keywords :: ${keywords}

* Notes
:PROPERTIES:
:Custom_ID: ${citekey}
:URL: ${url}
:AUTHOR: ${author-or-editor}
:NOTER_DOCUMENT: ${file}
:NOTER_PAGE:
:END:")

        ))

; Use package deft
(use-package deft
  :config
  (setq deft-extensions '("org")
        deft-directory org-roam-directory
        deft-recursive t
        deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
        deft-use-filename-as-title t)
  :bind
  ("C-c n d" . deft))
