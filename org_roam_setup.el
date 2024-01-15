;; Setup org roam
;;
;;
;; https://rgoswami.me/posts/org-note-workflow/
;; You dont need to do all setup here because you have doom emacs
(setq org-roam-directory "~/PhD/PAPERS/Notes/")

; This get org working with org-roam
; ;; org-roam-bibtex stuff
  (use-package! org-roam-bibtex
    :hook (org-roam-mode . org-roam-bibtex-mode))

  (setq orb-preformat-keywords
        '("citekey" "title" "url" "author-or-editor" "keywords" "file")
        orb-process-file-keyword t
        orb-file-field-extensions '("pdf"))

  (setq orb-templates
        '(("r" "ref" plain(function org-roam-capture--get-point)
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
:END:")))


(use-package deft
  :config
  (setq deft-extensions '("org")
        deft-directory org-roam-directory
        deft-recursive t
        deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
        deft-use-filename-as-title t)
  :bind
  ("C-c n d" . deft))
