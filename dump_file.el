;;; dump_file.el -*- lexical-binding: t; -*-


(use-package auctex
  :defer t
  :hook (LaTeX-mode . my/latex-mode-hook)
  :bind (:map LaTeX-mode-map
              ("C-c v" . my/vertico-bibtex))
  )


(defun my/vertico-bibtex (&optional arg)
  "insert a bibtex citation at point using `completing-read`. if
ARG is non-nil, refresh the bibtex-completion cache"
  (interactive "P")
  (when arg
    (bibtex-completion-clear-cache))
  (bibtex-completion-init)
  (let* ((candidates (bibtex-completion-candidates))
         (map (my/vertico-bibtex--build-map candidates))
         (keys (mapcar #'car map))
         (completion-extra-properties
          (list
           :annotation-function
           (lambda (key)
             (let ((obj (alist-get key map nil nil #'string=)))
               (marginalia--fields
                ((plist-get obj :author) :width 30 :truncate 0.5 :face 'marginalia-documentation)
                ((plist-get obj :title) :width 50 :truncate 0.5 :face 'marginalia-string)
                ((plist-get obj :journal) :width 30 :truncate 0.5 :face 'marginalia-value))))))
         (selection (completing-read "Insert citation: " keys)))
    (when selection
      (insert selection))))

(defun my/bibtex-generate-autokey (&rest r)
  (let* ((names (bibtex-autokey-get-names))
         (year (bibtex-autokey-get-year))
         (title (bibtex-autokey-get-title)))
    (capitalize (format "%s%s" names year))))
(advice-add #'bibtex-generate-autokey :around #'my/bibtex-generate-autokey)

(defun my/vertico-bibtex--get-field (key candidate)
  "return the field matching KEY in CANDIDATE"
  (alist-get key (cdr candidate) nil nil #'string=))

(defun vertico-bibtex--maybe-truncate (field len)
  (if field
      (substring field 0 (min len (length field)))
    field))

(defun my/vertico-bibtex--build-map (candidates)
  (mapcar
   (lambda (cand)
     (let* ((key (my/vertico-bibtex--get-field "=key=" cand))
            (title (vertico-bibtex--maybe-truncate(my/vertico-bibtex--get-field "title" cand) 35))
            (author (vertico-bibtex--maybe-truncate
                     (aif (my/vertico-bibtex--get-field "author" cand)
                          (string-replace " and " ", " it) it)
                     40))
            (book (my/vertico-bibtex--get-field "booktitle" cand))
            (journal (my/vertico-bibtex--get-field "journal" cand)))
       `(,key . (:title ,title :author ,author :journal ,(or journal book)))))
   candidates))



(use-package citar
  :general
  :bind
   (("C-c c" . citar-insert-citation)
   ("C-c r" . citar-insert-reference)
   ("C-c o" . citar-open-notes))
  :after oc
  :custom
  (citar-bibliography my/bibtex_files)
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-at-point-function 'embark-act)
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)
  )
