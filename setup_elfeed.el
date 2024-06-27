;;------*-----*-----*-----*-----*-----*-----*-----*----*
;;
;; SETUP ELFEED
;; Description: this runs setup for an RSS reader
;; in emacs which I use to see new academic papers
;;
;;------*-----*-----*-----*-----*-----*-----*-----*----*
; ------------------------------------------------------------------
; ELFEED-ORG
; Description: my elfeed feeds are stored in an org file
; ------------------------------------------------------------------


;-------------------------------------------------------------------
; ELFEED
; Description: package which manages RSS feeds
; ------------------------------------------------------------------
(use-package! elfeed
  :config
  ; Where elfeed stores stuff - usually this would be in .elfeed but
  ; I want to change the location to somewhere more useful
  ;(setq elfeed-db-directory "~/.config/doom/elfeed")
  ;(add-hook! 'elfeed-search-mode-hook 'elfeed-update)
  (defun concatenate-authors (authors-list)
  "Given AUTHORS-LIST, list of plists; return string of all authors concatenated."
  (mapconcat
   (lambda (author) (plist-get author :name))
   authors-list ", "))

(defun my/search-print-fn (entry)
  "Print ENTRY to the buffer."
  (let* (
         (date (elfeed-search-format-date (elfeed-entry-date entry)))
         (feed (elfeed-entry-feed entry))
         (feed-title
          (when feed
            (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
         (title (or (elfeed-meta entry :title)
                    (elfeed-entry-title entry) ""))
         (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
         (entry-authors (concatenate-authors
                         (elfeed-meta entry :authors)))
         (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
         (tags-str (mapconcat
                    (lambda (s) (propertize s 'face
                                            'elfeed-search-tag-face))
                    tags ","))

         (title-width (-(window-width) 2 elfeed-search-trailing-width))
         (title-column (elfeed-format-column title (elfeed-clamp elfeed-search-title-min-width title-width 150) :left))
         (authors-width 20)
         (authors-column (elfeed-format-column entry-authors (elfeed-clamp elfeed-search-title-min-width authors-width 100) :right)))

    ; The columns that are relevant
    (insert (propertize date 'face 'elfeed-search-date-face) " ")

    (insert (propertize title-column
                        'face title-faces 'kbd-help title) " ")

    (insert (propertize authors-column
                        'face 'elfeed-search-date-face
                        'kbd-help entry-authors) " ")

    ;; (when feed-title
    ;;   (insert (propertize entry-authors
    ;; 'face 'elfeed-search-feed-face) " "))

    (when entry-authors
      (insert (propertize feed-title
                          'face 'elfeed-search-feed-face) " "))
    (when tags
    (insert "(" tags-str ")"))

    )
  )

; ELFEED COLOURS
(custom-set-faces

 '(elfeed-search-tag-face
   ((t :foreground "#f0f"
       ;:background "#f0f"
       :weight regular
       :underline nil))

    elfeed-search-date-face
       ((t :foreground "#f0f"
       ;:background "#f0f"
       :weight regular
       :underline nil))

    elfeed-search-feed-face
       ((t :foreground "#f0f"
       ;:background "#f0f"
       :weight regular
       :underline nil))

    elfeed-search-title-face
       ((t :foreground "#f0f"
       ;:background "#f0f"
       :weight regular
       :underline nil))
   )
 )
  (setq elfeed-show-entry-switch 'display-buffer)
  (setq elfeed-search-remain-on-entry t)
  (setq elfeed-search-print-entry-function #'my/search-print-fn)
  (setq elfeed-search-filter "@6-week-ago +unread"))

; Keyboard shortcuts for elfeed
(global-set-key (kbd "C-x w") 'elfeed)

; Auto updating elfeed every hour
(run-at-time nil (* 1 60 60) #'elfeed-update)

; We would also like to instruct Elfeed to fetch the papers whenever we open the Elfeed interface.
(add-hook! 'elfeed-search-mode-hook 'elfeed-update)

; -------------------------------------------------
; ELFEED ORG
; ----------------------------------------------
(use-package elfeed-org
  :config
;; Optionally specify a number of files containing elfeed
;; configuration. If not set then the location below is used.
;; Note: The customize interface is also supported.
 (setq rmh-elfeed-org-files (list "~/.config/doom/elfeed/elfeed.org"))
  :commands (elfeed-org)
  )
(require 'elfeed-org)

; Elfeed marking and unmarking
(defalias 'elfeed-toggle-star
  (elfeed-expose #'elfeed-search-toggle-all 'star))

(eval-after-load 'elfeed-search
  '(define-key elfeed-search-mode-map (kbd "s") 'elfeed-toggle-star))

;; face for starred articles
(defface elfeed-search-star-title-face
  '((t :foreground "#f77"))
    "Marks a starred Elfeed entry.")

(push '(star elfeed-search-star-title-face) elfeed-search-face-alist)

(elfeed-org)
(elfeed)

; -------------------------------------------------
; ELFEED SCORE
; -------------------------------------------------
;; (use-package! elfeed-score
;;   :after elfeed
;;   :config
;;   (elfeed-score-load-score-file "~/.config/doom/elfeed/elfeed_score.el") ; See the elfeed-score documentation for the score file syntax
;;   (elfeed-score-enable)
;;   (define-key elfeed-search-mode-map "=" elfeed-score-map))

(defun elfeed-mark-all-as-read ()
      (interactive)
      (mark-whole-buffer)
      (elfeed-search-untag-all-unread))
(define-key elfeed-search-mode-map (kbd "R") 'my/elfeed-mark-all-as-read)

(use-package elfeed-curate
  :ensure
  :bind (:map elfeed-search-mode-map
              ("a" . elfeed-curate-edit-entry-annoation)
              ("x" . elfeed-curate-export-entries))
        (:map elfeed-show-mode-map
              ("a" . elfeed-curate-edit-entry-annoation)
              ("m" . elfeed-curate-toggle-star)
              ("q" . kill-buffer-and-window)))


