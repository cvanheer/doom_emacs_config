; ---- My elfeed setup -------

; Some code to use elfeed to get articles from RSS feeds
;

; Elfeed-org settings - https://cundy.me/post/elfeed/
; Elfeed org allows interfacing between elfeed and org mode so that your rss feeds
; are in an easier to read taggable format
(require 'elfeed-org)
(elfeed-org)
(setq rmh-elfeed-org-files (list "~/.config/doom/elfeed.org"))

(use-package! elfeed-score
  :after elfeed
  :config
  (elfeed-score-load-score-file "~/.config/doom/elfeed_score.el") ; See the elfeed-score documentation for the score file syntax
  (elfeed-score-enable)
  (define-key elfeed-search-mode-map "=" elfeed-score-map))

(global-set-key (kbd "C-x w") 'elfeed)
(run-at-time nil (* 1 60 60) #'elfeed-update) ; auto update every hour

; Then, set the default filter to show unread papers from 2 weeks ago. This is also customizable.
(setq elfeed-search-filter "@4-week-ago +unread")

;We would also like to instruct Elfeed to fetch the papers whenever we open the Elfeed interface:
(add-hook! 'elfeed-search-mode-hook 'elfeed-update)

;; Somewhere in your .emacs file
;(setq elfeed-feeds ;'("http://connect.biorxiv.org/biorxiv_xml.php?subject=neuroscience+animal_behavior_and_cognition"))

; Elfeed has two kinds of views: The search view which shows feed entries matching a search query, and the show view to read a specific entry. By default only one of these is active at a time, but a more useful split-pane setup is a few tweaks away:
; Stay in the search view and preview entries.
; There are only two pieces to this. The first is to specify the function we want Elfeed to use to display the buffer with the current entry. In this case write our own elfeed-display-buffer to set the height.

(setq elfeed-show-entry-switch #'elfeed-display-buffer)
; Custom function to display
(defun elfeed-display-buffer (buf &optional act)
  (pop-to-buffer buf)
  (set-window-text-height (get-buffer-window) (round (* 0.7 (frame-height)))))


; Easier tagging
(defun elfeed-tag-selection-as (mytag)
    "Returns a function that tags an elfeed entry or selection as
MYTAG"
    (lambda ()
      "Toggle a tag on an Elfeed search selection"
      (interactive)
      (elfeed-search-toggle-all mytag)))

(define-key elfeed-search-mode-map "l" (elfeed-tag-selection-as 'readlater))
(define-key elfeed-search-mode-map "d" (elfeed-tag-selection-as 'junk))


(defun concatenate-authors (authors-list)
    "Given AUTHORS-LIST, list of plists; return string of all authors concatenated."
    (if (> (length authors-list) 1)
        (format "%s et al." (plist-get (nth 0 authors-list) :name))
      (plist-get (nth 0 authors-list) :name)))

(defun my-search-print-fn (entry)
    "Print ENTRY to the buffer."
    (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
        (title (or (elfeed-meta entry :title)
                    (elfeed-entry-title entry) ""))
        (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
        (entry-authors (concatenate-authors
                        (elfeed-meta entry :authors)))
        (title-width (- (window-width) 10
                        elfeed-search-trailing-width))
        (title-column (elfeed-format-column
                        title 100
                        :left))
        (entry-score (elfeed-format-column (number-to-string (elfeed-score-scoring-get-score-from-entry entry)) 10 :left))
        (authors-column (elfeed-format-column entry-authors 40 :left)))
    (insert (propertize date 'face 'elfeed-search-date-face) " ")

    (insert (propertize title-column
                        'face title-faces 'kbd-help title) " ")
    (insert (propertize authors-column
                        'kbd-help entry-authors) " ")
    (insert entry-score " ")))

(setq elfeed-search-print-entry-function #'my-search-print-fn)
(setq elfeed-search-date-format '("%y-%m-%d" 10 :left))
(setq elfeed-search-title-max-width 110)
