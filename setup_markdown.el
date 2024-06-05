;;; setup_markdown.el
;;; -*- lexical-binding: t; -*-
;;;


;; Markdown mode
(use-package poly-markdown)

(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
(define-hostmode poly-markdown-hostmode
  :mode 'markdown-mode)


; Quarto mode
(use-package quarto-mode
  :mode (("\\.qmd" . poly-quarto-mode))
  )

;; Note that the following is not necessary to run quarto-mode in .qmd files! It's merely illustrating
;; how to associate different extensions to the mode.
;(add-to-list 'auto-mode-alist '("\\.qmd\\'" . poly-quarto-mode))

(require 'centaur-tabs)
(after! centaur-tabs
  (centaur-tabs-mode 1) ; run at startup
  ;(centaur-tabs-headline-match)
  (setq centaur-tabs-style "slant"
        centaur-tabs-height 32
        centaur-tabs-set-icons t
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-set-bar 'below
        centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker "*"
        ;centaur-tabs-change-fonts "Menlo" 120
        )
  )
