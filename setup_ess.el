;;; ess_setup.el -*- lexical-binding: t; -*-

(use-package! ess
  :config
  (set-popup-rules!
    '(("^\\*R:*\\*$" :side right :size 0.5 :ttl nil)))
  (setq ess-R-font-lock-keywords
        '((ess-R-fl-keyword:keywords . t)
          (ess-R-fl-keyword:constants . t)
          (ess-R-fl-keyword:modifiers . t)
          (ess-R-fl-keyword:fun-defs . t)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-R-fl-keyword:%op% . t)
          (ess-fl-keyword:fun-calls . t)
          (ess-fl-keyword:numbers . t)
          (ess-fl-keyword:operators . t)
          (ess-fl-keyword:delimiters . t)
          (ess-fl-keyword:= . t)
          (ess-R-fl-keyword:F&T . t)))
  (map! (:map (ess-mode-map inferior-ess-mode-map)
         :g ";" #'ess-insert-assign)))


(set-popup-rule! "^\\*R[:\\*]"  :side 'bottom :width 0.5 :quit nil :ttl nil)
(set-popup-rule! "^\\*R dired\\*"     :side 'right :slot -1 :width 0.33 :quit nil :ttl 0)
(set-popup-rule! "^\\*help\\[R\\]"  :side 'right :slot 1 :width 0.33 :quit nil :ttl 0)

; https://discourse.doomemacs.org/t/how-to-configure-the-ess-windows-similar-to-the-rstudio-layout/2444/3
(map! :after ess-help
      :map ess-help-mode-map
      :n "q" nil
      :n "ESC" nil)

(map! :after ess-rdired
      :map ess-rdired-mode-map
      :n "q" nil
      :n "ESC" nil)
