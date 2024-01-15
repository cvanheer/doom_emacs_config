;;; doom-nano-modeline-misc.el -- Miscellaneous functions for doom-nano-modeline -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ronan Arraes Jardim Chagas
;;
;; This package was highly based on N Λ N O modeline by Nicolas P. Rougier
;; <Nicolas.Rougier@inria.fr>.
;;
;;; License:
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;; Miscellaneous functions for doom-nano-modeline.

;;; Code:

(defun doom-nano-modeline-status ()
  "Return buffer status, one of 'read-only, 'modified or 'read-write."
  (with-current-buffer (or (buffer-base-buffer) (current-buffer))
    (let ((read-only   buffer-read-only)
         (modified    (and buffer-file-name (buffer-modified-p))))
      (cond (modified  'modified)
            (read-only 'read-only)
            (t         'read-write)))))

(defun doom-nano-modeline-buffer-name-vc-and-major-mode ()
  "Return the buffer name and the major mode."
  (let* ((buffer-name (cond
                       ((and (derived-mode-p 'org-mode)
                             (buffer-narrowed-p)
                             (buffer-base-buffer))
                        (format"%s [%s]" (buffer-base-buffer)
                               (org-link-display-format
                                (substring-no-properties (or (org-get-heading 'no-tags)
                                                             "-")))))
                       ((and (buffer-narrowed-p)
                             (buffer-base-buffer))
                        (format"%s [narrow]" (buffer-base-buffer)))
                       (t
                        (format-mode-line "%b"))))

         (buffer-modified (if (and buffer-file-name (buffer-modified-p)) "** " ""))

         (mode-name (format-mode-line mode-name))

         (vc-branch-name (doom-nano-modeline--get-vc-branch))

         (vc-branch (if vc-branch-name
                        `((vc-branch-name . nil))
                      nil)))

    `((,(concat buffer-modified buffer-name) . nil)
      (" " . nil)
      (,(if vc-branch-name (concat "[" vc-branch-name "]") "") . doom-nano-modeline-vc-branch-name-face)
      (,(if vc-branch-name " " "") . nil)
      (,(concat "(" mode-name ")") . doom-nano-modeline-major-mode-face))))

(defun doom-nano-modeline-org-clock-timer ()
  "Return the string with the current task time or nil if there is not an active clock."
  ;; We can check if there is an active time by performing the following
  ;; analysis:
  ;;
  ;;    - `org-mode-line-string' exists;
  ;;    - `org-mode-line-string' length is larger than 0; and
  ;;    - `org-mode-line-string' is a member of `global-mode-string'.
  ;;
  ;; The latter is necessary because Org does not empty `org-mode-line-string'
  ;; when the user clocks out.
  (when (and (boundp 'org-mode-line-string)
             (> (length org-mode-line-string) 0)
             (member 'org-mode-line-string global-mode-string))
    (let* ((str (substring-no-properties org-mode-line-string))
           (matches (string-match "\\(\\[[^]]*\\]\\)" str))
           (time-string (match-string 1 str)))
      (if time-string
          `((,time-string . doom-nano-modeline-org-clock-face))
        nil))))

(defun doom-nano-modeline-org-mode-buffer-name-and-major-mode ()
  "Return the buffer name and the major mode for Org buffers."
  (if (derived-mode-p 'org-mode)
      (let* ((org-title (doom-nano-modeline--get-org-title))
              (buffer-name (if org-title
                               org-title
                             (format-mode-line "%b")))
              (mode-name (format-mode-line mode-name))
              (buffer-modified (if (and buffer-file-name (buffer-modified-p)) "** " "")))

         `((,(concat buffer-modified buffer-name) . nil)
           (" " . nil)
           (,(concat "(" mode-name ")") . doom-nano-modeline-major-mode-face)))
    (doom-nano-modeline-default-mode)))

(defun doom-nano-modeline-cursor-position ()
  "Return the cursor position in the current buffer."
  `((,(format-mode-line "%l:%c") . doom-nano-modeline-cursor-position-face)))

(defun doom-nano-modeline-visual-selection-information ()
  "Return information about the visual selection in the current buffer."
  ;; We should check if we have an active mark or if we are in evil visual
  ;; state.
  (when (or mark-active
            (and (bound-and-true-p evil-local-mode)
                 (eq evil-state 'visual)))

    (cl-destructuring-bind (beg . end)
        ;; Obtain the beginning and end of the visual selection depending if we
        ;; are in evil mode or not.
        (if (and (bound-and-true-p evil-local-mode)
                 (eq evil-state 'visual))
            (cons evil-visual-beginning evil-visual-end)
          (cons (region-beginning) (region-end)))

      (let* ((lines (count-lines beg (min end (point-max))))
             (str (concat (cond (;; Visual block mode.
                                  (or (bound-and-true-p rectangle-mark-mode)
                                      (and (bound-and-true-p evil-visual-selection)
                                           (eq 'block evil-visual-selection)))
                                  (let ((cols (abs (- (save-excursion
                                                        (goto-char end)
                                                        (current-column))
                                                      (save-excursion
                                                        (goto-char beg)
                                                        (current-column))))))
                                    (format "[%dL x %dC]" lines cols)))
                                 ;; Visual line mode.
                                 ((and (bound-and-true-p evil-visual-selection)
                                       (eq evil-visual-selection 'line))
                                  (format "<%dL>" lines))
                                 ;; Visual mode.
                                 ((> lines 1)
                                  (format "<%dL, %dC>" lines (- end beg)))
                                 (t
                                  (format "<%dC>" (- end beg)))))))
        `((,str . doom-nano-modeline-visual-selection-information-face)
          (" " . nil))))))

(defun doom-nano-modeline--get-org-title ()
  "Get the `+title' property of an org file. If it does not exits, return nil."
  (let ((org-title (org-collect-keywords '("TITLE"))))
    (if org-title
        (car (cdr (car org-title)))
      nil)))

(defun doom-nano-modeline--get-vc-branch ()
  "Return current VC branch if any."
  (if vc-mode
      (let ((backend (vc-backend buffer-file-name)))
        (concat "#" (substring-no-properties vc-mode
                                 (+ (if (eq backend 'Hg) 2 3) 2))))  nil))

(defun doom-nano-modeline--space ()
  "Function to return a space for the modeline render function."
  `((" " . nil)))

(defun doom-nano-modeline--vterm-set-title-advice (title)
  "Advice to `vterm--set-title' to track the current directory in `default-dir'.

TITLE is the current value for the buffer title."
  (let ((dir (string-trim-left (concat (nth 1 (split-string title ":")) "/"))))
    (when (file-directory-p dir)
      (cd-absolute dir))))

(provide 'doom-nano-modeline-misc)

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not obsolete)
;; End:
;;; doom-nano-modeline-misc.el ends here
