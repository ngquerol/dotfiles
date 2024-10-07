;;; init-org.el --- Org-mode configuration -*- lexical-binding: t -*-

;; Author: Nicolas Gaulard-Querol <nicolas.gquerol@gmail.com>

;;; Commentary:

;; Configuration for Org documents browsing and editing.

;;; Code:

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :preface
  (defun ngq/org-summary-todo (n-done n-not-done)
    "Update parent TODO entry state appropriately when
its subchildren are updated."
    (let ((org-log-done nil))
      (cond
       ((and (> n-not-done 0) (> n-done 0)) (org-todo "INPROGRESS"))
       ((zerop n-not-done) (org-todo "DONE"))
       (t (org-todo "TODO")))))
  :hook (org-babel-after-execute . org-redisplay-inline-images)
  :config
  ;; General
  (add-hook 'org-after-todo-statistics-hook #'ngq/org-summary-todo)

  (setq org-startup-indented t
        org-startup-with-inline-images t
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-pretty-entities t
        org-hide-emphasis-markers t
        org-footnote-auto-adjust t
        org-auto-align-tags nil
        org-tags-column 0
        org-special-ctrl-a/e t
        org-ellipsis "…"
        org-pretty-entities t
        org-log-done 'time
        org-log-into-drawer t
        org-todo-keywords '((sequence "TODO(t)"
                                      "WAITING(w@/!)"
                                      "INPROGRESS(i!)"
                                      "|"
                                      "CANCELLED(c@/!)"
                                      "DONE(d)")))

  ;; Source blocks
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (C . t)
     (python . t)
     (js . t)
     (emacs-lisp . t)
     (dot . t)
     (gnuplot . t))))

(use-package ob
  :after org
  :ensure nil
  :config
  (setq-default org-confirm-babel-evaluate nil
                org-babel-noweb-error-all-langs t
                org-babel-js-function-wrapper
                "console.log(require('util')
                        .inspect(function() {
                          %s
                        }(), { depth: 100 }))")

  (setcdr (assq :noweb org-babel-default-header-args) "yes"))

(use-package ox-html
  :after org
  :ensure nil
  :config
  (setq-default org-html-doctype "html5"
                org-html-html5-fancy t
                org-html-head-include-default-style nil
                org-html-head-include-scripts nil
                org-html-table-caption-above nil
                org-html-divs '((preamble "aside" "preamble")
                                (content "main" "content")
                                (postamble "footer" "postamble"))
                org-html-checkbox-type nil
                org-html-metadata-timestamp-format "%Y-%m-%d %H:%M:%S"
                org-html-head (concat "<style type=\"text/css\">"
                                      (ngq/read-config-file "etc/org/style.css")
                                      "</style>")
                org-html-postamble (ngq/read-config-file "etc/org/postamble.html")
                org-html-format-drawer-function
                (lambda (name contents)
                  (format "<details><summary>%s</summary>%s</details>"
                          (capitalize name) contents))
                ;; use MathJax 3
                org-html-mathjax-options nil
                org-html-mathjax-template (ngq/read-config-file "etc/org/mathjax.html")))

;; External packages

;; It's the future
(use-package org-modern
  :hook (org-mode . org-modern-mode))

;; Prettify bullets
(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :config (setq org-superstar-remove-leading-stars t))

;; Use a variable pitch font for body text
(use-package org-variable-pitch
  :hook (org-mode . org-variable-pitch-minor-mode)
  :config
  (setq org-variable-pitch-fixed-font (face-attribute 'default :family))
  (let ((default-font-height (face-attribute 'default :height)))
    (set-face-attribute 'org-variable-pitch-face nil :height default-font-height)
    (set-face-attribute 'variable-pitch nil :height (+ default-font-height 10))))

;; Provides completion-at-point for code blocks
(use-package corg
  :after org
  :ensure (:host github :repo "isamert/corg.el"))

;; *Org*anize and send HTTP requests.
(use-package verb
  :after org
  :hook (verb-post-response . read-only-mode)
  :bind (:map verb-command-map ("C-l" . verb-util-show-log))
  :config
  (setq verb-json-use-mode #'js-ts-mode)

  (keymap-set org-mode-map "C-c C-r" verb-command-map)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((verb . t)))

  (add-to-list 'display-buffer-alist
               '("\\*HTTP Response [0-9]+\\*\\|\\*HTTP Headers\\*"
                 nil
                 (window-parameters (mode-line-format . none))
                 (window-height . 0.33)
                 (reusable-frames . visible)
                 (body-function . select-window))))

(provide 'init-org)

;;; init-org.el ends here
