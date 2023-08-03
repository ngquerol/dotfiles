;;; init-tex.el --- LaTeX configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; Configuration for LaTeX editing.

;;; Code:

(use-package auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook (LaTeX-mode . visual-line-mode)
  :config
  (setq TeX-parse-self t
        TeX-auto-save t
        TeX-clean-confirm nil)

  (when (eq system-type 'darwin)
    TeX-view-program-selection '((output-pdf "Preview"))
    TeX-view-program-list '(("Preview" "open -a Preview.app %o"))))

(provide 'init-tex)

;;; init-tex.el ends here
