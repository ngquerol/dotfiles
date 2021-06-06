;;; init-flycheck.el --- Flycheck configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; On-the-fly syntax checking with flycheck.

;;; Code:

(use-package flycheck
  :hook ((prog-mode LaTeX-mode) . flycheck-mode)
  :config (setq flycheck-emacs-lisp-load-path 'inherit))

(provide 'init-flycheck)

;;; init-flycheck.el ends here
