;;; init-shell.el --- Shell script configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; Configuration for shell script edition.

;;; Code:

;; Shell scripts
(use-package sh-script
  :straight (:type built-in)
  :mode ("\\.zsh\\'" . shell-script-mode))

;; External packages

(use-package flymake-shellcheck
  :after flymake
  :hook (sh-mode . flymake-shellcheck-load)
  :config (setq flymake-shellcheck-allow-external-files t))

(provide 'init-shell)

;;; init-shell.el ends here
