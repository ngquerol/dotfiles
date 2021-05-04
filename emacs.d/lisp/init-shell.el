;;; init-shell.el --- Shell script configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; Configuration for shell script edition.

;;; Code:

;; Shell scripts
(use-package sh-script
  :straight nil
  :mode ("\\.zsh\\'" . shell-script-mode))

(provide 'init-shell)

;;; init-shell.el ends here
