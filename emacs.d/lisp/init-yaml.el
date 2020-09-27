;;; init-yaml.el --- YAML editing configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; Configuration for YAML files edition.

;;; Code:

;; YAML files
(use-package yaml-mode
  :mode ("\\.y(a)?ml\\'" "\\.clang-format\\'")
  :preface (defun ngq/yaml-mode-hook () (auto-fill-mode -1))
  :config (add-hook 'yaml-mode-hook #'ngq/yaml-mode-hook))

(provide 'init-yaml)

;;; init-yaml.el ends here
