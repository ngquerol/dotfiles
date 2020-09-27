;;; init-go.el --- Go configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; Configuration for Go programming.

;;; Code:

(use-package go-mode
  :preface
  (defun ngq/lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  :mode ("\\.go\\'" . go-mode)
  :config

  ;; LSP integration
  (when (and (package-installed-p 'lsp-mode)
             (executable-find "gopls"))
    (setq-default lsp-go-hover-kind "FullDocumentation")
    (add-hook 'go-mode-hook #'ngq/lsp-go-install-save-hooks)
    (add-hook 'go-mode-hook #'lsp-deferred)))

(provide 'init-go)

;;; init-go.el ends here
