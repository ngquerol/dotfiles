;;; init-go.el --- Go configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; Configuration for Go programming.

;;; Code:

(use-package go-mode
  :preface (defun ngq/go-mode-hook ()
             (setq-local whitespace-style (remq 'tab-mark whitespace-style)))
  :mode ("\\.go\\'" . go-mode)
  :hook (go-mode . ngq/go-mode-hook)
  :config
  ;; Smartparens integration
  (with-eval-after-load 'smartparens
    (sp-local-pair 'go-mode "{" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair 'go-mode "(" nil :post-handlers '(("||\n[i]" "RET"))))

  ;; Projectile integration
  (with-eval-after-load 'projectile
    (add-to-list 'projectile-project-root-files "go.mod"))

  ;; LSP integration
  (when (executable-find "gopls")
    (setq-default lsp-go-hover-kind "FullDocumentation"
                  lsp-go-link-target "pkg.go.dev")
    (when (executable-find "gofumpt")
      (setq lsp-go-use-gofumpt t))
    (add-hook 'go-mode-hook #'lsp-deferred)))

(provide 'init-go)

;;; init-go.el ends here
