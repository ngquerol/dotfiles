;;; init-go.el --- Go configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; Configuration for Go programming.

;;; Code:

(use-package go-mode
  :preface
  (defun ngq/go-mode-hook ()
    (setq-local whitespace-style (remq 'tab-mark whitespace-style)))
  (defun ngq/lsp-go-install-save-hooks ()
    (setq whitespace-style '(tab-mark face trailing missing-newline-at-eof))
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  :mode ("\\.go\\'" . go-mode)
  :hook (go-mode . ngq/go-mode-hook)
  :config
  ;; Smartparens integration
  (with-eval-after-load 'smartparens
    (sp-local-pair 'go-mode "{" nil :post-handlers '(("||\n[i]" "RET"))))

  ;; Projectile integration
  (with-eval-after-load 'projectile
    (add-to-list 'projectile-project-root-files "go.mod"))
  (straight--installed-p "lsp-mode")

  ;; LSP integration
  (when (executable-find "gopls")
    (require 'lsp)
    (setq-default lsp-go-hover-kind "FullDocumentation"
                  lsp-go-link-target "pkg.go.dev")
    (lsp-register-custom-settings
     `(("gopls.completeUnimported" t t)
       ("gopls.staticcheck" ,(if (executable-find "staticcheck") t nil) t)
       ("gopls.gofumpt" ,(if (executable-find "gofumpt") t nil) t)))
    (add-hook 'go-mode-hook  #'ngq/lsp-go-install-save-hooks)
    (add-hook 'go-mode-hook #'lsp-deferred)))

(provide 'init-go)

;;; init-go.el ends here
