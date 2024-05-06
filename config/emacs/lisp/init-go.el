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
  (defun ngq/go-lsp-mode-hook ()
    (lsp-register-custom-settings '(("gopls.hints" ((assignVariableTypes . t)
                                                    (compositeLiteralFields . t)
                                                    (compositeLiteralTypes . t)
                                                    (constantValues . t)
                                                    (functionTypeParameters . t)
                                                    (parameterNames . t)
                                                    (rangeVariableTypes . t)))
                                    ("gopls.analyses" ((fieldalignment . t)
                                                       (nilness . t)
                                                       (unusedparams . t)
                                                       (unusedwrite . t)
                                                       (useany . t))))))
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
  (when (and (fboundp 'lsp-deferred) (executable-find "gopls"))
    (setq lsp-go-hover-kind "FullDocumentation"
          lsp-go-link-target "pkg.go.dev")
    (when (executable-find "gofumpt")
      (setq lsp-go-use-gofumpt t))
    (add-hook 'go-mode-hook #'lsp-deferred)
    (add-hook 'lsp-mode-hook #'ngq/go-lsp-mode-hook)))



(provide 'init-go)

;;; init-go.el ends here
