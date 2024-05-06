;;; init-lsp.el --- Language Server Protocol integration -*- lexical-binding: t -*-

;;; Commentary:

;; Configuration Emacs LSP clients, such as lsp-mode or eglot.
;;
;; Note: Language-specific LSP configuration is provided by the languages'
;; configuration files directly.

;;; Code:

(setq-default lsp-keymap-prefix "C-c l")

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((lsp-completion-mode . ngq/lsp-completion-mode-setup)
         (lsp-mode . ngq/lsp-mode-setup)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-signature-mode))
  :bind-keymap ("C-c l" . lsp-command-map)
  :bind (:map lsp-mode-map
              ([remap xref-find-definitions] . #'lsp-find-definition)
              ([remap xref-find-references] . #'lsp-find-references)
              ("C-c C-d" . #'lsp-describe-thing-at-point))
  :preface
  (defun ngq/lsp-completion-mode-setup ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  (defun ngq/lsp-mode-setup ()
    (add-hook 'before-save-hook #'lsp-format-buffer nil t)
    (add-hook 'before-save-hook #'lsp-organize-imports nil t))
  :config
  (setq lsp-completion-provider :none
        lsp-diagnostics-provider t
        lsp-headerline-breadcrumb-enable nil
        lsp-imenu-sort-methods '(position kind name)
        lsp-lens-enable nil
        lsp-inlay-hint-enable t
        lsp-modeline-code-action-fallback-icon "★"
        lsp-modeline-diagnostics-enable nil
        lsp-progress-prefix "⧗ "
        lsp-signature-auto-activate '(:after-completion :on-trigger-char)
        lsp-signature-doc-lines 1
        lsp-keep-workspace-alive nil))

(provide 'init-lsp)

;;; init-lsp.el ends here
