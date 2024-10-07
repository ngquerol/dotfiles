;;; init-lsp.el --- Language Server Protocol integration -*- lexical-binding: t -*-

;;; Commentary:

;; Configuration for Emacs LSP clients, such as lsp-mode or eglot.
;;
;; Note: Language-specific LSP configuration is provided by the languages'
;; configuration files directly.
;;
;; TODO: investigate svaante/dape

;;; Code:

(use-package eglot
  :functions (eglot-format-buffer
              eglot-rename
              eglot-code-actions
              eglot-format
              eglot-code-action-organize-imports
              eglot-inlay-hints-mode)
  :preface
  (defun ngq/eglot-format-buffer-before-save ()
    (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
  (defun ngq/eglot-organize-imports ()
    (call-interactively #'eglot-code-action-organize-imports))
  (defun ngq/eglot-organize-imports-before-save ()
    (add-hook 'before-save-hook #'ngq/eglot-organize-imports nil t))
  :bind (:map eglot-mode-map
              ("C-c C-r" . #'eglot-rename)
              ("C-c C-a" . #'eglot-code-actions)
              ("C-c C-f" . #'eglot-format)
              ("C-c C-d" . #'eldoc)
              ("C-c C-h" . #'eglot-inlay-hints-mode))
  :hook ((eglot-managed-mode . ngq/eglot-format-buffer-before-save)
         (eglot-managed-mode . ngq/eglot-organize-imports-before-save))
  :config (setq eglot-autoshutdown t
                eglot-events-buffer-config '(:size 0 :format full)
                eglot-stay-out-of '(company)))

(provide 'init-lsp)

;;; init-lsp.el ends here
