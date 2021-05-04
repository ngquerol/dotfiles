;;; init-rust.el --- Rust configuration -*- lexical-binding: t -*-

;; Author: Nicolas Gaulard-Querol <nicolas.gquerol@gmail.com>

;;; Commentary:

;; Settings for Rust programming.

;;; Code:

(use-package rustic
  :mode ("\\.rs$" . rustic-mode)
  :bind (:map rustic-mode-map
              ("C-c C-l" . rustic-format-buffer)
              ("C-c C-h" . lsp-rust-analyzer-inlay-hints-mode)
              ("C-c C-c" . rustic-cargo-build)
              ("C-c C-f" . rustic-cargo-clean)
              ("C-c C-r" . rustic-cargo-run)
              ("C-c C-t" . rustic-cargo-test)
              ("C-c C-y" . rustic-cargo-clippy))
  :config
  (setq-default rustic-compile-backtrace "FULL"
                rustic-format-trigger 'on-save)

  ;; Correctly link rustc errors in compilation buffers
  (add-to-list 'compilation-error-regexp-alist 'rust)
  (setf (alist-get 'rust compilation-error-regexp-alist-alist)
        '("^error[^:]*:[^\n]*\n *--> \\([^\n]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3))

  ;; Smartparens integration
  (with-eval-after-load 'smartparens
    (sp-local-pair 'rustic-mode "{" nil :post-handlers '(("||\n[i]" "RET")))))

(provide 'init-rust)

;;; init-rust.el ends here
