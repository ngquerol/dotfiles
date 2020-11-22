;;; init-rust.el --- Rust configuration -*- lexical-binding: t -*-

;; Author: Nicolas Gaulard-Querol <nicolas.gquerol@gmail.com>

;;; Commentary:

;; Settings for Rust programming.

;;; Code:

(use-package rustic
  :mode ("\\.rs$" . rustic-mode)
  :bind (:map rustic-mode-map
	          ("M-\\" . format-rust)
	          ("C-c C-c" . rustic-cargo-build)
	          ("C-c C-f" . rustic-cargo-clean)
	          ("C-c C-r" . rustic-cargo-run)
	          ("C-c C-t" . rustic-cargo-test)
	          ("C-c C-y" . rustic-cargo-clippy))
  :config (setq-default rustic-compile-backtrace "FULL"
                        rustic-format-on-save t
                        lsp-rust-analyzer-display-chaining-hints t
                        lsp-rust-analyzer-display-parameter-hints t
                        lsp-rust-analyzer-server-display-inlay-hints t))

(provide 'init-rust)

;;; init-rust.el ends here
