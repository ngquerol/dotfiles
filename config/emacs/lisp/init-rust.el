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
  (setq-default rustic-compile-backtrace "FULL")

  ;; Correctly link rustc errors in compilation buffers
  (add-to-list 'compilation-error-regexp-alist 'rust)
  (setf (alist-get 'rust compilation-error-regexp-alist-alist)
        '("^error[^:]*:[^\n]*\n *--> \\([^\n]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3))

  ;; Better Eldoc signature on hover
  (cl-defmethod lsp-clients-extract-signature-on-hover (contents (_server-id (eql rust-analyzer)))
    (-let* (((&hash "value") contents)
            (groups (--partition-by (s-blank? it) (s-lines (s-trim value))))
            (sig_group (if (s-equals? "```rust" (car (-third-item groups)))
                           (-third-item groups)
                         (car groups)))
            (sig (--> sig_group
                      (--drop-while (s-equals? "```rust" it) it)
                      (--take-while (not (s-equals? "```" it)) it)
                      (--map (s-trim it) it)
                      (s-join " " it))))
      (lsp--render-element (concat "```rust\n" sig "\n```"))))

  (with-eval-after-load 'lsp
    (add-hook 'rust-mode-hook #'lsp-deferred)
    (when (executable-find "cargo-clippy")
      (setq lsp-rust-analyzer-cargo-watch-command "clippy")))

  (with-eval-after-load 'tab-line
    (dolist (mode '(rustic-compilation-mode
                    rustic-popup-mode
                    rustic-cargo-test-mode
                    rustic-cargo-run-mode))
      (add-to-list 'tab-line-exclude-modes mode))))

(provide 'init-rust)

;;; init-rust.el ends here
