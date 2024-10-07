;;; init-rust.el --- Rust configuration -*- lexical-binding: t -*-

;; Author: Nicolas Gaulard-Querol <nicolas.gquerol@gmail.com>

;;; Commentary:

;; Settings for Rust programming.

;;; Code:

(use-package rustic
  :preface
  (defun ngq/rust-eglot-hook ()
    (setq eglot-workspace-configuration
          `(:rust-analyzer (
                            :check (:command "clippy")
                            :completion (
                                         :autoimport (:enable t)
                                         :postfix (:enable t))
                            :debug (:openDebugPane t)
                            :inlayHints (
                                         :chainingHints.enable: t
                                         :closureStyle: "rust_analyzer"
                                         :closureReturnTypeHints.enable: "always"
                                         :expressionAdjustmentHints.enable: "never"
                                         :implicitDrops.enable: :json-false
                                         :lifetimeElisionHints.enable: "always"
                                         :lifetimeElisionHints.useParameterNames: t
                                         :renderColons: :json-false)
                            :lens (:location "above_whole_item")
                            :procMacro (:enable t))))
    (eglot-ensure))
  :hook (rust-ts-mode . rustic-mode)
  :bind (:map rustic-mode-map
              ("C-c C-l" . rustic-format-buffer)
              ("C-c C-h" . eglot-inlay-hints-mode)
              ("C-c C-c" . rustic-cargo-build)
              ("C-c C-f" . rustic-cargo-clean)
              ("C-c C-r" . rustic-cargo-run)
              ("C-c C-t" . rustic-cargo-test)
              ("C-c C-y" . rustic-cargo-clippy))
  :config
  (setq-default rustic-compile-backtrace "FULL"
                rustic-lsp-client 'eglot)

  ;; Correctly link rustc errors in compilation buffers
  (add-to-list 'compilation-error-regexp-alist 'rust)
  (setf (alist-get 'rust compilation-error-regexp-alist-alist)
        '("^error[^:]*:[^\n]*\n *--> \\([^\n]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3))

  (when (and (fboundp #'eglot-ensure) (executable-find "rust-analyzer"))
    (add-hook 'rust-ts-mode-hook #'ngq/rust-eglot-hook))

  (with-eval-after-load 'tab-line
    (dolist (mode '(rustic-compilation-mode
                    rustic-popup-mode
                    rustic-cargo-test-mode
                    rustic-cargo-run-mode))
      (add-to-list 'tab-line-exclude-modes mode))))

(provide 'init-rust)

;;; init-rust.el ends here
