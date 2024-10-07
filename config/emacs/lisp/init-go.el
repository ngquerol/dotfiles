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
  (defun ngq/go-eglot-hook ()
    (setq eglot-workspace-configuration
          `(:gopls
            (:gofumpt
             ,(if (executable-find "gofumpt")
                  t
                :json-false)
             :hints
             (:assignVariableTypes
              t
              :compositeLiteralFields t
              :compositeLiteralTypes t
              :constantValues t
              :functionTypeParameters t
              :parameterNames t
              :rangeVariableTypes t)
             :analyses
             (:fieldalignment
              t
              :nilness t
              :unusedparams t
              :unusedwrite t
              :useany t))))
    (eglot-ensure))
  :mode "\\.go\\'"
  :hook ((go-mode go-ts-mode go-mod-ts-mode go-dot-work-mode) . ngq/go-mode-hook)
  :init
  ;; Smartparens integration
  (with-eval-after-load 'smartparens
    (sp-local-pair 'go-mode "{" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair 'go-mode "(" nil :post-handlers '(("||\n[i]" "RET"))))

  ;; Projectile integration
  (with-eval-after-load 'projectile
    (add-to-list 'projectile-project-root-files "go.mod"))

  ;; LSP integration
  (when (and (fboundp #'eglot-ensure))
    (dolist (hook '(go-mode-hook go-ts-mode-hook))
      (add-hook hook #'ngq/go-eglot-hook))))

(use-package gotest
  :after go-mode
  :defines go-test-args
  :functions (go-test-current-test
              go-test-current-file
              go-test-current-project
              go-test-current-benchmark
              go-test-current-file-benchmarks
              go-test-current-project-benchmarks)
  :bind (:map go-mode-map
              :prefix "C-c C-t"
              :prefix-map gotest-map
              :prefix-docstring "Keymap used to launch go tests or benchmarks."
              ("t" . #'go-test-current-test)
              ("f" . #'go-test-current-file)
              ("p" . #'go-test-current-project)
              ("b" . #'go-test-current-benchmark)
              ("B" . #'go-test-current-file-benchmarks)
              ("P" . #'go-test-current-project-benchmarks))
  :config (setq go-test-args "-benchmem"))

(provide 'init-go)

;;; init-go.el ends here
