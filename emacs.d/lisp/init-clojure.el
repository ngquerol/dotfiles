;;; init-clojure.el --- Clojure(script) configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:

;; Configuration for Clojure/Clojurescript programming.

;;; Code:

(use-package clojure-mode
  :mode (("\\.cljc\\'" . clojure-mode)
         ("\\.clj\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.edn\\'" . clojure-mode))
  :config
  (setq clojure-align-forms-automatically t)

  ;; LSP integration
  (when (executable-find "clojure-lsp")
    (dolist (hook '(clojure-mode-hook clojurec-mode-hook clojurescript-mode-hook))
      (add-hook hook #'lsp-deferred)))

  (with-eval-after-load 'smartparens
    (dolist (hook '(clojure-mode-hook clojurec-mode-hook clojurescript-mode-hook))
      (add-hook hook #'turn-on-smartparens-strict-mode))))

(use-package clojure-mode-extra-font-locking
  :after clojure-mode)

(use-package cider
  :hook ((cider-mode cider-repl-mode) . cider-company-enable-fuzzy-completion)
  :bind (:map cider-repl-mode-map ("C-c C-l" . cider-repl-clear-buffer))
  :config
  (setq cider-repl-use-pretty-printing t
        cider-repl-use-clojure-font-lock t
        cider-repl-result-prefix ";; => "
        cider-font-lock-dynamically '(macro core function var)
        cider-overlays-use-font-lock t
        cider-repl-display-help-banner nil
        cider-prompt-for-symbol nil
        nrepl-hide-special-buffers t)

  (with-eval-after-load 'smartparens
    (add-hook 'cider-repl-mode-hook #'smartparens-strict-mode))

  (with-eval-after-load 'tab-line
    (add-to-list 'tab-line-exclude-modes 'cider-repl-mode)))

(provide 'init-clojure)

;;; init-clojure.el ends here
