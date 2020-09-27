;;; init-clojure.el --- Clojure(script) configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:

;; Configuration for Clojure/Clojurescript programming.

;;; Code:

(use-package clojure-mode
  :mode ("\\.edn\\'" "\\.clj\\'"
         ("\\.cljs\\'" . clojurescript-mode))
  :config (setq clojure-align-forms-automatically t)

  (with-eval-after-load 'smartparens
    (add-hook 'clojure-mode-hook #'turn-on-smartparens-strict-mode)))

(use-package clojure-mode-extra-font-locking
  :after clojure-mode)

(use-package clj-refactor
  :hook (clojure-mode . clj-refactor-mode)
  :config (setq cljr-warn-on-eval nil)
  :diminish clj-refactor-mode)

(use-package cljr-ivy
  :after ivy clj-refactor
  :bind (:map clojure-mode-map
              ("C-c r" . cljr-ivy)
              :map clojurescript-mode-map
              ("C-c r" . cljr-ivy)))

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

(use-package flycheck-clj-kondo
  :after flycheck clojure-mode
  :config (require 'flycheck-clj-kondo))

(provide 'init-clojure)

;;; init-clojure.el ends here
