;;; init-scheme.el --- Scheme configuration -*- lexical-binding: t -*-

;; Author: Nicolas Gaulard-Querol <nicolas.gquerol@gmail.com>

;;; Commentary:

;; Configuration for Scheme programming.

;;; Code:

(use-package scheme
  :ensure nil
  :mode (("\\.scm\\'" . scheme-mode)
         ("\\.ss\\'" . scheme-mode)
         ("\\.sls\\'" . scheme-mode)
         ("\\.sps\\'" . scheme-mode)
         ("\\.sld\\'" . scheme-mode))
  :config
  (with-eval-after-load 'aggressive-indent
    (add-hook 'scheme-mode-hook #'aggressive-indent-mode))
  (with-eval-after-load 'smartparens
    (add-hook 'scheme-mode-hook #'turn-on-smartparens-strict-mode)))

(use-package geiser
  :hook (scheme-mode . geiser-mode)
  :commands run-geiser
  :defines (geiser-active-implementations
            geiser-default-implementation
            geiser-mode-start-repl-p
            geiser-debug-show-debug-p
            geiser-gambit-binary
            geiser-guile-binary
            geiser-guile-manual-lookup-nodes
            geiser-guile-debug-show-bt-p)
  :config
  (setq geiser-active-implementations '(guile)
        geiser-default-implementation 'guile
        geiser-mode-start-repl-p t)

  ;; Guile-specific configuration
  (setq geiser-guile-debug-show-bt-p t)

  (with-eval-after-load 'smartparens
    (add-hook 'geiser-repl-mode-hook #'smartparens-strict-mode)))

(provide 'init-scheme)

;;; init-scheme.el ends here
