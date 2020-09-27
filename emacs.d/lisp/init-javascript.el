;;; init-javascript.el --- Javascript configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; Configuration for Javascript programming.

;;; Code:

(use-package js-mode
  :ensure nil
  :mode "\\.jsx?\\'"
  :hook (js-mode . (lambda () (setq-default js-indent-level 2
                                            js-jsx-indent-level 2
                                            js-chain-indent t)))
  :config
  ;; LSP integration
  (when (and (package-installed-p 'lsp-mode)
             (executable-find "tsserver"))
    (add-hook 'js-mode-hook #'lsp-deferred)))

(use-package json-mode
  :mode "\\.json\\'"
  :config (setq-default json-reformat:indent-width 2))

(use-package js-comint
  :if (executable-find "node")
  :bind (:map js-mode-map :map js-comint-mode-map
              ("C-c C-z" . js-comint-start-or-switch-to-repl)
              ("C-c C-e" . js-comint-send-last-sexp)
              ("C-c C-r" . js-comint-send-region)
              ("C-c C-b" . js-comint-send-buffer)
              ("C-c C-l" . js-comint-load-file)
              ("C-c C-c" . js-comint-reset-repl)
              ("C-c C-q" . js-comint-quit-or-cancel))
  :config (setq js-comint-program-command (executable-find "node")))

(use-package add-node-modules-path
  :hook (js-mode . add-node-modules-path))

(provide 'init-javascript)

;;; init-javascript.el ends here
