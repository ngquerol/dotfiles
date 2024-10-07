;;; init-javascript.el --- Javascript configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; Configuration for Javascript programming.

;;; Code:

(use-package js
  :ensure nil
  :mode "\\.jsx?\\'"
  :init (when (and (fboundp #'eglot-ensure) (executable-find "tsserver"))
          (add-hook 'js-base-mode-hook #'eglot-ensure))
  :config (setq-default js-indent-level 2
                        js-jsx-indent-level 2
                        js-chain-indent t))

(use-package js-comint
  :if (executable-find "node")
  :bind (:map js-base-mode-map
              ("C-c C-z" . js-comint-start-or-switch-to-repl)
              ("C-c C-e" . js-comint-send-last-sexp)
              ("C-c C-r" . js-comint-send-region)
              ("C-c C-b" . js-comint-send-buffer)
              ("C-c C-l" . js-comint-load-file)
              ("C-c C-c" . js-comint-reset-repl)
              ("C-c C-q" . js-comint-quit-or-cancel))
  :config (setq js-comint-program-command (executable-find "node")))

(use-package add-node-modules-path
  :hook (js-base-mode . add-node-modules-path))

(provide 'init-javascript)

;;; init-javascript.el ends here
