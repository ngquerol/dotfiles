;;; init-python.el --- Python configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; Configuration for python programming.

;;; Code:

(when (executable-find "python3")
  (setq-default python-interpreter "python3"
                python-shell-interpreter "python3"))

(use-package python-mode
  :ensure nil
  :mode "\\.py\\'"
  :interpreter "python3"
  :init (when (and (fboundp #'eglot-ensure) (executable-find "pyright"))
          (add-hook 'python-base-mode-hook #'eglot-ensure))
  :config (when (executable-find "ipython")
            (setq-default python-shell-interpreter "ipython"
                          python-shell-interpreter-args "-i --simple-prompt"
                          python-shell-completion-native-enable nil)))

;; External packages

(use-package pet
  :after python
  :config (add-hook 'python-base-mode-hook #'pet-mode -10))

(use-package flymake-ruff
  :if (executable-find "ruff")
  :after (python flymake)
  :hook ((python-mode python-ts-mode) . flymake-ruff-load))

(use-package poetry
  :if (executable-find "poetry")
  :after python)

(provide 'init-python)

;;; init-python.el ends here
