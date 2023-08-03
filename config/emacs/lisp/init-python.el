;;; init-python.el --- Python configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; Configuration for python programming.
;;
;; TODO: investigate elpy

;;; Code:
(use-package python-mode
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python3" . python-mode)
  :config
  (when (executable-find "ipython")
    (setq-default python-shell-interpreter "ipython"
                  python-shell-interpreter-args "-i --simple-prompt")))

(use-package lsp-pyright
  :defer t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(use-package pyvenv
  :after python-mode
  :config
  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")))))

  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))

(use-package pipenv
  :if (executable-find "pipenv")
  :hook ((python-mode . pipenv-mode))
  :config (setq pipenv-projectile-after-switch-function
                #'pipenv-projectile-after-switch-extended))

(use-package poetry
  :if (executable-find "poetry")
  :hook ((python-mode . poetry-tracking-mode)))

(use-package python-black
  :if (executable-find "black")
  :hook ((python-mode . python-black-on-save-mode-enable-dwim)))

(provide 'init-python)

;;; init-python.el ends here
