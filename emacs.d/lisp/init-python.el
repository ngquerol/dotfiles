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
                  python-shell-interpreter-args "-i --simple-prompt"))

  ;; LSP integration
  (when (executable-find "pyls")
    (add-hook 'python-mode-hook #'lsp-deferred)))

(use-package python-black
  :if (executable-find "black")
  :hook (python-mode . python-black-on-save-mode)
  :diminish python-black-on-save-mode)

(use-package anaconda-mode
  :unless (executable-find "pyls")
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode))
  :diminish anaconda-mode)

(use-package company-anaconda
  :after anaconda-mode
  :preface (defun ngq/company-activate-anaconda ()
             (ngq/company-activate-local-backend 'company-anaconda))
  :hook (anaconda-mode . ngq/company-activate-anaconda))

(provide 'init-python)

;;; init-python.el ends here
