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

(use-package lsp-python-ms
  :preface
  (defun ngq/python-lsp-mode-hook ()
    (require 'lsp-python-ms)
    (lsp-deferred))

  (defun ngq/python-find-virtualenv ()
    "Find a virtualenv corresponding to the current buffer.
Return either a string or nil."
    (cl-block nil
      (when (and (executable-find "poetry")
                 (locate-dominating-file default-directory "pyproject.toml"))
        (with-temp-buffer
          ;; May create virtualenv, but whatever.
          (when (= 0 (call-process
                      "poetry" nil '(t nil) nil "run" "which" "python"))
            (goto-char (point-min))
            (when (looking-at "\\(.+\\)/bin/python\n")
              (let ((venv (match-string 1)))
                (when (file-directory-p venv)
                  (cl-return venv)))))))
      (when (and (executable-find "pipenv")
                 (locate-dominating-file default-directory "Pipfile"))
        (with-temp-buffer
          ;; May create virtualenv, but whatever.
          (when (= 0 (call-process "pipenv" nil '(t nil) nil "--venv"))
            (goto-char (point-min))
            (let ((venv (string-trim (buffer-string))))
              (when (file-directory-p venv)
                (cl-return venv))))))))
  :hook (python-mode . ngq/python-lsp-mode-hook)
  :config
  (setq lsp-python-ms-auto-install-server t)

  (advice-add #'lsp-python-ms--extra-init-params
              :around
              (lambda (func &rest args)
                "Automatically discover Pipenv and Poetry virtualenvs."
                (let ((lsp-python-ms-extra-paths lsp-python-ms-extra-paths)
                      (exec-path exec-path))
                  (when-let ((venv (ngq/python-find-virtualenv)))
                    (setq lsp-python-ms-extra-paths
                          (file-expand-wildcards
                           (expand-file-name
                            "lib/python*/site-packages" venv)))
                    (push (expand-file-name "bin" venv) exec-path))
                  (apply func args)))))

(use-package pipenv
  :if (executable-find "pipenv")
  :hook ((python-mode . pipenv-mode))
  :config (setq pipenv-projectile-after-switch-function
                #'pipenv-projectile-after-switch-extended))

(use-package python-black
  :if (executable-find "black")
  :hook ((python-mode . python-black-on-save-mode)))

(provide 'init-python)

;;; init-python.el ends here
