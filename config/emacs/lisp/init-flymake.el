;;; init-flymake.el --- Flymake configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; On-the-fly syntax checking with flymake.

;;; Code:

(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("C-c e b" . #'flymake-show-buffer-diagnostics)
              ("C-c e p" . #'flymake-show-project-diagnostics))
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (setq elisp-flymake-byte-compile-load-path
        (append elisp-flymake-byte-compile-load-path load-path)
        flymake-show-diagnostics-at-end-of-line nil))

;; External packages

(use-package flymake-diagnostic-at-point
  :after flymake
  :hook (flymake-mode . flymake-diagnostic-at-point-mode)
  :config (setq flymake-diagnostic-at-point-display-diagnostic-function
                #'flymake-diagnostic-at-point-display-popup))

(provide 'init-flymake)

;;; init-flymake.el ends here
