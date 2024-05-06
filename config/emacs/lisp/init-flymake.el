;;; init-flymake.el --- Flymake configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; On-the-fly syntax checking with flymake.

;;; Code:

(use-package flymake
  :straight (:type built-in)
  :hook ((prog-mode) . flymake-mode)
  :bind (:map flymake-mode-map
              ("C-c f d" . #'flymake-show-buffer-diagnostics)
              ("C-c f D" . #'flymake-show-project-diagnostics))
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (setq elisp-flymake-byte-compile-load-path
        (append elisp-flymake-byte-compile-load-path load-path)))

;; External packages

(use-package flymake-diagnostic-at-point
  :after flymake
  :hook (flymake-mode . flymake-diagnostic-at-point-mode)
  :config (setq flymake-diagnostic-at-point-display-diagnostic-function
                #'flymake-diagnostic-at-point-display-popup))

(use-package flymake-popon
  :disabled t
  :after flymake
  :hook (flymake-mode . flymake-popon-mode))

(provide 'init-flymake)

;;; init-flymake.el ends here
