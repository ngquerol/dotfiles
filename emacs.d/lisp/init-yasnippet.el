;;; init-yasnippet.el --- Yasnippet configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; Snippets!

;;; Code:

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :bind (:map yas-minor-mode-map
               ("C-c & n" . yas-new-snippet)
               ("C-c & s" . yas-insert-snippet)
               ("C-c & v" . yas-visit-snippet-file)
               ("C-c & r" . yas-reload-all)
               ("C-c & &" . yas-describe-tables))
  :init (setq yas-wrap-around-region t
              yas-verbosity 1)
  :diminish yas-minor-mode)

(use-package yasnippet-snippets
  :after yasnippet)

(provide 'init-yasnippet)

;;; init-yasnippet.el ends here
