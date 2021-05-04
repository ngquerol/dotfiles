;;; init-web.el --- Web (HTML/CSS/Templates) configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; Configuration for web markup/template edition.

;;; Code:

(use-package web-mode
  :mode ("\\.html?\\'" "\\.php\\'")
  :bind (:map web-mode-map
              ("M-<up>" . web-mode-element-previous)
              ("M-<down>" . web-mode-element-next)
              ("M-S-<up>" .  web-mode-element-parent)
              ("M-<end>" . web-mode-navigate)
              ("C-c c" . web-mode-comment-or-uncomment )
              ("C-c k" . web-mode-element-kill )
              ("C-c v" . web-mode-element-vanish)
              ("C-c w" . web-mode-element-wrap)
              ("C-c r" . web-mode-element-rename))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-css-colorization t
        web-mode-enable-auto-expanding t
        web-mode-enable-auto-pairing t
        web-mode-enable-auto-quoting t
        web-mode-enable-auto-opening t
        web-mode-enable-auto-closing t
        web-mode-enable-part-face nil
        web-mode-enable-comment-interpolation t
        web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight t)

  ;; LSP integration
  (when (executable-find "html-languageserver")
    (add-hook 'web-mode-hook #'lsp-deferred)))

(use-package css-mode
  :straight nil
  :mode "\\.css\\'"
  :config
  (setq css-indent-offset 2)

  ;; LSP integration
  (when (executable-find "css-languageserver")
    (add-hook 'css-mode-hook #'lsp-deferred)))

(provide 'init-web)

;;; init-web.el ends here
