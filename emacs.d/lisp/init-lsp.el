;;; init-lsp.el --- Language Server Protocol integration -*- lexical-binding: t -*-

;;; Commentary:

;; Configuration Emacs LSP clients, such as lsp-mode or eglot.
;;
;; Note: Language-specific LSP configuration is provided by the languages'
;; configuration files directly.

;;; Code:

(setq-default lsp-keymap-prefix "C-c l")

(use-package lsp-mode
  :preface
  (defun ngq/enable-lsp-capf ()
    (setq-local completion-category-defaults
                (add-to-list 'completion-category-defaults
                             '(lsp-capf (styles basic)))
                completion-at-point-functions #'lsp-completion-at-point))
  :commands (lsp lsp-deferred)
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . ngq/enable-lsp-capf))
  :bind (:map lsp-mode-map
              ([remap xref-find-definitions] . #'lsp-find-definition)
              ([remap xref-find-references] . #'lsp-find-references)
              ("C-c C-d" . #'lsp-describe-thing-at-point))
  :config (setq-default lsp-keep-workspace-alive nil
                        lsp-modeline-code-actions-segments '(name)
                        lsp-imenu-sort-methods '(position kind name)))

(use-package lsp-ui
  :preface
  (defun lsp-ui--imenu-window-p (window)
    (let ((buffer (window-buffer window)))
      (string-equal "*lsp-ui-imenu*" (buffer-name buffer))))

  (defun lsp-ui--imenu-adjust-frame (window)
    (let* ((frame-char-width (frame-char-width))
           (window-width (+ lsp-ui-imenu-window-width
                            (/ (frame-parameter nil 'left-fringe)
                               frame-char-width)
                            (/ (frame-parameter nil 'right-fringe)
                               frame-char-width)))
           (width-adjustment (* (if window -1 1) window-width)))
      (set-frame-width nil (+ (frame-width) width-adjustment))))

  (defun ngq/lsp-ui-toggle-imenu-window ()
    (interactive)
    (let ((window (get-window-with-predicate #'lsp-ui--imenu-window-p)))
      (when (and (display-graphic-p)
                 (not (frame-parameter nil 'fullscreen)))
        (lsp-ui--imenu-adjust-frame window))
      (if window
          (kill-buffer (window-buffer window))
        (lsp-ui-imenu)
        (set-window-fringes window 0 0))))
  :hook (lsp-mode . (lambda () (when (boundp 'flycheck-pos-tip-mode)
                                 (flycheck-pos-tip-mode -1))))
  :bind (:map lsp-ui-mode-map
              ("C-c l m" . ngq/lsp-ui-toggle-imenu-window)
              :map lsp-ui-imenu-mode-map
              ([remap lsp-ui-imenu--kill] . #'ngq/lsp-ui-toggle-imenu-window))
  :config (setq lsp-ui-doc-enable nil
                lsp-ui-sideline-enable nil
                lsp-ui-imenu-enable t
                lsp-ui-imenu-window-width 25))

(use-package lsp-treemacs
  :after lsp)

(provide 'init-lsp)

;;; init-lsp.el ends here
