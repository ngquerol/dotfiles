;;; init-vcs.el --- VCS-related configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; VCS integration.

;;; Code:

(use-package vc-hooks
  :straight (:type built-in)
  :config (setq vc-follow-symlinks t
                find-file-visit-truename t))

;; External packages

(use-package magit
  :commands (magit-init magit-status)
  :bind ("C-c g" . magit-status)
  :config
  (setq-default magit-restore-window-configuration t
                magit-diff-refine-hunk 'all
                magit-save-some-buffers 'dontask
                magit-no-message '("Turning on magit-auto-revert-mode..."))

  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-unpulled-from-upstream
                          'replace)

  (with-eval-after-load 'tab-line
    (dolist (mode '(magit-status-mode magit-log-mode))
      (add-to-list 'tab-line-exclude-modes mode))))

(use-package abridge-diff
  :after magit
  :config (abridge-diff-mode t))

(use-package diff-hl
  :disabled t
  :hook ((dired-mode . (lambda ()
                         (setq-local diff-hl-side 'left)
                         (diff-hl-dired-mode)))
         (tty-setup . diff-hl-margin-mode)
         (diff-hl-mode . diff-hl-flydiff-mode)
         (after-init . global-diff-hl-mode))
  :config
  (setq diff-hl-draw-borders nil
        diff-hl-side 'right)

  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

(provide 'init-vcs)

;;; init-vcs.el ends here
