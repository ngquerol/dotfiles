;;; init-projectile.el --- Projectile configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; Configuration for project management as provided by `projectile-mode'.

;;; Code:

(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :hook (after-init . projectile-mode)
  :preface
  (defun ngq/projectile-mode-line-function ()
    "Same as the default mode-line indicator, except that it
stays hidden when not visiting a project buffer."
    (if (projectile-project-p)
        (format " Proj[%s:%s]"
                (projectile-project-name)
                (projectile-project-type))
      ""))
  :init (setq-default projectile-mode-line-prefix "")
  :config
  (setq projectile-find-dir-includes-top-level t
        projectile-use-git-grep t
        projectile-sort-order 'recently-active
        projectile-mode-line-function #'ngq/projectile-mode-line-function)

  (put 'projectile-project-compilation-cmd 'safe-local-variable #'stringp)
  (put 'projectile-project-run-cmd 'safe-local-variable #'stringp)
  (put 'projectile-project-test-cmd 'safe-local-variable #'stringp))

(provide 'init-projectile)

;;; init-projectile.el ends here
