;;; init-packages.el --- Packages configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; Bootstraps package.el & use-package.

;;; Code:

;; Manage packages using `straight.el' & `use-package'
(setq straight-check-for-modifications '(find-when-checking)
      straight-vc-git-default-clone-depth 1
      straight-use-package-by-default t
      straight-base-dir user-emacs-directory)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" straight-base-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(straight-use-package 'diminish)

(setq use-package-compute-statistics t
      use-package-expand-minimally t)

(provide 'init-packages)

;;; init-packages.el ends here
