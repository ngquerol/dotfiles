;;; init-dired.el --- Dired configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; Configure Dired and friends.

;;; Code:

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :hook ((dired-mode . auto-revert-mode)
         (dired-mode . toggle-truncate-lines))
  :bind (:map ctl-x-map ("C-j" . dired-jump))
  :config
  (require 'ls-lisp) ; don't rely on system-provided ls
  (setq-default ls-lisp-use-insert-directory-program nil
                ls-lisp-dirs-first t
                ls-lisp-use-localized-time-format t
                ls-lisp-format-time-list '("%Y-%m-%d %H:%M"
                                           "%Y-%m-%d      ")
                dired-use-ls-dired nil
                dired-listing-switches "-aAlGh")

  (setq-default dired-dwim-target t
                dired-hide-details-hide-symlink-targets nil
                dired-auto-revert-buffer t
                dired-isearch-filenames 'dwim
                dired-recursive-deletes 'top
                dired-recursive-copies 'always)

  (when (boundp 'dired-kill-when-opening-new-dired-buffer)
    (setq dired-kill-when-opening-new-dired-buffer t)))

(use-package dired-x
  :after dired
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :bind (:map dired-mode-map ("C-x M-o" . dired-omit-mode))
  :config (setq dired-clean-confirm-killing-deleted-buffers nil
                dired-omit-files (concat dired-omit-files "\\|^\\..+$")))

(use-package wdired
  :after dired
  :ensure nil
  :bind (:map dired-mode-map ("C-c C-e" . wdired-change-to-wdired-mode))
  :config (setq wdired-use-dired-vertical-movement 'sometimes
                wdired-confirm-overwrite t
                wdired-use-interactive-rename t))

;; External packages

(use-package dired-single
  :after dired
  :if (not (boundp 'dired-kill-when-opening-new-dired-buffer))
  :preface
  (defun ngq/dired-up-directory ()
    "Go to parent directory, reusing existing dired buffer."
    (interactive)
    (dired-single-buffer ".."))
  :bind (:map dired-mode-map
              ([return] . dired-single-buffer)
              ([mouse-1] . dired-single-buffer-mouse)
              ([remap dired-up-directory] . ngq/dired-up-directory)
              ("<" . ngq/dired-up-directory)
              (">" . dired-single-buffer)))

(use-package diredfl
  :after dired
  :hook (dired-mode . diredfl-mode))

(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)))

(use-package dired-narrow
  :after dired
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(provide 'init-dired)

;;; init-dired.el ends here
