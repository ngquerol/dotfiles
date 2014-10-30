;;  Don't show dotfiles in dired by default (C-x M-o to toggle)
(require 'dired-x)
(setq-default dired-omit-files-p t) ; this is a buffer-local variable
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))

;; Hide clutter
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(provide 'init-dired)
