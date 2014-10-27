;;  Don't show dotfiles in dired by default (M-o to toggle)
(require 'dired-x)
(setq-default dired-omit-files-p t) ; this is buffer-local variable
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))


(provide 'init-dired)
