(require-package 'smex)

(smex-initialize)
(setq smex-save-file (concat junk-files-directory "smex-items"))
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(provide 'init-smex)
