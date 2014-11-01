;; Switch windows with windmove
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)

;; Popwin
(require-package 'popwin)
(require 'popwin)
(popwin-mode t)
(push '("*eshell*" :height 10 :position bottom :stick t) popwin:special-display-config)
(push '("*Backtrace*") popwin:special-display-config)
(push '("*Apropos*" :height 15 :position bottom :stick t) popwin:special-display-config)

(provide 'init-windows)
