;; Fix dead-keys
(require 'iso-transl)

;; Use utf-8 whenever possible
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-language-environment "UTF-8")

(provide 'init-locales)
