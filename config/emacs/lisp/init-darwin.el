;;; init-darwin.el --- macOS-specific configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; Common configuration for Emacs sessions (gui/tty) running in macOS.

;;; Code:

(unless (eq system-type 'darwin)
  (error "This file should only be loaded when running under macOS!"))

;; Set ⌘ to meta, leave right ⌥ alone to type special characters,
;; and do not apply any momentum to scrolling
(setq mac-option-modifier 'alt
      mac-right-option-modifier 'none
      mac-command-modifier 'meta
      ns-use-mwheel-momentum nil)

;; GUI sessions
(when (display-graphic-p)

  ;; Show menu bar
  (menu-bar-mode t)

  ;; Use the appropriate system font to display emoji
  (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)

  ;; Hide titlebar proxy icon
  (setq ns-use-proxy-icon nil)

  ;; Standard macOS app keybindings
  (when (fboundp 'ns-do-hide-emacs)
    (keymap-global-set "M-h" #'ns-do-hide-emacs))

  (when (fboundp 'ns-do-show-character-palette)
    (keymap-global-set "C-M-SPC" #'ns-do-show-character-palette))

  ;; Retrieve environment variables from the shell
  (use-package exec-path-from-shell
    :hook (elpaca-after-init . exec-path-from-shell-initialize)
    :config (setq-default exec-path-from-shell-arguments '("-l")
                          exec-path-from-shell-variables '("PATH"
                                                           "MANPATH"
                                                           "INFOPATH"
                                                           "PKG_CONFIG_PATH"
                                                           "CPATH"
                                                           "LIBRARY_PATH"
                                                           "GOPATH"))))

(provide 'init-darwin)

;;; init-darwin.el ends here
