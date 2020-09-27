;;; init.el --- Emacs startup file -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; This file bootstraps the configuration, which is divided into a number of
;; other files.

;;; Code:

;; Check if running Emacs is recent enough
(let ((minimum-version "27"))
  (when (version< emacs-version minimum-version)
    (error "This Emacs is too old -- this config requires version %s or higher"
           minimum-version)))

;; Personal information
(setq user-full-name "Nicolas G. Querol"
      user-mail-address "nicolas.gquerol@gmail.com")

;; Start in the user's HOME directory
(setq default-directory (getenv "HOME"))

;; If `user-emacs-directory' is a symlink, use its target directly
(when-let ((true-user-emacs-directory
            (file-symlink-p (directory-file-name user-emacs-directory))))
  (setq user-emacs-directory true-user-emacs-directory))

;; Add the split configuration files to the `load-path'
(add-to-list 'load-path (locate-user-emacs-file "lisp"))

;; Bootstrap the package manager
(require 'init-packages)

;; Platform-specific configuration
(let ((os (symbol-name system-type)))
  (require (intern (concat "init-" os)) nil t))

;; Core

(require 'init-core)
(require 'init-dired)
(require 'init-editing)
(require 'init-term)
(require 'init-utils)
(require 'init-ui)

;; Global

(require 'init-company)
(require 'init-flycheck)
(require 'init-ivy)
(require 'init-lsp)
(require 'init-projectile)

;; Domain-specific

(require 'init-build)
(require 'init-cc)
(require 'init-clojure)
(require 'init-eshell)
(require 'init-go)
(require 'init-javascript)
(require 'init-lisp)
(require 'init-org)
(require 'init-python)
(require 'init-ruby)
(require 'init-rust)
(require 'init-scheme)
(require 'init-shell)
(require 'init-tex)
(require 'init-vcs)
(require 'init-web)
(require 'init-yaml)
(require 'init-yasnippet)

;; Machine-specific (optional)
(require 'init-local nil t)

;; Load settings managed via `custom.el'
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)

(provide 'init)

;;; init.el ends here
