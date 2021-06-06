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

;; Add the split configuration files to the `load-path'
(add-to-list 'load-path ngq/lisp-directory)

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
(require 'init-theme)

;; Global

(require 'init-completion)
(require 'init-lsp)
(require 'init-projectile)
(require 'init-flycheck)

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

;; Machine-specific (optional)
(require 'init-local nil t)

(provide 'init)

;;; init.el ends here
