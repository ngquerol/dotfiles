;;; init-packages.el --- package.el configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; Bootstraps package.el & use-package.

;;; Code:

(require 'package)

;; Bootstrap `package.el'
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; ;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'bind-key)
  (package-install 'diminish))

(setq-default use-package-always-ensure t
              use-package-expand-minimally t
              use-package-enable-imenu-support t
              use-package-compute-statistics t)

;; Use Quelpa to manage local or git packages
(use-package quelpa-use-package
  :config 
  (setq-default quelpa-checkout-melpa-p nil)
  (quelpa-use-package-activate-advice))

(provide 'init-packages)

;;; init-packages.el ends here
