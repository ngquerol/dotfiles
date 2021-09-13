;;; init-ruby.el --- Ruby configuration -*- lexical-binding: t -*-

;; Author: Nicolas Gaulard-Querol <nicolas.gquerol@gmail.com>

;;; Commentary:

;; Settings for Ruby programming.

;;; Code:

(use-package ruby-mode
  :straight nil
  :mode ("\\.rb\\'" "Rakefile\\'" "Gemfile\\'")
  :interpreter "ruby"
  :config (setq ruby-insert-encoding-magic-comment nil))

(use-package robe
  :preface (defun ngq/company-activate-robe ()
             (ngq/company-activate-local-backend 'company-robe))
  :hook ((ruby-mode . robe-mode)
         (ruby-mode . ngq/company-activate-robe)))

(provide 'init-ruby)

;;; init-ruby.el ends here
