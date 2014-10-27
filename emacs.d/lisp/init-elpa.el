(require 'package)

;; Package repositories
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Initialize package.el
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Install package 'name'
(defun package-require (name)
  (unless (package-installed-p name)
    (package-install name)))

;; Handy keybindings
(global-set-key (kbd "C-c P") 'package-list-packages-no-fetch)
(global-set-key (kbd "C-c p") 'package-list-packages)

(provide 'init-elpa)
