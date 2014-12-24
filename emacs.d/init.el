;;; init.el - Emacs startup file
;;; TODO: load some settings after some package/mode has been loaded

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;; Add .emacs.d/lisp to load-path
(add-to-list 'load-path (concat user-emacs-directory "lisp/"))

;; To keep things clean
;; (these files are not deleted upon reboot, unlike those in
;; `temporary-file-directory`)
(setq junk-files-directory (concat user-emacs-directory "temp/"))

;; Ask for "y or n" instead of "yes or no"
(defalias 'yes-or-no-p 'y-or-n-p)

;; Load settings for specific modes and/or features
(require 'init-elpa)
(require 'init-windows)
(require 'init-gui)
(require 'init-editing)
(require 'init-session)

(require 'init-dired)
(require 'init-eshell)
(require 'init-git)
(require 'init-helm)
(require 'init-snippets)

;; Load custom settings, if they do exist
(let ((local-file (concat user-emacs-directory "local.el")))
  (if (file-readable-p local-file)
     (load-file local-file)))

;; Finally, set default locales
(require 'init-locales)

;; Fix the PATH variable
(require-package 'exec-path-from-shell)
(add-hook 'server-visit-hook (lambda ()
                               (if (display-graphic-p)
                                   (exec-path-from-shell-initialize))))
