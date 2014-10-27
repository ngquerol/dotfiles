;; Put auto-saves in the system's temp folder
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t))
      auto-save-list-file-prefix
      (concat temporary-file-directory "emacs-auto-saves-"))

;; Save point position between sessions
(require 'saveplace)
(setq save-place-file (concat junk-files-directory "saved-places"))
(setq-default save-place t)

;; Save minibuffer history between sessions
(setq savehist-file (concat junk-files-directory "history"))
(savehist-mode)

;; Save recent files
(require 'recentf)
(setq recentf-save-file (concat junk-files-directory "recentf")
      recentf-max-menu-items 50
      recentf-exclude (append recentf-exclude
                              '("/.emacs.d/elpa"
                                "/.emacs.d/temp"
                                ".git")))
(recentf-mode)

(provide 'init-session)
