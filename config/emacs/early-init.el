;;; early-init.el --- Emacs early startup file -*- lexical-binding: t -*-

;; Author: Nicolas Gaulard-Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; Emacs 27+ introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

;;; Code:

;; If `user-emacs-directory' is a symlink, resolve it
(when-let ((true-user-emacs-directory
            (file-symlink-p (directory-file-name user-emacs-directory))))
  (setq user-emacs-directory true-user-emacs-directory))

;; Standard paths
(defconst ngq/lisp-directory
  (expand-file-name "lisp" user-emacs-directory))

(defconst ngq/etc-directory
  (expand-file-name "etc" user-emacs-directory))

(defconst ngq/var-directory
  (expand-file-name "var" user-emacs-directory))

;; Native compilation
(when (featurep 'native-compile)
  (setq native-comp-speed 2
        native-comp-async-report-warnings-errors nil)
  (startup-redirect-eln-cache
   (expand-file-name "eln-cache/" ngq/var-directory)))

;; Disable most GUI widgets early on
(setq default-frame-alist '((horizontal-scroll-bars . nil)
                            (vertical-scroll-bars . nil)
                            (menu-bar-lines . 0)
                            (tool-bar-lines . 0)
                            (internal-border-width . 0)
                            (undecorated-round . t)
                            (height . 50)
                            (width . 95)))

;; Do not show any kind of startup message
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-buffer-menu t
      initial-buffer-choice t)

;; Set UTF-8 everywhere
(set-language-environment "UTF-8")

;; Disable `custom.el'
(setq custom-file null-device)

;; Performance-related settings

;; Avoid any GC during startup, and set increased runtime thresholds
(setq gc-cons-threshold (* 100 1024 1024)
      gc-cons-percentage 0.6)

(let ((orig-file-name-handler-alist file-name-handler-alist))
  (defun ngq/restore-gc-and-file-name-handlers ()
    (setq file-name-handler-alist orig-file-name-handler-alist
          gc-cons-threshold (* 50 1024 1024)
          gc-cons-percentage 0.2))
  (setq file-name-handler-alist nil)
  (add-hook 'after-init-hook #'ngq/restore-gc-and-file-name-handlers))

;; Run GC when Emacs is idle for a set period of time
(run-with-idle-timer 10 t #'garbage-collect)

;; Increase `read-process-output-max' to 1MB
(setq read-process-output-max (* 1024 1024))

;; Completely disable `package.el'
(setq package-enable-at-startup nil)

(provide 'early-init)

;;; early-init.el ends here
