;;; early-init.el --- Emacs early startup file -*- lexical-binding: t -*-

;; Author: Nicolas Gaulard-Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; Emacs 27+ introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

;;; Code:

;; Disable most GUI widgets early on
(setq default-frame-alist '((horizontal-scroll-bars . nil)
                            (vertical-scroll-bars . nil)
                            (menu-bar-lines . 0)
                            (tool-bar-lines . 0)
                            (internal-border-width . 0)
                            (height . 50)
                            (width . 90)))

;; Do not show any kind of startup message
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-buffer-menu t
      initial-buffer-choice t)

;; Do not resize frame implicitly
(setq frame-inhibit-implied-resize t)

;; Performance-related settings

(let ((orig-file-name-handler-alist file-name-handler-alist))
  (setq file-name-handler-alist nil)
  (add-hook 'after-init-hook
            (lambda () (setq file-name-handler-alist
                             orig-file-name-handler-alist))))

;; Run GC when Emacs goes out of focus
(add-function :after after-focus-change-function
              (lambda ()
                (unless (seq-some #'frame-focus-state (frame-list))
                  (garbage-collect))))

;; Run GC when Emacs is idle for a set period of time
(run-with-idle-timer 10 t #'garbage-collect)

;; Increase GC threshold
(setq gc-cons-threshold (* 10 1000 1000))

(provide 'early-init)

;;; early-init.el ends here
