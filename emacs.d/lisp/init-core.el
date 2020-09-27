;;; init-clojure.el --- Core Emacs configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; Configuration for low-level Emacs settings

;;; Code:

;; Use UTF-8 whenever possible
(setq locale-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system (if (eq system-type 'windows-nt)
                                 'utf-16-le
                               'utf-8))
(prefer-coding-system 'utf-8)

;; Increase the chunk size of the data read from subprocesses
(setq read-process-output-max (* 1024 1024))

;; External packages

;; Organize cache & persistent configuration files
(use-package no-littering
  :defines (recentf-exclude
            no-littering-var-directory
            undo-fu-session-incompatible-files
            woman-cache-filename)
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory))

  (with-eval-after-load 'undo-fu-session
    (add-to-list 'undo-fu-session-incompatible-files no-littering-var-directory))

  (with-eval-after-load 'woman
    (setq woman-cache-filename
          (no-littering-expand-var-file-name "woman-cache.el"))))

(provide 'init-core)

;;; init-core.el ends here
