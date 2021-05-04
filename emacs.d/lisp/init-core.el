;;; init-clojure.el --- Core Emacs configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; Configuration for low-level Emacs settings

;;; Code:

;; Personal information
(setq user-full-name "Nicolas G. Querol"
      user-mail-address "nicolas.gquerol@gmail.com")

;; Start in the user's HOME directory
(setq default-directory (getenv "HOME"))

;; Use UTF-8 whenever possible
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)

;; Native compilation
(when (featurep 'nativecomp)
  (setq comp-speed 2
        comp-async-report-warnings-errors nil))

;; External packages

;; Organize cache & persistent configuration files
(use-package no-littering
  :defines (recentf-exclude
            no-littering-var-directory
            undo-fu-session-incompatible-files
            woman-cache-filename)
  :config
  (setq no-littering-etc-directory ngq/etc-directory
        no-littering-var-directory ngq/var-directory
        auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory))

  (with-eval-after-load 'undo-fu-session
    (add-to-list 'undo-fu-session-incompatible-files no-littering-var-directory))

  (with-eval-after-load 'woman
    (setq woman-cache-filename
          (no-littering-expand-var-file-name "woman-cache.el"))))

;; Strat-up profiler
(use-package esup)

(provide 'init-core)

;;; init-core.el ends here
