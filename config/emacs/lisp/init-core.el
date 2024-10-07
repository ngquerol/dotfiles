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
(setq default-directory (file-name-as-directory (getenv "HOME")))

;; Kill buffer without confirmation
(keymap-set ctl-x-map "k" #'kill-current-buffer)

(use-package diff-mode
  :ensure nil
  :config (setq diff-default-read-only t))

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

;; Use sane(r) regexp syntax
(use-package pcre2el
  :hook (elpaca-after-init . pcre-mode))

(provide 'init-core)

;;; init-core.el ends here
