;;; init-build.el --- Build tools configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:

;; Configuration for build tools.

;;; Code:

;; Compilation buffers
(use-package compile
  :ensure nil
  :commands compile
  :preface
  (defun ngq/colorize-compilation-buffer ()
    "Colorize a compilation buffer by interpreting color escape sequences
lying between `compilation-filter-start' and `point'."
    (ansi-color-apply-on-region compilation-filter-start (point)))

  (defun ngq/bury-compilation-buffer-if-successful (buffer string)
    "Bury compilation BUFFER and delete its window (after a short delay) if no
warnings or errors were produced; Otherwise select its window."
    (let ((problem-regexp (rx (or (sequence "warn" (optional "ing"))
                                  (sequence "err" (optional "or"))))))
      (if (and (string-match "finished" string)
               (not (with-current-buffer buffer
                      (goto-char (point-min))
                      (search-forward-regexp problem-regexp nil t))))
          (run-with-timer 1 nil
                          (lambda (buffer)
                            (delete-window (get-buffer-window buffer))
                            (bury-buffer buffer))
                          buffer)
        (pop-to-buffer buffer nil t))))
  :config
  (setq-default compilation-scroll-output 'first-error
                compilation-always-kill t
                compilation-ask-about-save nil)

  (add-hook 'compilation-filter-hook #'ngq/colorize-compilation-buffer)

  (add-to-list 'compilation-finish-functions #'ngq/bury-compilation-buffer-if-successful)
  (put 'compilation-read-command 'safe-local-variable #'booleanp))

;; External packages

;; CMake build files
(use-package cmake-mode
  :mode "CMakeLists\\.txt\\'")

;; Meson build files
(use-package meson-mode
  :defines projectile-project-root-files
  :mode "meson\\.build\\'"
  :init (with-eval-after-load 'projectile
          (add-to-list 'projectile-project-root-files "meson.build")))

(provide 'init-build)

;;; init-build.el ends here
