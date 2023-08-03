;;; init-build.el --- Build tools configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:

;; Configuration for build tools.

;;; Code:

;; Compilation buffers
(use-package compile
  :straight (:type built-in)
  :commands compile
  :preface
  (defun --bury-compilation-buffer (buffer)
    (delete-window (get-buffer-window buffer))
    (bury-buffer buffer))

  (defun --maybe-bury-compilation-buffer (buffer delay)
    (let ((timer (run-with-timer delay nil #'--bury-compilation-buffer buffer)))
      (add-hook 'window-selection-change-functions
                (lambda (_)
                  (when (eq buffer (window-buffer (selected-window)))
                    (cancel-timer timer))) nil t)))

  (defun ngq/bury-compilation-buffer-if-successful (buffer string)
    "Bury compilation BUFFER and delete its window after a short delay if:

- no warnings or errors were produced during compilation
- it is not expressely selected by the user

Otherwise select its window."
    (let ((delay 3)
          (finished-output "finished")
          (problem-regexp (rx (or (sequence "warn" (optional "ing"))
                                  (sequence "err" (optional "or"))))))
      (unless (eq buffer (window-buffer (selected-window)))
        (if (and (string-match finished-output string)
                 (not (with-current-buffer buffer
                        (goto-char (point-min))
                        (search-forward-regexp problem-regexp nil t))))
            (--maybe-bury-compilation-buffer buffer delay)
          (pop-to-buffer buffer)))))
  :config
  (setq-default compilation-scroll-output t
                compilation-always-kill t
                compilation-ask-about-save nil)

  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

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
