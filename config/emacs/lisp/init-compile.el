;;; init-compile.el --- `compilation-mode' and build tools configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:

;; Configuration for `compilation-mode' and build tools.

;;; Code:

;; Compilation buffers
(use-package compile
  :ensure nil
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

  ;; Add AddressSanitizer error format
  (setq compilation-error-regexp-alist-alist
        (cons '(asan "^\s+#[0-9]+ 0x[0-9a-f]+ in [a-zA-Z_]+\\(?:+0x[0-9a-f]+\\)? \\(\\([a-zA-Z_]+\.[a-zA-Z_]+\\):\\([0-9]+\\)\\)$" 2 3 nil nil 1)
              compilation-error-regexp-alist-alist))
  (setq compilation-error-regexp-alist
        (cons 'asan compilation-error-regexp-alist))

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

(provide 'init-compile)

;;; init-compile.el ends here
