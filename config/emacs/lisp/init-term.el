;;; init-term.el --- Terminal configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; Configuration for Emacs sessions running in a text terminal.

;;; Code:

(defun ngq/update-xterm-title ()
  "Update xterm window title with a value similar to `frame-title-format'."
  (let ((window-title-format "\033]2; %s \007")
        (title (concat
                (if buffer-file-name
                    (concat
                     (abbreviate-file-name (buffer-file-name))
                     (cond
                      (buffer-read-only " (read-only)")
                      ((buffer-modified-p) "*")))
                  (buffer-name))
                " — "
                invocation-name
                " "
                emacs-version
                " @ "
                (system-name))))
    (send-string-to-terminal (format window-title-format title))))

(defun ngq/tty-setup-hook ()
  "Configuration for Emacs sessions running in a terminal."
  (xterm-mouse-mode t)
  (keymap-global-set "<mouse-4>" #'scroll-down-line)
  (keymap-global-set "<mouse-5>" #'scroll-up-line))

(when (and (not (display-graphic-p))
           (string-prefix-p "xterm" (getenv-internal "TERM" initial-environment)))
  (add-hook 'post-command-hook #'ngq/update-xterm-title)
  (add-hook 'tty-setup-hook #'ngq/tty-setup-hook))

;; External packages

(use-package xclip
  :unless (display-graphic-p)
  :hook (elpaca-after-init . xclip-mode))

(use-package vterm
  :config (with-eval-after-load 'tab-line
            (add-to-list 'tab-line-exclude-modes 'vterm-mode)))

(use-package vterm-toggle
  :after vterm
  :bind (("C-$" . vterm-toggle)
         :map vterm-mode-map
         ("C-<return>" . vterm-toggle-insert-cd))
  :config (add-to-list 'display-buffer-alist
                       '("*vterm*"
                         (display-buffer-reuse-window display-buffer-at-bottom)
                         (dedicated . t)
                         (reusable-frames . visible)
                         (window-height . 0.3))))


(provide 'init-term)

;;; init-term.el ends here
