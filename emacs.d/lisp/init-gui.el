;; Turn off mouse interface
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode blink-cursor-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Disable GUI dialogs completely
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t)
(setq-default indicate-buffer-boundaries t)

;; Smooth scrolling
(setq scroll-conservatively 10000
      scroll-margin 5)

;; Change cursor shape if mark is active and a region exists
(defun activate-mark-init ()
  (setq cursor-type 'bar))

(defun deactivate-mark-init ()
  (setq cursor-type 'box))

(add-hook 'activate-mark-hook 'activate-mark-init)
(add-hook 'deactivate-mark-hook 'deactivate-mark-init)

(provide 'init-gui)
