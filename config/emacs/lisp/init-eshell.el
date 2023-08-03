;;; init-eshell.el --- Eshell configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; Configure eshell and friends.

;;; Code:

(use-package eshell
  :straight (:type built-in)
  :defines (eshell-output-filter-functions
            eshell-mode-map
            eshell-cmpl-mode-map)
  :functions (eshell/cd
              eshell/clear
              eshell-send-input
              eshell-get-old-input
              eshell-skip-prompt
              eshell-life-is-too-much)
  :preface
  (defun ngq/eshell-exit-or-delete ()
    "Mimic shell behaviour: delete char under point or, if there is
  no input, exit eshell"
    (interactive)
    (eshell-skip-prompt)
    (if (= (point) (point-at-eol))
        (progn
          (eshell-life-is-too-much)
          (when (window-deletable-p)
            (delete-window)))
      (delete-char 1)))

  (defun ngq/eshell-clear ()
    "Mimic shell behaviour: clear all printed output and scrollback, keeping
 current input if any."
    (interactive)
    (let ((input (eshell-get-old-input)))
      (eshell/clear t)
      (eshell-send-input nil nil t)
      (insert input)))

  (defun ngq/eshell-here ()
    "Opens eshell in the directory associated with the current buffer's file.

Reuses a previously opened eshell buffer if possible. "
    (interactive)
    (let ((current-directory (expand-file-name default-directory)))
      (let ((size (- (/ (frame-height) 4)))
            (buffer (get-buffer "*eshell*")))
        (unless (eq buffer (current-buffer))
          (if buffer
              (progn (switch-to-buffer-other-window buffer)
                     (window-resize (get-buffer-window) size))
            (select-window (split-window-below size))
            (eshell))))
      (message current-directory)
      (unless (string= current-directory default-directory)
        (eshell/cd current-directory)
        (ngq/eshell-clear)
        (message "eshell: Changed directory to %s" current-directory))))

  ;; needed because `eshell-mode-map' is local
  (defun ngq/eshell-mode-hook ()
    "Common `eshell-mode' configuration."
    (define-key eshell-mode-map (kbd "C-n") #'eshell-next-input)
    (define-key eshell-mode-map (kbd "C-p") #'eshell-previous-input)
    (define-key eshell-mode-map (kbd "C-M-n") #'eshell-next-prompt)
    (define-key eshell-mode-map (kbd "C-M-p") #'eshell-previous-prompt)
    (define-key eshell-mode-map (kbd "C-d") #'ngq/eshell-exit-or-delete)
    (define-key eshell-mode-map (kbd "C-l") #'ngq/eshell-clear)
    (mapc
     (lambda (func) (add-to-list 'eshell-output-filter-functions func))
     '(eshell-truncate-buffer
       eshell-postoutput-scroll-to-bottom)))

  :hook (eshell-mode . ngq/eshell-mode-hook)
  :bind ("C-c $" . ngq/eshell-here)
  :config (setq-default eshell-hist-ignoredups t
                        eshell-history-size 1024
                        eshell-save-history-on-exit t
                        eshell-banner-message ""
                        eshell-prefer-lisp-functions nil
                        eshell-buffer-maximum-lines 2500
                        eshell-scroll-to-bottom-on-input 'all
                        eshell-scroll-to-bottom-on-output t
                        eshell-scroll-show-maximum-output t))

(provide 'init-eshell)

;;; init-eshell.el ends here
