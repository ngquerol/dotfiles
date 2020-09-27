;;; init-eshell.el --- Eshell configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; Configure eshell and friends.

;;; Code:

(use-package eshell
  :ensure nil
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
          (delete-window))
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
      (let ((buffer (get-buffer "*eshell*")))
        (if buffer (progn (select-window (get-buffer-window buffer))
                          (switch-to-buffer buffer)))
        (select-window (split-window-below (- (/ (frame-height) 4))))
        (eshell))
      (message current-directory)
      (unless (string= current-directory default-directory)
        (eshell/cd current-directory)
        (ngq/eshell-clear)
        (message "eshell: Changed directory to %s" current-directory))))

  (defun ngq/eshell-first-time-hook ()
    "Common `eshell-mode' configuration, set when eshell first loads."
    (mapc
     (lambda (func) (add-to-list 'eshell-output-filter-functions func))
     '(eshell-truncate-buffer
       eshell-postoutput-scroll-to-bottom)))

  ;; needed because `eshell-mode-map' is local
  (defun ngq/eshell-mode-hook ()
    "Common `eshell-mode' configuration."
    (bind-key "C-d" #'ngq/eshell-exit-or-delete eshell-mode-map)
    (bind-key "C-l" #'ngq/eshell-clear eshell-mode-map))

  :hook ((eshell-first-time-mode . ngq/eshell-first-time-hook)
         (eshell-mode . ngq/eshell-mode-hook))
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
