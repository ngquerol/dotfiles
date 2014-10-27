;; Clear eshell buffer like in bash or zsh
(defun eshell/clear ()
  "clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(add-hook 'eshell-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-l") 'eshell/clear)
             (local-set-key (kbd "C-d") 'eshell-life-is-too-much)))

(global-set-key (kbd "C-c s") 'eshell)

(provide 'init-eshell)
