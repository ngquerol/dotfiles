(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))))

(defun eshell/x ()
  "exit eshell and delete its window"
  (interactive)
  (insert "exit")
  (eshell-send-input)
  (delete-window))

(defun eshell/clear ()
  "clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(setq eshell-banner-message "\n")

(add-hook 'eshell-mode-hook #'(lambda ()
                                (define-key eshell-mode-map (kbd "C-d") 'eshell/x)
                                (define-key eshell-mode-map (kbd "C-l") 'eshell/clear)))

(global-set-key (kbd "C-c s") 'eshell-here)

(provide 'init-eshell)
