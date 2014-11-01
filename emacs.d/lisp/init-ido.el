(require-package 'ido-ubiquitous)
(require-package 'flx)
(require-package 'flx-ido)

(require 'ido)
(require-package 'ido-vertical-mode)
(setq ido-everywhere t
      ido-case-fold t
      ido-enable-flex-matching t
      ido-save-directory-list-file (concat junk-files-directory "ido.last")
      ido-create-new-buffer 'always
      ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
(ido-mode)
(ido-ubiquitous-mode)
(ido-vertical-mode)

;; Better flex matching
(flx-ido-mode)
(setq ido-use-faces nil)

;; Use ido for recentf
(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))
(global-set-key (kbd "C-x C-r") 'recentf-ido-find-file)

(provide 'init-ido)
