;;; init-utils.el --- Miscellaneous utilities -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; Utility functions used either in the init files or throughout a typical
;; session, no matter the current major/minor mode(s).

;;; Code:

;; Non-interactive functions (mainly config helpers)

(defun ngq/read-config-file (file-name &optional no-error)
  "Return a string representing FILE-NAME's contents literally.

Return nil if the file is not readable or does not exists.
Relative paths are resolved from `user-emacs-directory'.
If the file is not accessible or does not exist altogether, an
error is raised unless NO-ERROR is non-nil."
  (let ((path (expand-file-name file-name user-emacs-directory)))
    (with-temp-buffer
      (condition-case err
          (insert-file-contents-literally path)
        (error (if no-error nil (signal (car err) (cadr err)))))
      (buffer-substring-no-properties (point-min) (point-max)))))

;; Interactive functions (mainly editing helpers)

(defun ngq/kill-this-buffer ()
  "Kill current buffer without confirmation."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key [remap kill-buffer] #'ngq/kill-this-buffer)

(defun ngq/rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name))
        (completing-read-function 'completing-read-default))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New file name: " filename)))
        (if (vc-backend filename)
            (vc-rename-file filename new-name)
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t))))))

(global-set-key (kbd "C-c R") #'ngq/rename-file-and-buffer)

(defun ngq/delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (y-or-n-p (format "Really delete %s? " filename))
          (if (vc-backend filename)
              (vc-delete-file filename)
            (progn
              (delete-file filename t)
              (message "Deleted file %s" filename)
              (kill-buffer)))))))

(global-set-key (kbd "C-c D") #'ngq/delete-file-and-buffer)

(provide 'init-utils)

;;; init-utils.el ends here
