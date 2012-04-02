;; helper function
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
	(output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

;; mode line
(setq-default mode-line-format
	      (list
	       " "
	       ;; file path
	       '(:eval (propertize (shorten-directory default-directory 20)
				   'face 'mode-line-folder-face))
	       ;; file name
	       '(:eval (propertize "%b" 'face 'mode-line-buffer-name))
	       " "
	       ;; read-only or modified status
	       '(:eval
		 (cond (buffer-read-only
			(propertize " RO " 'face 'mode-line-read-only-face))
		       ((buffer-modified-p)
			(propertize " ** " 'face 'mode-line-modified-face))
		       (t " -- ")))
	       " "
	       '(:eval (propertize mode-name 'face 'mode-line-mode-face))
	       " :: "
	       ;; line #
	       "line %l, %p"
	       ))
