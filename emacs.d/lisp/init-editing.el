(setq-default auto-save-default nil
              case-fold-search t
              indent-tabs-mode nil
              make-backup-files nil
              sentence-end-double-space nil
              require-final-newline t)

;; Delete selected text
(delete-selection-mode)

;; Indentation
(setq-default standard-indent 4
              tab-width 4
              indent-tabs-mode nil
              c-default-style "java")

(add-hook 'c-mode-common-hook
          '(lambda ()
             (c-set-offset 'case-label '+)))

;; Electric all the things
(electric-indent-mode)
(electric-pair-mode)
(show-paren-mode)

;; Reload buffers automagically if the corresponding file has been changed
(global-auto-revert-mode)

;; Remove superfluous whitespace upon save
(add-hook 'before-save-hook '(lambda ()
                               (delete-trailing-whitespace)))

;; Hard-wrap lines at 80 columns when editing text files
(add-hook 'text-mode-hook
          '(lambda ()
             (set-fill-column 80)
             (turn-on-auto-fill)))

;; Automagically indent when inserting a newline
(global-set-key (kbd "RET") 'reindent-then-newline-and-indent)

;; Comment what I really mean
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
If no region is selected and current line is not blank and we
are not at the end of the line, then comment current line.
Replaces default behaviour of comment-dwim, when it inserts
comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (not (region-active-p))
      (comment-or-uncomment-region
       (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(global-set-key (kbd "C-c c") 'comment-dwim-line)

;; Smart home key
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line."
  (interactive "^")
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))
(global-set-key (kbd "<home>") 'smart-beginning-of-line)
(global-set-key (kbd "C-a") 'smart-beginning-of-line)

;; Browse-kill-ring
(require-package 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; Expand-region
(require-package 'expand-region)
(global-set-key (kbd "C-c e") 'er/expand-region)

;; Company
(require-package 'company)
(require-package 'company-c-headers)
(setq company-minimum-prefix-length 2
      company-abort-manual-when-too-short t
      company-selection-wrap-around t)
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'c-mode-common-hook
          '(lambda ()
             (add-to-list 'company-backends 'company-c-headers)
             (delete 'company-semantic company-backends)))

;; Grep
(setq grep-command "grep -nHR -e ")
(global-set-key (kbd "C-x g") 'grep)

;; Various keybindings
(global-set-key (kbd "C-c a") 'align-regexp)
(global-set-key (kbd "C-c b") 'electric-buffer-list)

(provide 'init-editing)
