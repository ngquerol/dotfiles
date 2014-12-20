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

;; Faster repeat
(global-set-key (kbd "C-z") 'repeat)

;; Join line
(defun my-join-line ()
  (interactive)
  (join-line -1))

(global-set-key (kbd "M-j") 'my-join-line)

;; Smart open line
(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "M-<down>") 'smart-open-line)
(global-set-key (kbd "M-<up>") 'smart-open-line-above)

;; Delete the whole line
(global-set-key (kbd "C-c d") 'kill-whole-line)

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

;; Smart beginn
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line."
  (interactive "^")
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))
(global-set-key (kbd "<home>") 'smart-beginning-of-line)
(global-set-key (kbd "C-a") 'smart-beginning-of-line)

;; Align according to regexp
(global-set-key (kbd "C-c a") 'align-regexp)

;; Expand-region
(require-package 'expand-region)
(global-set-key (kbd "C-c e") 'er/expand-region)

;; Company
(require-package 'company)
(require-package 'company-c-headers)
(require-package 'company-go)
(with-eval-after-load "company"
  (setq company-minimum-prefix-length 2
        company-abort-manual-when-too-short t
        company-selection-wrap-around t
        company-idle-delay 0.25)
  (setq company-backends '(company-clang
                           company-c-headers
                           company-go
                           company-capf
                           company-dabbrev-code
                           company-files))
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "<tab>") 'company-complete))

;; Flycheck
(with-eval-after-load "flycheck"
  (setq-default flycheck-gcc-language-standard "c99"
                flycheck-clang-language-standard "c99"
                flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; Programming-specific modes
(add-hook 'prog-mode-hook #'flycheck-mode)
(add-hook 'prog-mode-hook #'company-mode)

(provide 'init-editing)
