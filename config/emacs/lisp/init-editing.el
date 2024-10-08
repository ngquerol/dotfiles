;;; init-editing.el --- Editing helpers & configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; Settings related to text editing, in broad sense.

;;; Code:

;; General settings
(setq sentence-end-double-space nil
      require-final-newline t
      inhibit-eol-conversion t)

;; Indent with 4 spaces as a default
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Remove trailing whitespace upon save
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Wrap lines at 80 columns when editing text files
(setq-default fill-column 80)
(add-hook 'text-mode-hook #'auto-fill-mode)

;; Delete selected text
(delete-selection-mode t)

;; Automatically indent when inserting a newline
(keymap-global-set "RET" #'reindent-then-newline-and-indent)

;; Join following line by default
(defun ngq/my-join-line ()
  "Join the current line with the following one."
  (interactive)
  (join-line -1))

(keymap-global-set "M-j" #'ngq/my-join-line)

;; Smart open line
(defun ngq/smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun ngq/smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(keymap-global-set "M-<return>" #'ngq/smart-open-line)
(keymap-global-set "M-S-<return>" #'ngq/smart-open-line-above)

;; Zap (up) to char
(keymap-global-set "M-z" #'zap-to-char)
(keymap-global-set "M-Z" #'zap-up-to-char)

;; Align according to regexp
(keymap-global-set "C-c a" #'align-regexp)

;; Change case of region if active, of word otherwise.
(keymap-global-set "<remap> <upcase-word>" #'upcase-dwim)
(keymap-global-set "<remap> <downcase-word>" #'downcase-dwim)
(keymap-global-set "<remap> <capitalize-word>" #'capitalize-dwim)

;; Reload buffers automagically if the corresponding file has been changed
(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode)
  :config (setq auto-revert-avoid-polling t))

;; Show some whitespace characters
(use-package whitespace
  :ensure nil
  :hook (after-init . global-whitespace-mode)
  :config
  (setq whitespace-style '(tab-mark face trailing missing-newline-at-eof))
  (setf (cdr (assoc 'tab-mark whitespace-display-mappings))
        '(?\t [?\u00bb ?\t] [?\t])))

;; Expand abbreviations
(use-package abbrev
  :ensure nil
  :config
  (setq save-abbrevs 'silently)
  (when (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file)))

(use-package editorconfig
  :ensure nil
  :config (editorconfig-mode))

;; External packages

;; Visually interactive align-regexp
(use-package ialign
  :bind ("C-c a" . ialign)
  :config (with-eval-after-load 'pcre2el
            (setq ialign-pcre-mode t)))

;; Edit grep buffer and apply the changes
(use-package wgrep
  :commands (wgrep-setup wgrep-change-to-wgrep-mode))

;; Move line or region around
(use-package move-text
  :bind (("M-<up>" . move-text-up)
         ("M-<down>" . move-text-down)))

;; Move to beginning/end of code, line or comment
(use-package mwim
  :bind (([remap move-beginning-of-line] . #'mwim-beginning)
         ([remap move-end-of-line] . #'mwim-end)))

;; Increase selected region by semantic units
(use-package expand-region
  :bind (;; cf. all xxx-mode-expansions.el for more
         ("C-x x" . er/expand-region)
         ("C-c x i" . er/mark-inside-pairs)
         ("C-c x o" . er/mark-outside-pairs)
         ("C-c x c" . er/mark-comment)
         ("C-c x d" . er/mark-defun)
         ("C-c x s" . er/mark-symbol)
         ("C-c x w" . er/mark-word)
         ("C-c x m" . er/mark-method-call))
  :config (setq expand-region-contract-fast-key "X"
                expand-region-smart-cursor t))

;; Operate on current line if region is undefined
(use-package whole-line-or-region
  :hook (elpaca-after-init . whole-line-or-region-global-mode)
  :bind ("C-w" . whole-line-or-region-kill-region) ;; KLUDGE
  )

;;  A regexp/replace command for Emacs with interactive visual feedback
(use-package visual-regexp
  :bind ([remap query-replace] . vr/query-replace))

;; Multiple cursors for Emacs
(use-package multiple-cursors
  :bind (("C-c m m" . mc/mark-more-like-this-extended)
         ("C-c m s" . mc/mark-all-symbols-like-this-in-defun)
         ("C-c m w" . mc/mark-all-words-like-this-in-defun)
         ("C-c m e" . mc/edit-lines)
         ("C-c m a" . mc/mark-all-dwim)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

;; Comment what I really mean
(use-package comment-dwim-2
  :bind ([remap comment-dwim] . comment-dwim-2))

;; Automatic insertion, wrapping and paredit-like navigation with user defined pairs
(use-package smartparens
  :hook (((prog-mode org-mode) . turn-on-smartparens-mode)
         (prog-mode . turn-on-show-smartparens-mode)
         ((emacs-lisp-mode lisp-mode) . turn-on-smartparens-strict-mode))
  :bind (("C-M-f" . sp-forward-sexp)
         ("C-M-b" . sp-backward-sexp)
         ("C-M-u" . sp-backward-up-sexp)
         ("C-M-d" . sp-down-sexp)
         ("C-M-p" . sp-backward-down-sexp)
         ("C-M-n" . sp-up-sexp)
         ("M-d" . sp-kill-word)
         ("M-D" . sp-kill-symbol)
         ("M-s" . sp-splice-sexp)
         ("M-r" . sp-splice-sexp-killing-around)
         ("C-M-<left>" . sp-backward-slurp-sexp)
         ("C-M-<right>" . sp-forward-slurp-sexp)
         ("C-S-M-<left>" . sp-backward-barf-sexp)
         ("C-S-M-<right>" . sp-forward-barf-sexp)
         ("C-M-a" . sp-beginning-of-sexp)
         ("C-M-e" . sp-end-of-sexp)
         ("C-M-t" . sp-transpose-sexp)
         ("C-M-k" . sp-kill-sexp)
         ("C-M-w" . sp-copy-sexp)
         ("M-<backspace>" . sp-backward-unwrap-sexp)
         ("M-<delete>" . sp-unwrap-sexp)
         ("M-F" . sp-forward-symbol)
         ("M-B" . sp-backward-symbol)
         ("C-)" . sp-wrap-round)
         ("C-]" . sp-wrap-square)
         ("C-}" . sp-wrap-curly)
         ("C-\"" . (lambda () (interactive) (sp-wrap-with-pair "\"")))
         ("C-'" . (lambda () (interactive) (sp-wrap-with-pair "'")))
         ("C-M-)" . sp-rewrap-sexp)
         ("C-(" . sp-change-inner)
         ("C-M-(" . sp-change-enclosing))
  :config
  (require 'smartparens-config)

  (dolist (pair '("(" "{" "["))
    (sp-local-pair 'prog-mode pair nil :post-handlers '(("||\n[i]" "RET")))))

(provide 'init-editing)

;;; init-editing.el ends here
