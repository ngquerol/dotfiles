;;; init.el - Emacs startup file

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Package.el

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;;; Locales

(prefer-coding-system        'utf-8)
(set-default-coding-systems  'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(set-language-environment "UTF-8")

;; Fix the PATH variable
(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

;; Load custom settings, if they do exist
(let ((local-file (concat user-emacs-directory "local.el")))
  (if (file-readable-p local-file)
      (load-file local-file)))

;;; Interface

;; Turn off mouse interface
(dolist (mode '(menu-bar-mode
                tool-bar-mode
                scroll-bar-mode
                blink-cursor-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Disable GUI dialogs & useless messages
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t)

;; Ask for "y or n" instead of "yes or no"
(fset 'yes-or-no-p #'y-or-n-p)

;; Show sometimes useful info
(setq-default indicate-buffer-boundaries 'right)
(column-number-mode t)

;; Smooth scrolling
(setq scroll-conservatively 10000
      scroll-margin 5)

;; Show matching parentheses
(show-paren-mode t)
(setq show-paren-delay 0)

;; Show commands as they are typed
(setq echo-keystrokes 0.01)

;; Switch windows with windmove
(use-package windmove
  :bind (("C-c <left>" . windmove-left)
         ("C-c <right>" . windmove-right)
         ("C-c <up>" . windmove-up)
         ("C-c <down>" . windmove-down)))

;; Manipulate buffers and corresponding files
(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(global-set-key (kbd "C-c R") #'rename-file-and-buffer)

(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(global-set-key (kbd "C-c D")  'delete-file-and-buffer)

;; Easily switch between source and headers files in c-modes
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key (kbd "C-c ,") 'ff-find-other-file)))

;;; Editing

(setq-default auto-save-default nil
              make-backup-files nil
              case-fold-search t
              sentence-end-double-space nil
              require-final-newline t)

;; Indentation
(setq-default indent-tabs-mode nil
              tab-width 4
              c-basic-offset 4
              c-default-style "k&r")

(add-hook 'c-mode-common-hook
          #'(lambda ()
            ;; Indent case statements in C modes
            (c-set-offset 'case-label '+)
            ;; Indent from the same level as opening braces
            (c-set-offset 'substatement-open 0)))

;; Delete selected text
(delete-selection-mode)

;; Electric all the things
(use-package electric
  :init (electric-layout-mode))

(use-package elec-pair
  :init (electric-pair-mode))

;; Reload buffers automagically if the corresponding file has been changed
(global-auto-revert-mode)

;; Remove superfluous whitespace upon save
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Hard-wrap lines at 80 columns when editing text files
(setq-default fill-column 80)
(add-hook 'text-mode-hook #'auto-fill-mode)

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

;; Smart beginning of line
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line."
  (interactive "^")
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))
(global-set-key (kbd "<home>") 'smart-beginning-of-line)
(global-set-key (kbd "C-a") 'smart-beginning-of-line)

(defun forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))
(global-set-key (kbd "M-<end>") #'forward-or-backward-sexp)

;; Align according to regexp
(global-set-key (kbd "C-c a") 'align-regexp)

;;; Dired
(use-package dired
  :defer t
  :config
  (progn
    (require 'dired-x)
    (add-hook 'dired-mode-hook #'dired-hide-details-mode)
    (setq-default dired-hide-details-hide-symlink-targets nil
                  dired-omit-mode t
                  dired-omit-files (concat dired-omit-files "\\|^\\..+$")
                  dired-auto-revert-buffer t
                  dired-listing-switches "-alh")))

;;; Eshell

(defun eshell/clear ()
  "Clear the eshell buffer"
  (interactive)
  (let ((eshell-buffer-maximum-lines 0))
    (eshell-truncate-buffer)))

(defun eshell/exit-or-delete ()
  "Mimic shell behaviour: delete char under point or, if there is
  no input, exit eshell"
  (interactive)
  (eshell-skip-prompt)
  (if (= (point) (point-at-eol))
      (progn
        (eshell-life-is-too-much)
        (delete-window))
    (delete-char 1)))

(defun eshell-here ()
  "Opens a new eshell in the directory associated with the current buffer's file.
  Reuses a previously opened eshell if applicable."
  (interactive)
  (let* ((height (/ (window-total-height) 3))
         (current-dir default-directory)
         (eshell-buffer "*eshell*"))
    (when (= (length (window-list)) 1)
      (split-window-vertically (- height)))
    (unless (string= (buffer-name) eshell-buffer)
      (other-window 1)
      (switch-to-buffer eshell-buffer))
    (eshell)
    (unless (string= current-dir default-directory)
      (eshell/cd current-dir)
      (eshell-send-input)
      (eshell/clear)
      (message "%s" (concat "eshell: Changed directory to " current-dir)))))

(use-package eshell
  :bind (("C-c s" . eshell-here))
  :config
  (progn (setq eshell-hist-ignoredups             t
               eshell-history-size                1024
               eshell-save-history-on-exit        t
               eshell-banner-message              ""
               eshell-buffer-maximum-lines        2500
               eshell-scroll-to-bottom-on-output  t
               eshell-scroll-show-maximum-output  t)
         (add-hook 'eshell-mode-hook
                   #'(lambda ()
                       (add-to-list 'eshell-output-filter-functions
                                    'eshell-truncate-buffer 'eshell-handle-ansi-color)
                       (bind-key "C-d" #'eshell/exit-or-delete eshell-mode-map)
                       (bind-key "C-l" #'eshell/clear eshell-mode-map)
                       (bind-key "C-r" #'helm-eshell-history eshell-mode-map)))))

;;; Ediff
(use-package ediff-wind
  :defer t
  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function #'split-window-horizontally))

;;; Misc

;; Always follow symlinks to file in VCS repos
(use-package vc-hooks
  :defer t
  (setq vc-follow-symlinks t))

;; Shell scripts
(use-package sh-script
  :mode ("\\.zsh\\'" . sh-mode))

;;; Backups

;; To keep things clean
(setq user-temp-files-directory (concat user-emacs-directory "temp/"))

;; Keep backup and auto save files out of the way
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t))
      auto-save-list-file-prefix temporary-file-directory)

;; Recent files & history
(use-package recentf
  :defer t
  :idle (recentf-mode)
  :config
  (setq recentf-save-file (concat user-temp-files-directory "recentf")
        recentf-max-menu-items 15
        recentf-max-saved-items 200
        recentf-auto-cleanup 60
        recentf-exclude (append recentf-exclude
                                '("/\\.git/.*\\'"
                                  "/elpa/.*\\'"
                                  "/tmp/.*\\'"
                                  "/var/tmp/.*\\'"))))

(use-package saveplace
  :config
  (progn (setq save-place-file
               (concat user-temp-files-directory "saved-places")
               save-place-forget-unreadable-files t)
         (setq-default save-place t)))

(use-package savehist
  :init (savehist-mode t)
  :config
  (setq savehist-file (concat user-temp-files-directory "history")
        savehist-save-minibuffer-history t))

;;; External packages
(use-package company
  :ensure t
  :defer t
  :init (global-company-mode)
  :config
  (progn (setq company-abort-manual-when-too-short t
               company-selection-wrap-around t
               company-idle-delay 0.25
               company-tooltip-align-annotations t)
         (bind-key [remap completion-at-point] #'company-complete company-mode-map)
         (bind-key "C-n" #'company-select-next company-active-map)
         (bind-key "C-p" #'company-select-previous company-active-map))
  :diminish company-mode)

(use-package company-c-headers
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-c-headers)))

(use-package company-go
  :ensure t
  :commands go-mode
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-go)))

(use-package expand-region
  :ensure t
  :config
  (setq expand-region-contract-fast-key "X")
  :bind (("C-x x" . er/expand-region)))

(use-package flycheck
  :ensure t
  :config
  (progn
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
    (add-hook 'prog-mode-hook #'flycheck-mode)))

(use-package go-mode
  :ensure t
  :commands go-mode)

(use-package helm
  :ensure t
  :bind
  (("C-x b" . helm-buffers-list)
   ("C-x C-f" . helm-find-files)
   ("C-x C-r" . helm-recentf)
   ("C-c i"   . helm-imenu)
   ("C-c h"   . helm-command-prefix)
   ("M-x"     . helm-M-x)
   ("M-y"     . helm-show-kill-ring))
  :diminish helm-mode
  :init
  (helm-mode t)
  :config
  (progn
    (use-package helm-config)
    (global-unset-key (kbd "C-x c"))
    (setq helm-buffers-fuzzy-matching           t
          helm-M-x-fuzzy-match                  t
          helm-imenu-fuzzy-match                t
          helm-buffers-fuzzy-matching           t
          helm-quick-update                     t
          helm-ff-auto-update-initial-value     nil
          helm-split-window-in-side-p           t
          helm-move-to-line-cycle-in-source     t
          helm-scroll-amount                    8
          helm-ff-search-library-in-sexp        t
          helm-ff-file-name-history-use-recentf t
          helm-ff-skip-boring-files             t)
    (bind-key "<tab>" #'helm-execute-persistent-action helm-map)
    (bind-key "C-i" #'helm-execute-persistent-action helm-map)
    (bind-key "C-z" #'helm-select-action helm-map)))

(use-package helm-swoop
  :ensure t
  :bind (("C-c o" . helm-swoop)
         ("C-c O" . helm-multi-swoop-all)))

(use-package helm-flycheck
  :ensure t
  :bind (("C-c f" . helm-flycheck)))

(use-package magit
  :ensure t
  :config (setq magit-restore-window-configuration t)
  :bind (("C-c g" . magit-status))
  :diminish magit-auto-revert-mode)

(use-package magit-filenotify
  :ensure t
  :config (add-hook 'magit-status-mode-hook #'magit-filenotify-mode)
  :diminish magit-filenotify-mode)

(use-package multiple-cursors
  :ensure t
  :bind (("C-," . mc/mark-next-symbol-like-this)
         ("C-*" . mc/mark-more-like-this-extended)
         ("C-;" . mc/mark-next-like-this)))

(use-package tex
  :ensure auctex
  :defer t
  :config
  (setq TeX-parse-self t
        TeX-auto-save t
        TeX-clean-confirm nil
        TeX-PDF-mode t
        TeX-view-program-selection '((output-pdf "PDF Viewer"))
        TeX-view-program-list '(("PDF Viewer" "evince %o"))))

(use-package web-mode
  :ensure t
  :mode
  (("\\.html?\\'" . web-mode)
   ("\\.php\\'" . web-mode)
   ("\\.css\\'" . web-mode))
  :config
  (progn
    (setq web-mode-markup-indent-offset 2
          web-mode-css-indent-offset 2
          web-mode-code-indent-offset 4
          web-mode-enable-auto-expanding t
          web-mode-enable-auto-pairing t
          web-mode-enable-current-element-highlight t)
    (bind-key "M-<up>" #'web-mode-element-previous web-mode-map)
    (bind-key "M-<down>" #'web-mode-element-next web-mode-map)
    (bind-key "M-S-<up>" #'web-mode-element-parent web-mode-map)
    (bind-key "M-<end>" #'web-mode-navigate web-mode-map)
    (bind-key "C-c c" #'web-mode-comment-or-uncomment web-mode-map)
    (bind-key "C-c k" #'web-mode-element-kill web-mode-map)
    (bind-key "C-c v" #'web-mode-element-vanish web-mode-map)
    (bind-key "C-c w" #'web-mode-element-wrap web-mode-map)
    (bind-key "C-c r" #'web-mode-element-rename web-mode-map)
    (bind-key "C-x x" #'web-mode-mark-and-expand web-mode-map)))

(use-package yasnippet
  :ensure t
  :config
  (progn (setq-default yas-prompt-functions
                       (delete 'yas-x-prompt yas-prompt-functions))
         (yas-reload-all)
         (add-hook 'prog-mode-hook 'yas-minor-mode))
  :diminish yas-minor-mode)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
