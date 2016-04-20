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

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;;; Locales
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)

;;; Interface

;; Turn off mouse interface
(dolist (mode '(tool-bar-mode
                scroll-bar-mode
                blink-cursor-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Disable GUI dialogs & useless messages
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t)

;; Display buffer name in frame title
(setq frame-title-format
      (list "%b — " (user-login-name) " @ Emacs " emacs-version))

;; Ask for "y or n" instead of "yes or no"
(fset 'yes-or-no-p #'y-or-n-p)

;; Show sometimes useful info
(setq-default indicate-buffer-boundaries 'right)
(column-number-mode t)

;; Smooth scrolling
(setq scroll-conservatively 10000
      scroll-margin 5)

;; Show commands as they are typed
(setq echo-keystrokes 0.01)

;; No beeping of any sort
(setq ring-bell-function #'ignore)

;; OS-specific settings
(if (eq system-type 'darwin)
    (progn (use-package exec-path-from-shell
             :ensure t
             :init (exec-path-from-shell-initialize))
           (setq mac-option-modifier 'alt
                 mac-command-modifier 'meta
                 mac-right-option-modifier 'none)
           ))

;; Switch windows with windmove
(use-package windmove
  :bind (("C-c <left>" . windmove-left)
         ("C-c <right>" . windmove-right)
         ("C-c <up>" . windmove-up)
         ("C-c <down>" . windmove-down)))

;; Manipulate buffers and corresponding files
(defun ngq/rename-file-and-buffer ()
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

(global-set-key (kbd "C-c R") #'ngq/rename-file-and-buffer)

(defun ngq/delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (y-or-n-p (format "Really delete %s ?" filename))
          (if (vc-backend filename)
              (vc-delete-file filename)
            (progn
              (delete-file filename)
              (message "Deleted file %s" filename)
              (kill-buffer)))))))

(global-set-key (kbd "C-c D") #'ngq/delete-file-and-buffer)

;;; Editing
(setq-default auto-save-default nil
              make-backup-files nil
              case-fold-search t
              sentence-end-double-space nil
              require-final-newline t)

(global-prettify-symbols-mode t)

;; Indentation
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Delete selected text
(delete-selection-mode t)

;; Reload buffers automagically if the corresponding file has been changed
(global-auto-revert-mode t)

;; Remove superfluous whitespace upon save
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Hard-wrap lines at 80 columns when editing text files
(setq-default fill-column 80)
(add-hook 'text-mode-hook #'auto-fill-mode)

;; Sentences are delimited by a single space
(setq sentence-end-double-space nil)

;; Automagically indent when inserting a newline
(global-set-key (kbd "RET") #'reindent-then-newline-and-indent)

;; Faster repeat
(global-set-key (kbd "C-z") #'repeat)

;; Join line
(defun ngq/my-join-line ()
  (interactive)
  (join-line -1))

(global-set-key (kbd "M-j") #'ngq/my-join-line)

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

(global-set-key (kbd "M-o") #'ngq/smart-open-line)
(global-set-key (kbd "M-O") #'ngq/smart-open-line-above)

;; Smart beginning of line
(defun ngq/smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line."
  (interactive "^")
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))
(global-set-key (kbd "<home>") #'ngq/smart-beginning-of-line)
(global-set-key (kbd "C-a") #'ngq/smart-beginning-of-line)

;; Comment what I really mean
(defun ngq/comment-dwim-line (&optional arg)
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
(global-set-key (kbd "C-c c") #'ngq/comment-dwim-line)

;; Align according to regexp
(global-set-key (kbd "C-c a") #'align-regexp)

;;; Dired
(use-package dired
  :init (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  :config (require 'dired-x)
  (setq-default dired-hide-details-hide-symlink-targets nil
                dired-omit-mode t
                dired-omit-files (concat dired-omit-files "\\|^\\..+$")
                dired-auto-revert-buffer t
                dired-listing-switches "-alh"))

(use-package wdired
  :init
  (bind-key "C-c C-e" #'wdired-change-to-wdired-mode dired-mode-map)
  :config (setq wdired-use-dired-vertical-movement 'sometimes
                wdired-confirm-overwrite t
                wdired-use-interactive-rename t))

(use-package dired+
  :after dired
  :ensure t
  :config (toggle-diredp-find-file-reuse-dir t))

;;; Eshell
(use-package eshell
  :bind (("C-c s" . ngq/eshell-here))
  :init (defun ngq/eshell-clear ()
          "Clear the eshell buffer"
          (interactive)
          (let ((eshell-buffer-maximum-lines 0))
            (eshell-truncate-buffer)))

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

  (defun ngq/eshell-here ()
    "Opens a new eshell in the directory associated with the current buffer's
  file. Reuses a previously opened eshell if applicable."
    (interactive)
    (let* ((height (/ (window-total-height) 4))
           (current-dir default-directory)
           (eshell-buffer "*eshell*"))
      (unless (string= (buffer-name) eshell-buffer)
        (when (= (length (window-list)) 1)
          (split-window-vertically (- height)))
        (other-window 1)
        (if (get-buffer eshell-buffer)
            (switch-to-buffer eshell-buffer)
          (eshell)))
      (unless (string= current-dir default-directory)
        (eshell/cd current-dir)
        (eshell-send-input)
        (eshell/clear)
        (message "eshell: Changed directory to %s" current-dir))))

  (add-hook 'eshell-mode-hook
            #'(lambda () (progn (add-to-list 'eshell-output-filter-functions
                                        'eshell-truncate-buffer
                                        'eshell-handle-ansi-color)
                           (bind-key "C-d" #'ngq/eshell-exit-or-delete eshell-mode-map)
                           (bind-key "C-l" #'ngq/eshell-clear eshell-mode-map))))
  :config (setq-default eshell-hist-ignoredups             t
                        eshell-history-size                1024
                        eshell-save-history-on-exit        t
                        eshell-banner-message              ""
                        eshell-buffer-maximum-lines        2500
                        eshell-scroll-to-bottom-on-output  t
                        eshell-scroll-show-maximum-output  t))

;;; Ediff
(use-package ediff-wind
  :config (setq ediff-window-setup-function #'ediff-setup-windows-plain
                ediff-split-window-function #'split-window-horizontally))

;;; Misc

;; Always follow symlinks to file in VCS repos
(use-package vc-hooks
  :defer (setq vc-follow-symlinks t
               find-file-visit-truename t))

;; Shell scripts
(use-package sh-script
  :mode ("\\.zsh\\'" . sh-mode))

;;; Backups

(defvar user-temp-files-directory (concat user-emacs-directory ".cache/")
  "Directory where temporary (but persistent accross sessions) files reside.")

;; Keep backup and auto save files out of the way
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t))
      auto-save-list-file-prefix temporary-file-directory)

;; Recent files & history
(use-package recentf
  :config (setq recentf-save-file (concat user-temp-files-directory "recentf-list")
                recentf-max-menu-items 15
                recentf-max-saved-items 200
                recentf-auto-cleanup 60
                recentf-exclude (append recentf-exclude '("/\\.git/.*\\'"
                                                          "/elpa/.*\\'"
                                                          "/tmp/.*\\'"
                                                          "/var/tmp/.*\\'"))))

(use-package saveplace
  :config (setq-default save-place-file (concat user-temp-files-directory
                                                "saved-places")
                        save-place-forget-unreadable-files t
                        save-place t))

(use-package savehist
  :init (add-hook 'after-init-hook #'savehist-mode)
  :config (setq savehist-file (concat user-temp-files-directory
                                      "minibuffer-history")
                savehist-save-minibuffer-history t
                savehist-autosave-interval 60))

;; C modes common settings
(defun ngq/c-mode-setup ()
  (setq-default c-basic-offset 4
                c-default-style "linux")
  (define-key 'c-mode-map "C-c ," #'ff-find-other-file)
  (c-set-offset 'case-label '+)
  (c-set-offset 'substatement-open 0))

(add-hook 'c-mode-common-hook #'ngq/c-mode-setup)

;;; External packages
(use-package aggressive-indent
  :ensure t
  :init (add-hook 'prog-mode-hook #'aggressive-indent-mode)
  :diminish aggressive-indent-mode)

(use-package anzu
  :ensure t
  :bind (([remap query-replace-regexp] . anzu-query-replace-regexp)
         ([remap query-replace] . anzu-query-replace))
  :init (add-hook 'after-init-hook #'global-anzu-mode)
  :config (setq anzu-deactivate-region t
                anzu-replace-to-string-separator " => ")
  :diminish (anzu-mode))

(use-package company
  :ensure t
  :init (add-hook 'prog-mode-hook #'company-mode)
  (add-hook 'eshell-mode-hook #'company-mode)
  :bind (:map company-mode-map
              ([remap completion-at-point] . company-complete)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :config (defun company-mode/backend-with-yas (backend)
            (if (and (listp backend) (member 'company-yasnippet backend))
                backend
              (append (if (consp backend) backend (list backend))
                      '(:with company-yasnippet))))

  (setq company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-tooltip-flip-when-above t
        company-tooltip-align-annotations t
        company-backends '(company-css
                           company-clang
                           company-cmake
                           company-files
                           company-capf
                           company-keywords)
        company-backends (mapcar #'company-mode/backend-with-yas
                                 company-backends))
  :diminish company-mode)

(use-package company-c-headers
  :after company
  :ensure t
  :config (add-hook 'c-mode-hook #'(lambda () (add-to-list 'company-backends
                                                      'company-c-headers))))

(use-package company-web
  :after company
  :ensure t
  :config (add-hook 'web-mode-hook #'(lambda ()
                                       (add-to-list 'company-backends
                                                    'company-web-html))))

(use-package expand-region
  :ensure t
  :config (setq expand-region-contract-fast-key "X")
  :bind (("C-x x" . er/expand-region)))

(use-package flycheck
  :ensure t
  :init (add-hook 'prog-mode-hook #'flycheck-mode)
  (add-hook 'LaTeX-mode-hook #'flycheck-mode)
  :config (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package helm
  :ensure t
  :bind (("C-c i" . helm-imenu)
         ("C-x C-r" . helm-recentf)
         ("M-y" . helm-show-kill-ring)
         ([remap find-file] . helm-find-files)
         ([remap list-buffers] . helm-buffers-list)
         ([remap switch-to-buffer] . helm-mini)
         ([remap execute-extended-command] . helm-M-x)
         (:map helm-map
               ("<tab>" . helm-execute-persistent-action)
               ("C-z" . helm-select-action))
         (:map eshell-mode-map
               ([remap eshell-pcomplete] . helm-esh-pcomplete)
               ("C-r" . helm-eshell-history)))
  :init (add-hook 'helm-mode-hook #'helm-autoresize-mode)
  (helm-mode t)
  :config (setq-default helm-buffers-fuzzy-matching t
                        helm-M-x-fuzzy-match t
                        helm-recentf-fuzzy-match t
                        helm-imenu-fuzzy-match t
                        helm-quick-update t
                        helm-truncate-lines t
                        helm-display-header-line nil
                        helm-move-to-line-cycle-in-source t
                        helm-ff-skip-boring-files t
                        helm-ff-file-name-history-use-recentf t
                        helm-ff-auto-update-initial-value t
                        helm-ff-newfile-prompt-p nil
                        helm-boring-buffer-regexp-list (append helm-boring-buffer-regexp-list
                                                               '("\\*tramp"
                                                                 "\\*epc"
                                                                 "\\*Completions"
                                                                 "\\*Compile-Log"
                                                                 "\\*clang-output"
                                                                 "\\*clang-error"
                                                                 "\\*magit"
                                                                 "\\*terminal-notifier"))
                        helm-boring-file-regexp-list (append helm-boring-file-regexp-list
                                                             '("\\.DS_Store$")))
  :diminish helm-mode)

(use-package helm-flycheck
  :after helm
  :ensure t
  :bind (("C-c f" . helm-flycheck)))

(use-package magit
  :ensure t
  :config (setq-default magit-restore-window-configuration t)
  :bind (("C-c g" . magit-status)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-," . mc/mark-next-symbol-like-this)
         ("C-*" . mc/mark-more-like-this-extended)
         ("C-;" . mc/mark-next-like-this)))

(use-package rainbow-delimiters
  :ensure t
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode-enable))

(use-package smartparens-config
  :ensure smartparens
  :init (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (dolist (hook '(inferior-emacs-lisp-mode-hook
                  emacs-lisp-mode-hook
                  racket-mode-hook))
    (add-hook hook #'smartparens-strict-mode))
  :config (sp-local-pair 'cc-mode "{" nil :post-handlers '(("||\n[i]" "RET")))
  :diminish smartparens-mode)

(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :config (add-hook 'LaTeX-mode-hook #'visual-line-mode)
  (setq TeX-parse-self t
        TeX-auto-save t
        TeX-clean-confirm nil
        TeX-view-program-selection '((output-pdf "Preview"))
        TeX-view-program-list '(("Preview" "open -a Preview.app %o"))))

(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.php\\'" . web-mode))
  :bind (:map web-mode-map
              ("M-<up>" . web-mode-element-previous)
              ("M-<down>" . web-mode-element-next)
              ("M-S-<up>" .  web-mode-element-parent)
              ("M-<end>" . web-mode-navigate)
              ("C-c c" . web-mode-comment-or-uncomment )
              ("C-c k" . web-mode-element-kill )
              ("C-c v" . web-mode-element-vanish)
              ("C-c w" . web-mode-element-wrap)
              ("C-c r" . web-mode-element-rename)
              ("C-x x" . web-mode-mark-and-expand))
  :config (setq web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 4
                web-mode-enable-auto-expanding t
                web-mode-enable-auto-pairing t
                web-mode-enable-current-element-highlight t))

(use-package whole-line-or-region
  :ensure t
  :config (add-hook 'after-init-hook #'whole-line-or-region-mode)
  :diminish whole-line-or-region-mode)

(use-package yasnippet
  :ensure t
  :init (add-hook 'prog-mode-hook #'yas-minor-mode)
  :config
  (setq yas-snippet-dirs yas-installed-snippets-dir
        yas-verbosity 2)
  (yas-reload-all)
  :diminish yas-minor-mode)

;; Load custom settings, if they do exist
(let ((local-file (concat user-emacs-directory "local.el")))
  (if (file-readable-p local-file)
      (load-file local-file)))

;;; init.el ends here
