;; init.el - Emacs startup file
;; Author : Nicolas G.Querol<nicolas.gquerol @gmail.com>

;; Package.el
(require 'package)
(setq package-enable-at-startup nil
      package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu"."https://elpa.gnu.org/packages/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'diminish))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(setq use-package-always-ensure t
      use-package-compute-statistics t)

;;; Locales
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)

;;; Interface

;; Turn off mouse interface
(dolist (mode '(tool-bar-mode scroll-bar-mode blink-cursor-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Disable GUI dialogs & useless messages
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t)

(unless (display-graphic-p)
  (menu-bar-mode -1))

;; Initial frame size
(setq initial-frame-alist '((width . 90)
                            (height . 45)))

;; Display buffer name in frame title
(setq frame-title-format
      (list "%b — " (user-login-name) " @ Emacs " emacs-version))

;; Ask for "y or n" instead of "yes or no"
(fset 'yes-or-no-p #'y-or-n-p)

;; Show sometimes useful info
(add-hook 'prog-mode-hook #'(lambda ()
                              (setq-local indicate-buffer-boundaries 'right)
                              (setq-local indicate-empty-lines t)))

;; Line numbers
(bind-key "C-c n" #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Smooth scrolling
(setq scroll-conservatively 10000
      scroll-margin 5)

;; Highlight current line
(add-hook 'prog-mode-hook #'hl-line-mode)

;; Disable scroll-margin for specific modes
(mapc (lambda (hook) (add-hook hook #'(lambda ()
                                        (setq-local scroll-margin 0))))
      '(comint-mode-hook
        eshell-mode-hook
        messages-buffer-mode-hook
        term-mode-hook))

;; Show commands as they are typed
(setq echo-keystrokes 0.01)

;; No beeping of any sort
(setq ring-bell-function #'ignore)

;; OS-specific settings
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'alt
        mac-command-modifier 'meta
        mac-right-option-modifier 'none)
  (mac-auto-operator-composition-mode)
  (use-package exec-path-from-shell
    :if (display-graphic-p)
    :config (setq exec-path-from-shell-check-startup-files nil)
    (exec-path-from-shell-initialize)))

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
      (let ((new-name (read-file-name "New file name: " filename)))
        (if (vc-backend filename)
            (vc-rename-file filename new-name)
          (progn (rename-file filename new-name t)
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

(setq-default case-fold-search t
              sentence-end-double-space nil
              require-final-newline t)

;; Backup (this is a test)

;; Backup files when they are saved
(setq-default backup-by-copying t
	          delete-old-versions t
	          kept-new-versions 4
	          kept-old-versions 2
	          version-control t
	          vc-make-backup-files t)

;; Auto-save after 20 seconds of inactivity, or after 200 characters typed, if
;; the buffer is modified.
(setq-default auto-save-timeout 20
              auto-save-interval 200)
(auto-save-visited-mode t)

;; Indentation
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Delete selected text
(delete-selection-mode t)

;; Reload buffers automagically if the corresponding file has been changed
(use-package autorevert
  :init (global-auto-revert-mode t)
  :diminish auto-revert-mode)

;; Remove superfluous whitespace upon save
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Hard-wrap lines at 80 columns when editing text files
(setq-default fill-column 80)
(add-hook 'text-mode-hook #'auto-fill-mode)

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

(global-set-key (kbd "C-<return>") #'ngq/smart-open-line)
(global-set-key (kbd "C-S-<return>") #'ngq/smart-open-line-above)

;; Move current line up or down
(defun ngq/move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun ngq/move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "C-S-<up>") #'ngq/move-line-up)
(global-set-key (kbd "C-S-<down>") #'ngq/move-line-down)

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
  :ensure nil
  :commands dired
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . dired-omit-mode))
  :config (setq-default dired-dwim-target t
                        dired-hide-details-hide-symlink-targets nil
                        dired-auto-revert-buffer t
                        dired-isearch-filenames t
                        dired-use-ls-dired nil
                        dired-listing-switches "-alGh"))

(use-package wdired
  :after dired
  :bind (:map dired-mode-map ("C-c C-e" . wdired-change-to-wdired-mode))
  :config (setq wdired-use-dired-vertical-movement 'sometimes
                wdired-confirm-overwrite t
                wdired-use-interactive-rename t))

(use-package dired-x
  :after dired
  :ensure nil
  :config (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$")))

(use-package eshell
  :preface
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

  (defun ngq/eshell-clear ()
    "Clear eshell's buffer"
    (interactive)
    (let ((eshell-buffer-maximum-lines 0))
      (eshell-truncate-buffer)))

  (defun ngq/eshell-here ()
    "Opens eshell in the directory associated with the current buffer's file.

Reuses a previously opened eshell buffer if possible. "
    (interactive)
    (let ((current-directory (expand-file-name default-directory))
          (eshell-buffer-height (/ (window-total-height) 4))
          (eshell-buffer-name "*eshell*"))
      (if-let (eshell-buffer (get-buffer eshell-buffer-name))
          (progn (select-window (get-buffer-window eshell-buffer))
                 (switch-to-buffer eshell-buffer))
        (progn (select-window (split-window-below (- eshell-buffer-height)))
               (eshell)))
      (message current-directory)
      (unless (string= current-directory default-directory)
        (eshell/cd current-directory)
        (eshell-send-input)
        (ngq/eshell-clear)
        (message "eshell: Changed directory to %s" current-directory))))
  :bind (("C-c s" . ngq/eshell-here))
  :init (add-hook 'eshell-first-time-mode-hook
                  #'(lambda () (add-to-list 'eshell-output-filter-functions
                                            'eshell-postoutput-scroll-to-bottom
                                            'eshell-truncate-buffer)
                      (setq-local indicate-buffer-boundaries nil)
                      (setq-local indicate-empty-lines nil)))
  (add-hook 'eshell-mode-hook
            #'(lambda () (progn
                           (bind-key "C-d" #'ngq/eshell-exit-or-delete eshell-mode-map)
                           (bind-key "C-l" #'ngq/eshell-clear eshell-mode-map))))
  :config (setq-default eshell-hist-ignoredups             t
                        eshell-history-size                1024
                        eshell-save-history-on-exit        t
                        eshell-banner-message              ""
                        eshell-prefer-lisp-functions       nil
                        eshell-buffer-maximum-lines        2500
                        eshell-scroll-to-bottom-on-input   'all
                        eshell-scroll-to-bottom-on-output  t
                        eshell-scroll-show-maximum-output  t))

;; Recent files & history
(use-package recentf
  :config (setq recentf-max-menu-items 15
                recentf-max-saved-items 200
                recentf-auto-cleanup 60
                recentf-exclude (append recentf-exclude
                                        '("/\\.git/.*\\'"
                                          "/elpa/.*\\'"
                                          "/tmp/.*\\'"
                                          "/var/tmp/.*\\'"))))

(use-package saveplace
  :hook (after-init . save-place-mode)
  :config (setq-default save-place-forget-unreadable-files t))

(use-package savehist
  :hook (after-init . savehist-mode)
  :config (setq savehist-save-minibuffer-history t
                savehist-autosave-interval 60))

;;; Ediff
(use-package ediff-wind
  :ensure nil
  :commands (ediff)
  :config (setq ediff-window-setup-function #'ediff-setup-windows-plain
                ediff-split-window-function #'split-window-horizontally))

;;; Misc

;; Hide eldoc-mode in the modeline
(setq eldoc-minor-mode-string nil)

;; Always follow symlinks to file in VCS repos
(use-package vc-hooks
  :ensure nil
  :config (setq vc-follow-symlinks t
                find-file-visit-truename t))

;; Kill the current buffer without confirmation
(global-set-key (kbd "C-x S-k") #'kill-this-buffer)

;; Shell scripts
(use-package sh-script
  :mode ("\\.zsh\\'" . sh-mode))

;; C modes common settings
(use-package cc-mode
  :bind (:map c-mode-map ("C-c ," . ff-find-other-file))
  :config
  (setq c-default-style "linux"
        c-basic-offset 4)
  (c-set-offset 'case-label '+)
  (c-set-offset 'substatement-open 0))

;; CSS mode
(use-package css-mode
  :mode "\\.css\\'"
  :config (setq css-indent-offset 2))

;;; External packages

;; Organize trash files
(use-package no-littering
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

;; Traverse backed-up versions of a file
(use-package backup-walker
  :bind (("C-c b" . backup-walker-start)))

;; Reuse the current dired buffer to visit a directory
(use-package dired-single
  :after dired
  :bind (:map dired-mode-map
              ([return] . dired-single-buffer)
              ([mouse-1] . dired-single-buffer-mouse)
              ([remap dired-up-directory] . (lambda () (interactive)
                                              (dired-single-buffer "..")))))

;; Incremental and narrowing framework
(use-package helm
  :defines helm-boring-buffer-regexp-list
  :bind (("C-c i" . helm-imenu)
         ("C-c o" . helm-occur)
         ("C-c m" . helm-man-woman)
         ("C-c r" . helm-resume)
         ("C-x C-r" . helm-recentf)
         ("M-y" . helm-show-kill-ring)
         ("C-x C-f" . helm-find-files)
         ("C-x C-b" . helm-buffers-list)
         ("C-x b" . helm-mini)
         ("M-x" . helm-M-x)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-z" . helm-select-action)
         :map isearch-mode-map
         ("C-o" . helm-occur-from-isearch))
  :init (progn (helm-mode t)
               (helm-adaptive-mode t)
               (helm-autoresize-mode t))
  :config (setq-default helm-candidate-number-limit 100
                        helm-mode-fuzzy-match t
                        helm-completion-in-region-fuzzy-match t
                        helm-display-header-line nil
                        helm-split-window-inside-p t
                        helm-always-two-windows t
                        helm-move-to-line-cycle-in-source t
                        helm-ff-skip-boring-files t
                        helm-ff-file-name-history-use-recentf t
                        helm-ff-auto-update-initial-value t
                        helm-ff-newfile-prompt-p nil
                        helm-autoresize-min-height 10
                        helm-autoresize-max-height 40
                        helm-boring-buffer-regexp-list (append
                                                        helm-boring-buffer-regexp-list
                                                        '("\\*WoMan-Log*"
                                                          "\\*Man "
                                                          "\\*Colors*"
                                                          "\\*Help*"
                                                          "\\*code-conversion-work*"
                                                          "\\*tramp"
                                                          "\\*epc"
                                                          "\\*Completions"
                                                          "\\*Compile-Log"
                                                          "\\*clang-output"
                                                          "\\*clang-error"
                                                          "\\*magit"))
                        helm-boring-file-regexp-list (append
                                                      helm-boring-file-regexp-list
                                                      '("\\.DS_Store$")))
  :diminish helm-mode)

;; Integrate Helm with eshell
(use-package helm-eshell
  :after (helm eshell)
  :ensure nil
  :init (add-hook 'eshell-mode-hook #'(lambda ()
                                        (bind-key "C-r" #'helm-eshell-history eshell-mode-map)))
  :config (setq helm-eshell-fuzzy-match t))


;; Projectile

(use-package projectile
  :defer t
  :bind (("C-c p" . projectile-command-map))
  :init (projectile-mode)
  :config (setq projectile-find-dir-includes-top-level t))

(use-package helm-projectile
  :after (helm projectile)
  :config
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))

;; Increase selected region by semantic units
(use-package expand-region
  :bind (("C-x x" . er/expand-region))
  :config (setq expand-region-contract-fast-key "X"))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Multiple cursors for Emacs
(use-package multiple-cursors
  :bind (("C-," . mc/mark-next-symbol-like-this)
         ("C-*" . mc/mark-more-like-this-extended)
         ("C-;" . mc/mark-next-like-this)))

;; Operate on current line if region is undefined
(use-package whole-line-or-region
  :init (whole-line-or-region-global-mode t)
  :diminish whole-line-or-region-local-mode)

;; Automatic insertion, wrapping and paredit-like navigation with user defined pairs
(use-package smartparens
  :hook ((lisp-mode . smartparens-strict-mode)
         (emacs-lisp-mode . smartparens-strict-mode)
         (cider-mode . smartparens-strict-mode)
         (cider-repl-mode . smartparens-strict-mode)
         (prog-mode . smartparens-global-mode)
         (prog-mode . show-smartparens-global-mode))
  :bind (("C-M-f" . sp-forward-sexp)
         ("C-M-b" . sp-backward-sexp)
         ("C-M-d" . sp-down-sexp)
         ("C-M-a" . sp-backward-down-sexp)
         ("C-S-d" . sp-beginning-of-sexp)
         ("C-S-a" . sp-end-of-sexp)
         ("C-M-e" . sp-up-sexp)
         ("C-M-u" . sp-backward-up-sexp)
         ("C-M-t" . sp-transpose-sexp)
         ("C-M-n" . sp-next-sexp)
         ("C-M-p" . sp-previous-sexp)
         ("C-M-k" . sp-kill-sexp)
         ("C-M-w" . sp-copy-sexp)
         ("M-<delete>" . sp-unwrap-sexp)
         ("M-<backspace>" . sp-backward-unwrap-sexp)
         ("C-M-<left>" . sp-backward-slurp-sexp)
         ("C-M-<right>" . sp-backward-barf-sexp)
         ("M-D" . sp-splice-sexp)
         ("C-M-<delete>" . sp-splice-sexp-killing-forward)
         ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
         ("C-S-<backspace>" . sp-splice-sexp-killing-around)
         ("C-]" . sp-select-next-thing-exchange)
         ("C-<left_bracket>" . sp-select-previous-thing)
         ("C-M-]" . sp-select-next-thing)
         ("M-F" . sp-forward-symbol)
         ("M-B" . sp-backward-symbol)
         ("C-\"" . sp-change-inner))
  :config (progn (require 'smartparens-config)
                 (require 'smartparens-html)
                 (sp-with-modes '(c-mode c++-mode js2-mode web-mode)
                   (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
                   (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                                             ("* ||\n[i]" "RET")))))
  :diminish smartparens-mode)

;; Modular text completion framework
(use-package company
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :hook ((prog-mode eshell-mode cider-repl-mode) . company-mode)
  :preface (defun company-mode/backend-with-yas (backend)
             (if (and (listp backend) (member 'company-yasnippet backend))
                 backend
               (append (if (consp backend) backend (list backend))
                       '(:with company-yasnippet))))
  :config
  (setq company-minimum-prefix-length 2
        company-backends '(company-css
                           company-clang
                           company-cmake
                           company-files
                           company-capf
                           company-keywords)
        company-backends (mapcar #'company-mode/backend-with-yas
                                 company-backends)
        company-transformers '(company-sort-by-occurrence))
  :diminish company-mode)

(use-package company-quickhelp
  :disabled ; doesn't work in emacs-mac port
  :after company
  :init (company-quickhelp-mode)
  :config (setq company-quickhelp-max-lines 20
                company-quickhelp-delay 1.5
                company-quickhelp-use-propertized-text t))

;; On-the-fly syntax checking
(use-package flycheck
  :hook ((prog-mode LaTeX-mode) . flycheck-mode)
  :config (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package flycheck-pos-tip
  :after flycheck
  :hook (flycheck-mode . flycheck-pos-tip-mode)
  :config (setq flycheck-pos-tip-max-width 100))

;; Integrate Helm with flycheck
(use-package helm-flycheck
  :after (helm flycheck)
  :bind (("C-c f" . helm-flycheck)))

;; A Git porcelain inside Emacs
(use-package magit
  :bind (("C-c g" . magit-status))
  :config (setq-default magit-restore-window-configuration t))

;; Yet another snippet extension for Emacs
(use-package yasnippet-snippets)

(use-package yasnippet
  :after yasnippet-snippets
  :hook (prog-mode . yas-minor-mode)
  :config (yas-reload-all)
  :diminish yas-minor-mode)

;; C/C++
(use-package clang-format
  :if (executable-find "clang-format")
  :after cc-mode
  :init (dolist (mode '(c-mode-hook c++-mode-hook))
          (add-hook mode (lambda ()
                           (add-hook 'before-save-hook
                                     #'clang-format-buffer nil t)))))

(use-package irony
  :hook ((c++-mode . irony-mode)
         (c-mode . irony-mode)
         (irony-mode . irony-cdb-autosetup-compile-options))
  :config (setq irony-server-install-prefix irony-user-dir)
  :diminish irony-mode)

(use-package irony-eldoc
  :after irony
  :hook (irony-mode . irony-eldoc))

(use-package company-irony
  :after (irony company)
  :config (with-eval-after-load 'company
            (add-to-list 'company-backends 'company-irony)))

(use-package company-irony-c-headers
  :after (irony company)
  :config (with-eval-after-load 'company
            (add-to-list 'company-backends 'company-irony-c-headers)))

;; TeX
(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook (LaTeX-mode . visual-line-mode)
  :config
  (setq TeX-parse-self t
        TeX-auto-save t
        TeX-clean-confirm nil
        TeX-view-program-selection '((output-pdf "Preview"))
        TeX-view-program-list '(("Preview" "open -a Preview.app %o"))))

;; Web
(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.php\\'" . web-mode)
         ("\\.eex\\'" . web-mode))
  :bind (:map web-mode-map
              ("M-<up>" . web-mode-element-previous)
              ("M-<down>" . web-mode-element-next)
              ("M-S-<up>" .  web-mode-element-parent)
              ("M-<end>" . web-mode-navigate)
              ("C-c c" . web-mode-comment-or-uncomment )
              ("C-c k" . web-mode-element-kill )
              ("C-c v" . web-mode-element-vanish)
              ("C-c w" . web-mode-element-wrap)
              ("C-c r" . web-mode-element-rename))
  :init (add-hook 'web-mode-hook #'toggle-truncate-lines)
  :config (setq web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2
                web-mode-enable-auto-expanding t
                web-mode-enable-auto-pairing t
                web-mode-enable-current-element-highlight t))

(use-package company-web
  :after (company web-mode)
  :config (with-eval-after-load 'company
            (add-to-list 'company-backends 'company-web-html)))

;; Javascript
(use-package js2-mode
  :mode "\\.js\\'"
  :bind (:map js2-mode-map ("M-j" . ngq/my-join-line))
  :config
  (setq-default js2-basic-offset 2
                js2-basic-indent 2
                js2-auto-indent-p t
                js2-cleanup-whitespace t
                js2-enter-indents-newline t
                js2-indent-on-enter-key t
                js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil))

(use-package tern
  :if (executable-find "tern")
  :bind (:map tern-mode-keymap
              ([remap tern-rename-variable] . ngq/tern-rename-variable))
  :hook js2-mode
  :preface
  (defun ngq/tern-rename-variable ()
    "Wraps `tern-rename-variable' to provide the `symbol-at-point' as the
default input and avoid useless renaming in case the new variable name is the
same as the old one."
    (interactive)
    (let* ((old-var-name (symbol-name (symbol-at-point)))
           (new-var-name (read-string "New variable name: " old-var-name)))
      (if (not (string= old-var-name new-var-name))
          (tern-rename-variable new-var-name)
        (message "Did not rename variable; New name matches the old one.")))))

(use-package company-tern
  :after company tern
  :config (with-eval-after-load 'company
            (add-to-list 'company-backends 'company-tern)))

;; Python
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config (when (executable-find "ipython")
            (setq python-shell-interpreter "ipython"
                  python-shell-interpreter-args "--simple-prompt -i")))

(use-package py-autopep8
  :after python
  :hook (python-mode . py-autopep8-enable-on-save))

(use-package anaconda-mode
  :after python
  :hook python-mode
  :diminish anaconda-mode)

(use-package company-anaconda
  :after python
  :config (with-eval-after-load 'company
            (add-to-list 'company-backends 'company-anaconda)))

;; Clojure

(use-package clojure-mode
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljs\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode))
  :hook (clojure-mode . smartparens-strict-mode))

(use-package clojure-mode-extra-font-locking
  :after clojure-mode)

(use-package clj-refactor
  :after clojure-mode
  :hook (clojure-mode . clj-refactor-mode)
  :config (setq cljr-warn-on-eval nil)
  :diminish clj-refactor-mode)

(use-package cljr-helm
  :after clj-refactor helm
  :bind (:map clojure-mode-map
              ("C-c C-r" . cljr-helm)
              :map clojurescript-mode-map
              ("C-c C-r" . cljr-helm)))

(use-package cider
  :after clojure-mode
  :hook ((cider-mode . eldoc-mode)
         (cider-mode . cider-company-enable-fuzzy-completion)
         (cider-repl-mode . cider-company-enable-fuzzy-completion))
  :bind (:map cider-repl-mode-map ("C-c C-l" . cider-repl-clear-buffer))
  :config (setq cider-repl-use-pretty-printing t
                cider-repl-use-clojure-font-lock t
                cider-repl-result-prefix ";; => "
                cider-font-lock-dynamically '(macro core function var)
                cider-overlays-use-font-lock t
                cider-show-error-buffer nil
                cider-repl-display-help-banner nil
                nrepl-hide-special-buffers t))

(use-package flycheck-clojure
  :disabled
  :after clojure-mode
  :config (flycheck-clojure-setup))

;; Misc editing modes
(use-package json-mode
  :mode "\\.json\\'"
  :config (setq-default json-reformat:indent-width 2))

(use-package yaml-mode
  :mode "\\.y(a)?ml\\'"
  :config (add-hook 'yaml-mode-hook #'(lambda () (auto-fill-mode -1))))

(use-package markdown-mode
  :mode ("\\.\\(markdown\\|md\\)\\'" . gfm-mode))

;; Aggressive-indent
(use-package aggressive-indent
  :hook ((emacs-lisp-mode lisp-mode clojure-mode) . aggressive-indent-mode)
  :diminish aggressive-indent-mode)

;; Which-key
(use-package which-key
  :init (which-key-mode)
  :diminish (which-key-mode))

;; Highlight VCS changes

(use-package diff-hl
  :hook ((diff-hl-mode . diff-hl-dired-mode)
         (diff-hl-mode . diff-hl-flydiff-mode))
  :init (global-diff-hl-mode t)
  :config (setq diff-hl-draw-borders nil))

;; Load custom settings, if they do exist
(let ((user-local-file (concat user-emacs-directory "local.el")))
  (when (file-readable-p user-local-file)
    (load-file user-local-file)))

;; Settings handled by Custom.el
(let ((user-custom-file (concat user-emacs-directory "custom.el")))
  (setq custom-file user-custom-file)
  (when (file-readable-p user-custom-file)
    (load user-custom-file)))
