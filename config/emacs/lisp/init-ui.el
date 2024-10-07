;;; init-ui.el --- Emacs UI interaction -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; Customizations pertaining to interacting with Emacs itself, whether in a
;; terminal or in graphical session, independent of host OS.

;;; Code:

;; Set frame title
(setq frame-title-format '((:eval
                            (if-let ((file-name (buffer-file-name)))
                                (concat
                                 file-name
                                 (cond
                                  (buffer-read-only " (read-only)")
                                  ((buffer-modified-p) "*")))
                              "%b"))
                           " — "
                           invocation-name
                           " "
                           emacs-version
                           " @ "
                           (:eval (system-name))))

;; Ask for "y or n" instead of "yes or no"
(when (boundp 'use-short-answers)
  (fset 'yes-or-no-p #'y-or-n-p)
  (setq use-short-answers t))

;; Do not try to use the default font when displaying symbols
(setq use-default-font-for-symbols nil)

(setq help-window-select t ;; Select help windows upon opening
      echo-keystrokes 0.02 ;; Show commands as they are typed
      ring-bell-function #'ignore ;; No beeping of any sort
      delete-by-moving-to-trash t ;; Do not delete files permanently by default
      ;; Scroll one mine at a time by default, three at a time when
      ;; pressing shift, and +/- frame font size when pressing control
      mouse-wheel-scroll-amount '(1 ((shift) . 3)
                                    ((meta))
                                    ((control) . text-scale)))

(defun ngq/prog-mode-hook ()
  "Common `prog-mode' configuration."
  (when (display-graphic-p)
    (setq-local indicate-buffer-boundaries 'right)
    (hl-line-mode t)))

(add-hook 'prog-mode-hook #'ngq/prog-mode-hook)

;; Do case-sensitive searches, unless input has mixed case
(setq case-fold-search t)

;; Show search matches count
(setq isearch-lazy-count t)

;; Stay at the end of a line when moving from another,
;; except when the line is truncated due to its length
(setq track-eol t
      line-move-visual 'goal-column)

;; Add a little more breathing room between lines
(setq-default line-spacing 1)

;; Pop mark more conveniently
(setq set-mark-command-repeat-pop t)

;; Use view mode for read-only buffers.
(setq view-read-only t)

;; No blinking cursor
(blink-cursor-mode -1)

;; Show current line, column and next-buffer size in modeline
(keymap-global-set "C-c n" #'display-line-numbers-mode)
(setq mode-line-position-column-format nil)
(column-number-mode t)
(size-indication-mode t)

;; Show tabs for window-local buffers
(use-package tab-line
  :disabled t
  :ensure nil
  :if (display-graphic-p)
  :hook (after-init . global-tab-line-mode)
  :config
  (setq tab-line-new-button-show nil
        tab-line-close-button-show nil
        tab-line-tabs-function #'tab-line-tabs-window-buffers
        tab-line-separator " "
        tab-line-tab-face-functions '(tab-line-tab-face-special
                                      tab-line-tab-face-modified
                                      tab-line-tab-face-inactive-alternating))
  (dolist (mode '(ediff-mode
                  eshell-mode
                  process-menu-mode
                  term-mode
                  compilation-mode
                  help-mode
                  Man-mode
                  special-mode))
    (add-to-list 'tab-line-exclude-modes mode)))

;; Nicer scrolling
(setq scroll-conservatively 101
      scroll-margin 5
      scroll-preserve-screen-position t)

(when (display-graphic-p)
  (add-hook 'after-init-hook #'pixel-scroll-mode))

;; Disable scroll-margin for specific modes
(mapc (lambda (hook) (add-hook hook (lambda () (setq-local scroll-margin 0))))
      '(compilation-mode-hook
        comint-mode-hook
        eshell-mode-hook
        messages-buffer-mode-hook
        term-mode-hook))

;; Faster repeat last command
(keymap-global-set "C-z" #'repeat)

;; Do not delete files immediately
(setq delete-by-moving-to-trash t)

;; Disable auto save and backups files
(setq auto-save-default nil
      make-backup-files nil)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Show documentation in the minibuffer
(use-package eldoc
  :ensure nil
  :config (setq eldoc-echo-area-use-multiline-p nil))

;; Remember recently visited files
(use-package recentf
  :ensure nil
  :hook (elpaca-after-init . recentf-mode)
  :config (setq recentf-max-saved-items 200
                recentf-auto-cleanup 60
                recentf-exclude (append recentf-exclude
                                        '("/\\.git/.*\\'"
                                          "/elpa/.*\\'"
                                          "/tmp/.*\\'"
                                          "/private/.*\\'"
                                          "/var/tmp/.*\\'"))))

;; Remember last visited place in a file
(use-package saveplace
  :ensure nil
  :hook (elpaca-after-init . save-place-mode)
  :config (setq save-place-forget-unreadable-files t))

;; Save minibuffer history
(use-package savehist
  :ensure nil
  :hook (elpaca-after-init . savehist-mode)
  :config (setq history-delete-duplicates t
                savehist-autosave-interval 60
                savehist-additional-variables '(compile-command
                                                kill-ring
                                                search-ring
                                                regexp-search-ring)))

;; Switch windows with windmove
(use-package windmove
  :ensure nil
  :config (windmove-default-keybindings 'shift))

;; Comint buffers
(use-package comint
  :ensure nil
  :defer t
  :config (setq comint-process-echoes t
                comint-prompt-read-only t
                comint-input-ignoredups t
                comint-scroll-to-bottom-on-input 'this
                comint-scroll-to-bottom-on-output 'others
                comint-scroll-show-maximum-output t
                comint-move-point-for-output 'others))

;; Man pages
(use-package woman
  :ensure nil
  :bind ("C-c C-x m" . woman)
  :commands woman
  :config (setq woman-fill-frame t
                woman-use-topic-at-point t
                woman-imenu t
                woman-imenu-generic-expression '((nil "\n\\([A-Z].*\\)" 1))

                ;; Handle lowercase headings / "see also"
                Man-heading-regexp "^\\([A-Z][a-zA-Z0-9 /-:]+\\)$"
                Man-see-also-regexp "\\(SEE ALSO\\|See also\\)"))

;; Diffs
(use-package ediff
  :ensure nil
  :defer
  :config (setq ediff-ignore-similar-regions t))

(use-package ediff-wind
  :ensure nil
  :after ediff
  :config (setq ediff-window-setup-function #'ediff-setup-windows-plain-compare))

;; Display rules for specific buffers & modes
(add-to-list 'display-buffer-alist
             '((lambda (buffer _)
                 (with-current-buffer buffer
                   (or (string-equal (buffer-name) "*eldoc*")
                       (seq-some (lambda (mode) (derived-mode-p mode))
                                 '(helpful-mode
                                   help-mode
                                   read-only-mode
                                   compilation-mode
                                   Man-mode
                                   flymake-diagnostics-buffer-mode
                                   flymake-project-diagnostics-mode)))))
               (display-buffer-reuse-window display-buffer-below-selected)
               (reusable-frames . visible)
               (window-height . 0.33)
               (body-function . select-window)))


;; External packages

;; Prettier modeline
(use-package moody
  :if (display-graphic-p)
  :hook (;; FIXME not compatible with variable-width fonts in modeline
         ;; (elpaca-after-init . moody-replace-vc-mode)
         (elpaca-after-init . moody-replace-mode-line-buffer-identification)
         (elpaca-after-init . moody-replace-mode-line-front-space)
         (elpaca-after-init . moody-replace-eldoc-minibuffer-message-function))
  :config (setq moody-mode-line-height 16))

;; Hide minor-modes in a tidy menu
(use-package minions
  :hook (elpaca-after-init . minions-mode)
  :config (with-eval-after-load 'flymake
            (setq minions-prominent-modes '(flymake-mode))))

;; Highlight/dim all parens, brackets, etc.
(use-package paren-face
  :hook (prog-mode . paren-face-mode)
  :config (setq paren-face-regexp "[][(){}||]"
                paren-face-modes '(prog-mode)))

;; Highlight TODOs, FIXMEs, etc.
(use-package hl-todo
  :hook (prog-mode . global-hl-todo-mode))

;; Better line highlighting for special modes
(use-package lin
  :hook ((elpaca-after-init . lin-global-mode))
  :config (setq lin-face 'lin-cyan-override-fg))

;; Highlight surrounding parentheses
(use-package highlight-parentheses
  :disabled t
  :hook (((emacs-lisp-mode lisp-mode clojure-mode) . highlight-parentheses-mode)
         (highlight-parentheses-mode . ngq/set-highlight-parentheses-colors))
  :preface
  (defun ngq/--fade-out-color (highlight-color steps)
    (require 'color)
    (mapcar (lambda (rgb) (apply #'color-rgb-to-hex rgb))
            (color-gradient
             (color-name-to-rgb highlight-color)
             (color-name-to-rgb (face-foreground 'parenthesis)) steps)))

  (defun ngq/set-highlight-parentheses-colors ()
    (setq highlight-parentheses-colors (ngq/--fade-out-color "OrangeRed1" 5)))
  :custom-face (highlight-parentheses-highlight ((t (:weight semibold)))))

;; Pulse modified region
(use-package goggles
  :if (display-graphic-p)
  :hook ((prog-mode text-mode) . goggles-mode))

;; Persist *scratch* next-buffer
(use-package persistent-scratch
  :hook (elpaca-after-init . persistent-scratch-setup-default)
  :bind (:map lisp-interaction-mode-map
              ([remap save-buffer] . persistent-scratch-save))
  :config (setq persistent-scratch-backup-directory
                (no-littering-expand-var-file-name "persistent-scratch/")))

;; Persistent & consistent undo
(use-package undo-fu
  :hook (elpaca-after-init . global-undo-fu-session-mode)
  :bind (([remap undo] . undo-fu-only-undo)
         ("C-x U" . undo-fu-only-redo)))

(use-package undo-fu-session
  :config (setq undo-fu-session-file-limit 1000
                undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'"
                                                     "/git-rebase-todo\\'")))

;; Display available keybindings when entering a command
(use-package which-key
  :ensure nil
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-idle-secondary-delay 0.05
        which-key-add-column-padding 2
        which-key-sort-order 'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil)
  (which-key-setup-side-window-bottom))

;; Better help buffers
(use-package helpful
  :defer t
  :bind (("C-h f" . #'helpful-callable)
         ("C-h v" . #'helpful-variable)
         ("C-h k" . #'helpful-key)
         ("C-h c" . #'helpful-command)
         ("C-c C-d" . #'helpful-at-point)
         :map helpful-mode-map
         ("q" . (lambda () (interactive) (quit-window t))))
  :config (setq read-symbol-positions-list nil) ;; FIXME
  :init
  (with-eval-after-load 'tab-line
    (add-to-list 'tab-line-exclude-modes 'helpful-mode)))

(provide 'init-ui)

;;; init-ui.el ends here
