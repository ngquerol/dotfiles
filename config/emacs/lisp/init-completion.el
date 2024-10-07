;;; init-completion.el --- Emacs completion UI -*- lexical-binding: t -*-

;;; Commentary:

;; Configuration pertaining to the completion / selection UI within Emacs.
;; Almost all packages mentioned here are by Daniel Mendler
;; (https://github.com/minad) !

;; TODO: investigate
;; - https://github.com/minad/tempel
;; - https://github.com/Crandel/tempel-collection

;;; Code:

(use-package emacs
  :ensure nil
  :init (setq completion-cycle-threshold 3
              read-extended-command-predicate #'command-completion-default-include-p
              tab-always-indent 'complete
              tab-first-completion 'word-or-paren-or-punct))

(use-package completion-preview
  :ensure nil
  :config (global-completion-preview-mode))

;; External packages

;; Orderless completion style
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion))
                                        (unicode-name (styles . (substring))))

        orderless-matching-styles '(orderless-literal
                                    orderless-regexp
                                    orderless-initialism)))

;; Minibuffer completion UI
(use-package vertico
  :hook ((elpaca-after-init . vertico-mode))
  :config (setq vertico-cycle t))

;; Annotations for the minibuffer completion UI
(use-package marginalia
  :hook (vertico-mode . marginalia-mode)
  :bind (:map minibuffer-local-map ("C-o" . marginalia-cycle))
  :config (setq marginalia-align-offset 0
                marginalia-align 'left
                marginalia--ellipsis "…"))

;; Consulting `completing-read'
(use-package consult
  :functions (consult-fd
              consult-find
              consult-ripgrep
              consult-git-grep
              consult-grep
              consult-xref)
  :defines (flymake-mode-map
            eshell-mode-map
            xref-show-definitions-function
            xref-show-xrefs-function)
  :preface
  (defun ngq/in-git-repo-p ()
    (string= (vc-responsible-backend default-directory 'no-error) "Git"))

  (defun ngq/consult-find (&optional dir initial)
    (interactive)
    (let ((command (if (executable-find "fd")
                       #'consult-fd
                     #'consult-find)))
      (funcall command dir initial)))

  (defun ngq/consult-grep-at-point (&optional dir initial)
    (interactive (list prefix-arg (when-let ((s (symbol-at-point)))
                                    (symbol-name s))))
    (let ((command (cond ((executable-find "rg") #'consult-ripgrep)
                         ((and (executable-find "git") (ngq/in-git-repo-p)) #'consult-git-grep)
                         (t #'consult-grep))))
      (funcall command dir initial)))
  :bind (("M-X" . consult-mode-command)
         ("C-x C-r" . consult-recent-file)
         ("C-c i" . consult-imenu)
         ("C-c h" . consult-man)
         ("C-c b" . consult-compile-error)
         ("C-c s" . ngq/consult-grep-at-point)
         ("C-c f" . ngq/consult-find)
         ([remap isearch-forward] . consult-line)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ([remap yank-pop] . consult-yank-pop)
         ([remap goto-line] . consult-goto-line)
         :map minibuffer-local-map ("C-r" . consult-history)
         :map flymake-mode-map ("C-c e e" . consult-flymake))
  :config
  (setq consult-preview-key nil)

  (with-eval-after-load 'xref
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref))

  (with-eval-after-load 'projectile
    (setq consult-project-function 'projectile-project-root))

  (add-hook 'eshell-mode-hook
            (lambda () (define-key eshell-mode-map (kbd "C-r") #'consult-history))))

;; Consult commands for `eglot'
(use-package consult-eglot
  :disabled t ;; FIXME
  :after (consult eglot)
  :bind (:map eglot-mode-map ("C-c C-s" . consult-eglot-symbols)))

;; Execute context-sensitive actions on different targets
(use-package embark
  :bind (:map minibuffer-mode-map
              ("C-c C-e" . embark-export)
              ("C-c C-c" . embark-act)
              ("C-c C-a" . embark-act-all)
              ("C-SPC" . embark-select))
  :config
  (setf (alist-get 'kill-buffer embark-pre-action-hooks) nil)
  (setq embark-quit-after-action '((kill-buffer . nil) (t . t)))
  (add-to-list 'display-buffer-alist
               '("\\*Embark \\(Collect\\|Export\\|Completions\\)"
                 nil
                 (display-buffer-reuse-window display-buffer-below-selected)
                 (reusable-frames . visible)
                 (window-height . 0.33)
                 (body-function . select-window))))

;; Integrate Embark with Consult
(use-package embark-consult
  :after (embark consult)
  :functions (embark-consult-export-location-grep)
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  :config (setf (alist-get 'consult-location embark-exporters-alist)
                #'embark-consult-export-location-grep))

;; Asynchronous fuzzy finder similar to fzf
(use-package affe
  :if (executable-find "rg")
  :after orderless
  :functions (affe-grep
              orderless-pattern-compiler
              orderless--highlight)
  :bind (("C-c S" . ngq/affe-grep-at-point)
         ("C-c F" . affe-find))
  :preface
  (defun ngq/affe-grep-at-point (&optional dir initial)
    (interactive (list prefix-arg (when-let ((s (symbol-at-point)))
                                    (symbol-name s))))
    (affe-grep dir initial))

  (defun ngq/affe-orderless-regexp-compiler (input _type ignorecase)
    (setq input (orderless-pattern-compiler input))
    (cons input (lambda (str) (orderless--highlight input ignorecase str))))
  :config (setq affe-count 20
                affe-regexp-compiler #'ngq/affe-orderless-regexp-compiler))

;; In-region completion UI
(use-package corfu
  :ensure (:files (:defaults "extensions/corfu-popupinfo.el"
                             "extensions/corfu-info.el"
                             "extensions/corfu-echo.el"))
  :bind ((:map corfu-map
               ([tab] . corfu-next)
               ("TAB" . corfu-next)
               ([backtab] . corfu-previous)
               ("S-TAB" . corfu-previous)))
  :hook ((elpaca-after-init . global-corfu-mode)
         (corfu-mode . corfu-popupinfo-mode))
  :preface
  (defun corfu-send-shell (&rest _)
    "Send completion candidate when inside comint/eshell."
    (cond
     ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
      (eshell-send-input))
     ((and (derived-mode-p 'comint-mode)  (fboundp 'comint-send-input))
      (comint-send-input))))
  :config
  (setq-default corfu-auto t
                corfu-auto-prefix 2
                corfu-cycle t
                corfu-max-width 80
                corfu-min-width 40
                corfu-preselect 'valid
                corfu-preview-current 'insert
                corfu-quit-at-boundary t
                corfu-count 10
                corfu-scroll-margin 5)
  (advice-add #'corfu-insert :after #'corfu-send-shell)
  :custom-face (corfu-popupinfo ((t (:height 1.0)))))

(use-package corfu-terminal
  :unless (display-graphic-p)
  :hook (corfu-mode . corfu-terminal-mode)
  :ensure (:repo "https://codeberg.org/akib/emacs-corfu-terminal.git"))

(use-package kind-icon
  :after corfu
  :defines (corfu-margin-formatters)
  :functions (kind-icon-margin-formatter)
  :config
  (setq kind-icon-use-icons t
        kind-icon-blend-background t
        kind-icon-blend-frac 0.2
        kind-icon-extra-space t
        kind-icon-default-face 'corfu-default)

  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Extra `completion-at-point-functions'
(use-package cape
  :functions (cape-dabbrev cape-file cape-keyword)
  :init
  (dolist (backend (list #'cape-dabbrev #'cape-file #'cape-keyword))
    (add-to-list 'completion-at-point-functions backend)))

(use-package yasnippet-capf
  :after (cape yasnippet)
  :ensure (:host github :repo "elken/yasnippet-capf")
  :preface
  (defun ngq/cape-setup-yasnippet ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       (car completion-at-point-functions)
                       #'yasnippet-capf)
                      #'cape-dabbrev #'cape-file #'cape-keyword)))
  :hook (prog-mode . ngq/cape-setup-yasnippet))

;; Snippets
(use-package yasnippet
  :hook ((prog-mode . yas-minor-mode)
         (org-mode . yas-minor-mode))
  :bind (:map yas-minor-mode-map
              ("C-c & n" . yas-new-snippet)
              ("C-c & s" . yas-insert-snippet)
              ("C-c & v" . yas-visit-snippet-file)
              ("C-c & r" . yas-reload-all)
              ("C-c & &" . yas-describe-tables))
  :init (setq yas-wrap-around-region t
              yas-verbosity 1))

(use-package yasnippet-snippets
  :after yasnippet
  :functions (yasnippet-snippets-initialize)
  :config (yasnippet-snippets-initialize))

(provide 'init-completion)

;;; init-completion.el ends here
