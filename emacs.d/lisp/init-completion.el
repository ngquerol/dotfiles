;;; init-completion.el --- Emacs completion UI -*- lexical-binding: t -*-

;;; Commentary:

;; Configuration pertaining to the completion / selection UI within Emacs.

;;; Code:

;; External packages

(use-package vertico
  :hook (after-init . vertico-mode)
  :config (setq vertico-cycle t))

(use-package marginalia
  :hook (vertico-mode . marginalia-mode)
  :bind (:map minibuffer-local-map ("C-o" . marginalia-cycle)))

(use-package orderless
  :config
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (initials)))
                                        (unicode-name (styles . (substring)))))

  (setq orderless-matching-styles
        '(orderless-literal orderless-regexp orderless-initialism)
        orderless-style-dispatchers
        '((lambda (pattern &rest _)
            (when (string-prefix-p "!" pattern)
              `(orderless-without-literal . ,(substring pattern 1))))
          (lambda (pattern &rest _)
            (when (string-suffix-p "~" pattern)
              `(orderless-flex . ,(substring pattern 0 -1))))
          (lambda (pattern &rest _)
            (when (string-suffix-p "=" pattern)
              `(orderless-literal . ,(substring pattern 0 -1)))))))

(use-package affe
  :after orderless
  :bind (("C-c S" . ngq/affe-grep-at-point)
         ("C-c F" . affe-find))
  :preface
  (defun ngq/affe-grep-at-point (&optional dir initial)
    (interactive (list prefix-arg (when-let ((s (symbol-at-point)))
                                    (symbol-name s))))
    (affe-grep dir initial))

  (defun ngq/affe-orderless-regexp-compiler (input _type)
    (setq input (orderless-pattern-compiler input))
    (cons input (lambda (str) (orderless--highlight input str))))
  :config (setq affe-count 20
                affe-regexp-function #'orderless-pattern-compiler
                affe-highlight-function #'orderless-highlight-matches
                affe-regexp-compiler #'ngq/affe-orderless-regexp-compiler))

(use-package consult
  :bind (("M-X" . consult-mode-command)
         ("C-x C-r" . consult-recent-file)
         ("C-c i" . consult-imenu)
         ("C-c h" . consult-man)
         ("C-c b" . consult-compile-error)
         ("C-c s" . ngq/consult-grep-at-point)
         ([remap isearch-forward] . consult-line)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ([remap yank-pop] . consult-yank-pop)
         ([remap goto-line] . consult-goto-line))
  :preface
  (defun ngq/consult-grep-at-point (&optional dir initial)
    (interactive (list prefix-arg (when-let ((s (symbol-at-point)))
                                    (symbol-name s))))
    (let ((command (cond ((executable-find "rg") #'consult-ripgrep)
                         ((executable-find "git" #'consult-git-grep))
                         (t #'consult-grep))))
      (funcall command dir initial)))
  :config
  (setq consult-preview-key nil
        consult-project-root-function #'(lambda () (cdr (project-current))))

  (with-eval-after-load 'xref
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref))

  (with-eval-after-load 'projectile
    (global-set-key (kbd "C-c I") #'consult-project-imenu)
    (setq consult-project-root-function 'projectile-project-root)))

(use-package consult-flycheck
  :after flycheck
  :bind (:map flycheck-mode-map
              ("C-c f" . #'consult-flycheck)))

(use-package consult-lsp
  :after lsp-mode
  :config
  (define-key lsp-command-map (kbd "s") #'consult-lsp-symbols)
  (define-key lsp-command-map (kbd "d") #'consult-lsp-diagnostics))

(use-package embark
  :bind (("C-c C-o" . embark-export)
         ("C-c C-c" . embark-act))
  :config (setq embark-quit-after-action nil
                embark-prompter #'embark-keymap-prompter
                embark-action-indicator
                (lambda (map _target)
                  (which-key--show-keymap "Embark" map nil nil 'no-paging)
                  #'which-key--hide-popup-ignore-command)
                embark-become-indicator embark-action-indicator))

(use-package embark-consult
  :after (embark consult))

;; Completions at point
(use-package company
  :hook ((prog-mode eshell-mode cider-repl-mode comint-mode) . company-mode)
  :preface
  (defun ngq/company-activate-local-backend (backend)
    (make-local-variable 'company-backends)
    (setq company-backends (copy-tree company-backends))
    (push backend (car company-backends)))

  (defun ngq/just-one-face (fn &rest args)
    (let ((orderless-match-faces [completions-common-part]))
      (apply fn args)))
  :bind ((:map company-mode-map
               ("TAB" . #'company-indent-or-complete-common))
         (:map company-active-map
               ("C-l" . #'company-show-location)
               ("C-s" . #'company-filter-candidates)
               ("C-d" . #'company-show-doc-buffer)
               ("C-a" . #'company-select-first)
               ("C-e" . #'company-select-last)
               ("C-n" . #'company-select-next)
               ("C-p" . #'company-select-previous)))
  :config
  (setq company-require-match 'never
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-tooltip-width-grow-only t
        company-format-margin-function #'company-text-icons-margin
        company-idle-delay 0.1
        company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend)
        company-backends '((company-capf :with company-yasnippet)
                           company-files
                           (company-keywords company-dabbrev-code)
                           company-dabbrev))
  (advice-add 'company-capf--candidates :around #'ngq/just-one-face))

(use-package company-dabbrev
  :straight nil
  :after company
  :config (setq company-dabbrev-downcase nil
                company-dabbrev-ignore-case t))

;; Snippets
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
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
  :config (yasnippet-snippets-initialize))

(provide 'init-completion)

;;; init-completion.el ends here
