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
  :straight (:type built-in)
  :init (setq completion-cycle-threshold 3
              read-extended-command-predicate #'command-completion-default-include-p
              tab-always-indent 'complete))

;; External packages

;; Orderless completion style
(use-package orderless
  :config
  (setq  completion-styles '(orderless basic)
         completion-category-defaults nil
         completion-category-overrides '((file (styles basic partial-completion))
                                         (unicode-name (styles . (substring))))

         orderless-matching-styles '(orderless-literal
                                     orderless-regexp
                                     orderless-initialism)))

;; Minibuffer completion UI
(use-package vertico
  ;; :straight (:files (:defaults "extensions/*"))
  :hook ((after-init . vertico-mode))
  :config (setq vertico-cycle t))

(use-package vertico-multiform
  :disabled t
  :after vertico
  :straight nil
  :hook ((vertico-mode . vertico-multiform-mode))
  :config
  ;; Choices: vertical grid flat reverse unobtrusive
  (setq vertico-multiform-categories '((command flat))))

(use-package vertico-posframe
  :if (display-graphic-p)
  :after vertico
  :hook (after-init . vertico-posframe-mode)
  :config (setq vertico-posframe-parameters '((left-fringe . 8)
                                              (right-fringe . 8))
                vertico-posframe-border-width 2))

;; Annotations for the minibuffer completion UI
(use-package marginalia
  :hook ((vertico-mode . marginalia-mode))
  :bind ((:map minibuffer-local-map ("C-o" . marginalia-cycle)))
  :config (setq marginalia-align-offset 0
                marginalia-align 'left
                marginalia--ellipsis "…"))

;; Consulting `completing-read'
(use-package consult
  :bind (("M-X" . consult-mode-command)
         ("C-x C-r" . consult-recent-file)
         ("C-c i" . consult-imenu)
         ("C-c h" . consult-man)
         ("C-c b" . consult-compile-error)
         ("C-c s" . ngq/consult-grep-at-point)
         ("C-c f" . consult-flymake)
         ([remap isearch-forward] . consult-line)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ([remap yank-pop] . consult-yank-pop)
         ([remap goto-line] . consult-goto-line)
         :map minibuffer-local-map ("C-r" . consult-history))
  :preface
  (defun ngq/in-git-repo-p ()
    (string= (vc-responsible-backend default-directory 'no-error) "Git"))

  (defun ngq/consult-grep-at-point (&optional dir initial)
    (interactive (list prefix-arg (when-let ((s (symbol-at-point)))
                                    (symbol-name s))))
    (let ((command (cond ((executable-find "rg") #'consult-ripgrep)
                         ((and (executable-find "git") (ngq/in-git-repo-p)) #'consult-git-grep)
                         (t #'consult-grep))))
      (funcall command dir initial)))
  :config
  (setq consult-preview-key nil)

  (with-eval-after-load 'xref
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref))

  (with-eval-after-load 'projectile
    (setq consult-project-function 'projectile-project-root))

  (add-hook 'eshell-mode-hook
            (lambda () (define-key eshell-mode-map (kbd "C-r") #'consult-history))))

;; Consult commands for `lsp-mode'
(use-package consult-lsp
  :after (consult lsp-mode)
  :config
  (define-key lsp-command-map (kbd "s") #'consult-lsp-symbols)
  (define-key lsp-command-map (kbd "i") #'consult-lsp-file-symbols)
  (define-key lsp-command-map (kbd "d") #'consult-lsp-diagnostics))

;; Execute context-sensitive actions on different targets
(use-package embark
  :disabled t
  :bind (("C-c C-o" . embark-export)
         ("C-c C-c" . embark-act))
  :config
  (setq embark-quit-after-action nil
        embark-indicators '(embark-minimal-indicator
                            embark-highlight-indicator
                            embark-isearch-highlight-indicator))

  (add-to-list 'display-buffer-alist
               '("\\*Embark Actions\\*"
                 nil
                 (window-height . 0.33)
                 (window-parameters (mode-line-format . none)))))

;; Integrate Embark with Consult
(use-package embark-consult
  :after (embark consult))

;; Asynchronous fuzzy finder similar to fzf
(use-package affe
  :if (executable-find "rg")
  :after orderless
  :bind (("C-c S" . ngq/affe-grep-at-point)
         ("C-c F" . affe-find))
  :preface
  (defun ngq/affe-grep-at-point (&optional dir initial)
    (interactive (list prefix-arg (when-let ((s (symbol-at-point)))
                                    (symbol-name s))))
    (affe-grep dir initial))

  (defun ngq/affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (orderless-pattern-compiler input))
    (cons input (lambda (str) (orderless--highlight input str))))
  :config (setq affe-count 20
                affe-regexp-compiler #'ngq/affe-orderless-regexp-compiler))

;; In-region completion UI
(use-package corfu
  :straight (:files (:defaults "extensions/corfu-popupinfo.el"
                               "extensions/corfu-info.el"))
  :bind ((:map corfu-map
               ("TAB" . corfu-next)
               ([tab] . corfu-next)
               ("S-TAB" . corfu-previous)
               ([backtab] . corfu-previous)))
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
                corfu-preview-current t
                corfu-quit-at-boundary t
                corfu-quit-no-match t
                corfu-scroll-margin 5)

  (advice-add #'corfu-insert :after #'corfu-send-shell)
  :custom-face (corfu-popupinfo ((t (:height 1.0))))
  :init (progn
          (global-corfu-mode)
          (unless (display-graphic-p)
            (corfu-popupinfo-mode))))

(use-package corfu-terminal
  :unless (display-graphic-p)
  :after corfu
  :hook (corfu-mode . corfu-terminal-mode)
  :straight (:type git :repo "https://codeberg.org/akib/emacs-corfu-terminal.git"))

(use-package corfu-doc-terminal
  :unless (display-graphic-p)
  :after corfu
  :hook (corfu-mode . corfu-doc-terminal-mode)
  :straight (:type git :repo "https://codeberg.org/akib/emacs-corfu-doc-terminal.git"))

(use-package kind-icon
  :after corfu
  :config
  (setq kind-icon-use-icons t
        kind-icon-blend-background t
        kind-icon-blend-frac 0.2
        kind-icon-extra-space t
        kind-icon-default-face 'corfu-default)

  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Extra `completion-at-point-functions'
(use-package cape
  :init
  (dolist (backend (list #'cape-dabbrev #'cape-file #'cape-keyword))
    (add-to-list 'completion-at-point-functions backend)))

(use-package cape-yasnippet
  :after (cape yasnippet)
  :straight (:host github :repo "elken/cape-yasnippet")
  :preface
  (defun ngq/cape-setup-yasnippet ()
    (setq-local completion-at-point-functions
                (append (list
                         (cape-super-capf #'cape-yasnippet
                                          (car completion-at-point-functions)))
                        (list #'cape-dabbrev #'cape-file #'cape-keyword))))
  :hook ((prog-mode . ngq/cape-setup-yasnippet)
         (lsp-completion-mode . ngq/cape-setup-yasnippet)))

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
