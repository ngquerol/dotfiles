;;; init-ivy.el --- Ivy & friends configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol

;;; Commentary:

;; Use ivy & friends to handle completions throughout Emacs.

;;; Code:

(use-package ivy
  :hook (after-init . ivy-mode)
  :bind (("C-c C-r" . ivy-resume)
         (:map ivy-minibuffer-map
               ("<up>" . ivy-previous-line-or-history)
               ("<down>" . ivy-next-line-or-history)))
  :config
  (setq ivy-dynamic-exhibit-delay-ms 150
        ivy-extra-directories nil
        ivy-use-selectable-prompt t
        ivy-use-virtual-buffers t
        ivy-wrap t
        ivy-count-format "(%d/%d) ")

  (setf (alist-get t ivy-format-functions-alist)
        #'ivy-format-function-line)

  (setf (alist-get 'counsel-recentf ivy-sort-functions-alist)
        #'file-newer-than-file-p)

  (ivy-set-actions #'counsel-find-file
                   '(("c" counsel-find-file-copy "copy")
                     ("d" counsel-find-file-delete "delete")
                     ("r" counsel-find-file-move "move or rename")
                     ("m" counsel-find-file-mkdir-action "mkdir")
                     ("s" counsel-find-file-as-root "open as root")
                     ("x" counsel-find-file-extern "open external")
                     ("l" find-file-literally  "open literally")
                     ("w" find-file-other-window "other window")
                     ("f" find-file-other-frame "other frame")))

  (ivy-set-actions 'ivy-switch-buffer
                   '(("k" ivy--kill-buffer-action "kill")
                     ("r" ivy--rename-buffer-action "rename")
                     ("w" ivy--switch-buffer-other-window-action "other window")
                     ("x" counsel-open-buffer-file-externally "open externally")))

  (with-eval-after-load 'projectile
    (setq-default projectile-completion-system 'ivy))

  :diminish)

(use-package ivy-prescient
  :after ivy
  :hook ((ivy-mode . ivy-prescient-mode)
         (ivy-prescient-mode . prescient-persist-mode))
  :config
  (setq prescient-filter-method '(literal regexp initialism)
        ivy-prescient-sort-commands '(:not swiper
                                           swiper-isearch
                                           ivy-switch-buffer
                                           counsel-find-file
                                           counsel-git-grep
                                           counsel-grep
                                           counsel-rg
                                           counsel-imenu
                                           counsel-recentf
                                           counsel-yank-pop)))

(use-package ivy-rich
  :after ivy
  :hook (ivy-mode . ivy-rich-mode)
  :config (setq ivy-rich-parse-remote-buffer nil
                ivy-rich-path-style 'abbrev
                ivy-virtual-abbreviate 'abbreviate))

(use-package ivy-xref
  :after ivy
  :config (setq xref-show-xrefs-function #'ivy-xref-show-xrefs
                xref-show-definitions-function #'ivy-xref-show-defs
                ivy-xref-use-file-path t))

(use-package ivy-yasnippet
  :after ivy yasnippet
  :bind ("C-c y" . ivy-yasnippet))

(use-package counsel
  :hook (after-init . counsel-mode)
  :bind (([remap execute-extended-command] . counsel-M-x)
         ([remap yank-pop] . counsel-yank-pop)
         ([remap find-file-read-only] . counsel-recentf)
         ([remap find-file] . counsel-find-file)
         ([remap insert-char] . counsel-unicode-char)
         ([remap isearch-forward] . counsel-grep-or-swiper)
         ([remap isearch-backward] . counsel-grep-or-swiper-backward)
         ("C-c i" . counsel-imenu))
  :config
  (setq counsel-find-file-at-point t
        counsel-yank-pop-separator "\n————————————————\n")

  (with-eval-after-load 'flycheck
    (global-set-key (kbd "C-c f") #'counsel-flycheck))

  (with-eval-after-load 'helpful
    (setq counsel-describe-function-function #'helpful-callable
          counsel-describe-variable-function #'helpful-variable))

  (global-set-key (kbd "C-c s") (cond
                                 ((executable-find "rg") #'counsel-rg)
                                 ((executable-find "git" #'counsel-git-grep))
                                 (t #'counsel-grep)))
  :diminish)

(use-package counsel-projectile
  :after counsel projectile
  :init (counsel-projectile-mode t)
  :config
  (setq counsel-projectile-rg-initial-input '(ivy-thing-at-point)
        counsel-projectile-grep-initial-input '(ivy-thing-at-point)
        counsel-projectile-sort-files t)

  (define-key projectile-command-map (kbd "s")
    (if (executable-find "rg")
        #'counsel-projectile-rg
      #'counsel-projectile-grep))) ; uses git grep if applicable

(provide 'init-ivy)

;;; init-ivy.el ends here
