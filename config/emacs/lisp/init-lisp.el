;;; init-lisp.el --- Lisp editing configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol

;;; Commentary:

;; Lisp editing configuration and helpers.

;;; Code:

(require 'pp)

;; Make `pp-display-expression' output read-only, and switch to its buffer upon
;; invocation.
(advice-add
 #'pp-display-expression
 :after
 (lambda (_ out-buffer-name)
   (when (get-buffer out-buffer-name)
     (with-current-buffer out-buffer-name
       (view-mode t)
       (toggle-truncate-lines t))
     (switch-to-buffer-other-window out-buffer-name))))

(defun ngq/pp-macroexpand-last-sexp (arg)
  "Same as `pp-macroexpand-last-sexp', but optionally perform all macro expansions if prefix ARG is given."
  (interactive "P")
  (let ((expression (pp-last-sexp)))
    (pp-display-expression
     (if arg
         (macroexpand-all expression)
       (macroexpand-1 expression))
     "*Pp Macroexpand Output*")))

(dolist (keymap (list emacs-lisp-mode-map lisp-interaction-mode-map))
  (define-key keymap (kbd "C-x p") #'pp-eval-last-sexp)
  (define-key keymap (kbd "C-x m") #'ngq/pp-macroexpand-last-sexp))

;; External packages

;; Highlight known Emacs Lisp symbols
(use-package highlight-defined :hook (emacs-lisp-mode . highlight-defined-mode))

;; Expand/collapse Emacs Lisp macros interactively
(use-package
 macrostep
 :bind
 (:map
  lisp-interaction-mode-map ("C-c e" . macrostep-expand)
  :map emacs-lisp-mode-map ("C-c e" . macrostep-expand)))

;; Superior Lisp Interaction Mode for Emacs
(use-package
 slime
 :if (executable-find "sbcl")
 :commands slime
 :config
 (setq
  inferior-lisp-program "sbcl"
  slime-contribs '(slime-fancy slime-asdf slime-indentation slime-sbcl-exts))

 (with-eval-after-load 'smartparens
   (add-hook 'slime-repl-mode-hook #'smartparens-strict-mode)))

(provide 'init-lisp)

;;; init-lisp.el ends here
