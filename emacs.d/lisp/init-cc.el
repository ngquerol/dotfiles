;;; init-cc.el --- C modes common configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; Configuration for C family languages (C, C++, Objective-C).
;;
;; TODO: investigate rtags/ctags for large projects

;;; Code:

(use-package cc-mode
  :ensure nil
  :bind (:map c-mode-base-map
              ("C-c C-o" . ff-find-other-file)
              ("C-c C-c" . compile)
              ("C-c C-k" . kill-compilation))
  :preface (defun llvm-lineup-statement (langelem)
             (let ((in-assign (c-lineup-assignments langelem)))
               (if (not in-assign)
                   '++
                 (aset in-assign 0
                       (+ (aref in-assign 0)
                          (* 2 c-basic-offset)))
                 in-assign)))
  :config
  (setq c-basic-offset 2)
  (c-set-offset 'case-label '+)
  (c-set-offset 'substatement-open 0)

  ;; Use LLVM-style indentation
  (c-add-style "llvm"
               '("gnu"
                 (fill-column . 80)
                 (c++-indent-level . 2)
                 (c-basic-offset . 2)
                 (indent-tabs-mode . nil)
                 (c-offsets-alist . ((arglist-intro . ++)
                                     (innamespace . 0)
                                     (member-init-intro . ++)
                                     (statement-cont . llvm-lineup-statement)))))
  (setcdr (assoc 'other c-default-style) "llvm")

  ;; LSP integration via clangd
  (when (and (package-installed-p 'lsp-mode)
             (executable-find "clangd"))
    (setq-default lsp-clients-clangd-args '("--all-scopes-completion"
                                            "--background-index"
                                            "--completion-style=detailed"
                                            "--cross-file-rename"
                                            "--header-insertion=never"
                                            ;; "--header-insertion-decorators"
                                            "--suggest-missing-includes"
                                            "--clang-tidy"))
    (mapc (lambda (hook) (add-hook hook #'lsp-deferred))
          '(c-mode-hook c++-mode-hook objc-mode-hook))))

;; Source formatting via clang-format
(use-package clang-format+
  :if (executable-find "clang-format")
  :hook ((c-mode c++-mode objc-mode) . clang-format+-mode)
  :bind (:map c-mode-base-map ("C-c C-f" . clang-format-region))
  :config (setq clang-format+-context 'definition)
  :diminish)

(provide 'init-cc)

;;; init-cc.el ends here
