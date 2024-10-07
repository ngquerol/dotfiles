;;; init-cc.el --- C modes common configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; Configuration for C family languages (C, C++, Objective-C).

;;; Code:

(use-package cc-mode
  :ensure nil
  :bind (:map c-mode-base-map
              ("TAB" . indent-for-tab-command)
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
  :init (when (and (fboundp #'eglot-ensure) (executable-find "clangd"))
          (dolist (hook '(c-mode-hook
                          c-ts-mode-hook
                          c++-mode-hook
                          objc-mode-hook))
            (add-hook hook #'eglot-ensure)))
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
  (setcdr (assoc 'other c-default-style) "llvm"))

(use-package disaster
  :after cc-mode
  :bind (:map c-mode-base-map ("C-c d" . disaster))
  :config (with-eval-after-load 'tab-line
            (add-to-list 'tab-line-exclude "^\*")))

(provide 'init-cc)

;;; init-cc.el ends here
