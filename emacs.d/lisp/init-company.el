;;; init-company.el --- Company configuration -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; Configure `company-mode' to show completions at point.

;;; Code:
(use-package company
  :hook ((prog-mode eshell-mode cider-repl-mode comint-mode) . company-mode)
  :preface (defun ngq/company-activate-local-backend (backend)
             (make-local-variable 'company-backends)
             (setq company-backends (copy-tree company-backends))
             (push backend (car company-backends)))
  :bind ((:map company-mode-map
               ("TAB" . #'company-indent-or-complete-common))
         (:map company-active-map
               ("C-l" . #'company-show-location)
               ("C-s" . #'company-filter-candidates)
               ("C-d" . #'company-show-doc-buffer)
               ("C-n" . #'company-select-next)
               ("C-p" . #'company-select-previous)))
  :config (setq company-require-match 'never
                company-minimum-prefix-length 2
                company-selection-wrap-around t
                company-tooltip-align-annotations t
                company-idle-delay .2
                company-frontends '(company-pseudo-tooltip-frontend
                                    company-echo-metadata-frontend)
                company-backends '((company-capf company-keywords :with company-yasnippet)
                                   (company-files :with company-yasnippet)
                                   (company-dabbrev-code :with company-yasnippet)
                                   (company-dabbrev :with company-yasnippet)))
  :diminish company-mode)

(provide 'init-company)

;;; init-company.el ends here
