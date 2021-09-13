;;; init-theme.el --- UI Colors & Fonts -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; (Trying to) make Emacs pretty.

;;; Code:

;; External packages

(use-package ligature
  :straight (:type git :host github :repo "mickeynp/ligature.el")
  :hook (after-init . global-ligature-mode)
  :config (ligature-set-ligatures 'prog-mode
                                  ;; These are setup for Iosevka at the moment.
                                  '("-<<" "-<" "-<-" "<--" "<---" "<<-" "<-"
                                    "->" "->>" "-->" "--->" "->-" ">-" ">>-"
                                    "=<<" "=<" "=<=" "<==" "<===" "<<=" "<="
                                    "=>" "=>>" "==>" "===>" "=>=" ">=" ">>="
                                    "<->" "<-->" "<--->" "<---->" "<=>" "<==>"
                                    "<===>" "<====>" "-------->" "<~~" "<~" "~>"
                                    "~~>" "::" ":::" "==" "!=" "/=" "~=" "<>"
                                    "===" "!==" "=/=" "=!=" ":=" ":-" ":+" "<*"
                                    "<*>" "*>" "<|" "<|>" "|>" "<." "<.>" ".>"
                                    "+:" "-:" "=:" ":>" "(*" "*)" "[|" "|]" "{|"
                                    "|}" "++" "+++" "\\/" "/\\" "|-" "-|" "<!--"
                                    "<!---" "<***>")))

(use-package modus-themes
  :preface
  (defun ngq/find-font (fonts)
    (seq-find (lambda (name)
                (find-font (font-spec :family name)))
              fonts))

  (defun ngq/set-fonts ()
    (let ((monospace-font (ngq/find-font '("Iosevka SS15" "Menlo" "Consolas" "Dejavu Mono")))
          (variable-width-font (ngq/find-font '("SF Compact Text" "Segoe UI" "Dejavu Sans"))))
      (set-face-attribute 'default nil :family monospace-font :height 120)
      (set-face-attribute 'bold nil :weight 'semibold)
      (set-face-attribute 'variable-pitch nil :family variable-width-font :height 130)
      (set-face-attribute 'fixed-pitch nil :family monospace-font)
      (set-face-attribute 'fixed-pitch-serif nil :family monospace-font)))

  (defun ngq/set-theme (appearance)
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light
       (load-theme 'modus-operandi t)
       (set-face-attribute 'mode-line nil :box '(:color "#aaaaaa")))
      ('dark
       (load-theme 'modus-vivendi t)
       (set-face-attribute 'mode-line nil :box '(:color "#505050"))))
    (ngq/set-fonts))
  :config
  (setq modus-themes-completions 'opinionated
        modus-themes-fringes nil
        modus-themes-lang-checkers '(background)
        modus-themes-links '(faint neutral-underline)
        modus-themes-org-blocks '(gray-background)
        modus-themes-paren-match '(bold)
        modus-themes-prompts '(bold)
        modus-themes-region '(bg-only no-extend)
        modus-themes-bold-constructs t
        modus-themes-italic-constructs t
        modus-themes-subtle-line-numbers t
        modus-themes-syntax '(faint)
        modus-themes-diffs '(desaturated)
        modus-themes-mode-line nil
        modus-themes-variable-pitch-headings t
        modus-themes-variable-pitch-ui t)

  (if (boundp 'ns-system-appearance-change-functions)
      (add-hook 'ns-system-appearance-change-functions #'ngq/set-theme)
    (ngq/set-theme 'light)))

(provide 'init-theme)

;;; init-theme.el ends here
