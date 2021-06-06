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

  (defun ngq/set-fonts (appearance)
    (let ((monospace-font (ngq/find-font '("Iosevka SS15" "Menlo" "Consolas" "Dejavu Mono")))
          (variable-width-font (ngq/find-font '("SF Compact Text" "Segoe UI" "Dejavu Sans"))))
      (set-face-attribute 'default nil :family monospace-font :height 120)
      (set-face-attribute 'bold nil :weight 'semibold)
      (set-face-attribute 'variable-pitch nil :family variable-width-font :height 130)
      (set-face-attribute 'fixed-pitch nil :family monospace-font)
      (set-face-attribute 'fixed-pitch-serif nil :family monospace-font)
      (set-face-attribute 'mode-line nil :box (pcase 'appearance
                                                ('light "#aaa")
                                                ('dark "#505050")))))

  (defun ngq/set-theme (appearance)
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (load-theme 'modus-operandi t))
      ('dark (load-theme 'modus-vivendi t)))
    (ngq/set-fonts appearance))
  :config
  (setq modus-themes-completions 'opinionated
        modus-themes-fringes nil
        modus-themes-lang-checkers 'subtle-foreground
        modus-themes-links 'faint-neutral-underline
        modus-themes-mode-line nil
        modus-themes-org-blocks 'greyscale
        modus-themes-paren-match 'subtle-bold
        modus-themes-prompts 'subtle-accented
        modus-themes-region 'bg-only-no-extend
        modus-themes-bold-constructs t
        modus-themes-slanted-constructs t
        modus-themes-subtle-line-numbers t
        modus-themes-syntax 'faint
        modus-themes-diffs 'desaturated
        modus-themes-vivendi-color-overrides '((bg-main . "#1a1f26")
                                               (bg-dim . "#161129")
                                               (bg-alt . "#181732")
                                               (bg-hl-line . "#282a36")
                                               (bg-active . "#282e46")
                                               (bg-inactive . "#1a1e39")
                                               (bg-region . "#393a53")
                                               (bg-header . "#202037")
                                               (bg-tab-bar . "#262b41")
                                               (bg-tab-active . "#120f18")
                                               (bg-tab-inactive . "#3a3a5a")
                                               (fg-unfocused . "#9a9aab"))
        modus-themes-variable-pitch-headings t
        modus-themes-variable-pitch-ui t)

  (if (boundp 'ns-system-appearance-change-functions)
      (add-hook 'ns-system-appearance-change-functions #'ngq/set-theme)
    (ngq/set-theme 'light)))

(provide 'init-theme)

;;; init-theme.el ends here
