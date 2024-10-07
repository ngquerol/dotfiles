;;; init-theme.el --- UI Colors & Fonts -*- lexical-binding: t -*-

;; Author: Nicolas G. Querol <nicolas.gquerol@gmail.com>

;;; Commentary:
;;
;; (Trying to) make Emacs pretty.

;;; Code:

(defun ngq/find-font (fonts)
  (seq-find
   (lambda (name) (find-font (font-spec :family name)))
   fonts))

(defun ngq/set-fonts ()
  (let ((monospace-font (ngq/find-font '("Berkeley Mono" "Iosevka" "Menlo" "Consolas" "Dejavu Mono")))
        (variable-width-font (ngq/find-font '("SF Compact Display" "Segoe UI" "Dejavu Sans")))
        (height 120))
    (set-face-attribute 'default nil :family monospace-font :height height)
    (set-face-attribute 'bold nil :weight 'semibold)
    (set-face-attribute 'variable-pitch nil :family variable-width-font :height height :weight 'normal)
    (set-face-attribute 'fixed-pitch nil :family monospace-font)
    (set-face-attribute 'fixed-pitch-serif nil :family monospace-font)))

(defun ngq/set-theme (appearance)
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (enable-theme 'modus-operandi))
    ('dark (enable-theme 'modus-vivendi)))
  (ngq/set-fonts))

;; External packages

(use-package ligature
  :if (display-graphic-p)
  :hook (elpaca-after-init . global-ligature-mode)
  :config (ligature-set-ligatures 'prog-mode
                                  ;; These are setup for Iosevka at the moment.
                                  '("<---" "<--" "<<-" "<-" "->" "-->" "--->"
                                    "<->" "<-->" "<--->" "<---->" "<!--" "<=="
                                    "<===" "<=" "=>" "=>>" "==>" "===>" ">="
                                    "<=>" "<==>" "<===>" "<====>" "<!---" "<~~"
                                    "<~" "~>" "~~>" "::" ":::" "==" "!=" "==="
                                    "!==" ":=" ":-" ":+" "<*" "<*>" "*>" "<|"
                                    "<|>" "|>" "+:" "-:" "=:" "<******>" "++"
                                    "+++")))

(use-package modus-themes
  :custom-face
  (region ((t :extend nil)))
  (eglot-inlay-hint-face ((t :height 0.9 :foreground "#757575")))
  :init
  (require-theme 'modus-themes)

  (setq modus-themes-prompts '(medium)
        modus-themes-bold-constructs t
        modus-themes-italic-constructs t
        modus-themes-variable-pitch-ui t
        modus-themes-mixed-fonts t
        modus-themes-common-palette-overrides `(;; region
                                                (bg-region bg-sage)
                                                (fg-region unspecified)

                                                ;; matching parentheses
                                                (bg-paren-match bg-magenta-intense)

                                                ;; (in)active mode-line
                                                (bg-mode-line-active bg-sage)
                                                (border-mode-line-active unspecified)
                                                (bg-mode-line-inactive bg-dim)
                                                (border-mode-line-inactive unspecified)

                                                ;; tabs
                                                (bg-tab-bar bg-dim)
                                                (bg-tab-current bg-lavender)
                                                (bg-tab-other bg-inactive)

                                                ;; org src blocks
                                                (bg-prose-block-contents unspecified)
                                                (bg-prose-block-delimiter bg-dim)
                                                (fg-prose-block-delimiter fg-dim)

                                                ;; use `faint' colors preset for the rest
                                                ,@modus-themes-preset-overrides-faint)
        modus-vivendi-palette-overrides `(;; background
                                          (bg-main ,(if (display-graphic-p) "#1c1c1c" nil))
                                          (bg-dim "#272727")))

  (mapc (lambda (theme) (load-theme theme t t))
        '(modus-operandi modus-vivendi))

  (if (and (display-graphic-p) (boundp 'ns-system-appearance-change-functions))
      (progn
        (when (fboundp #'kind-icon-reset-cache)
          (add-hook 'ns-system-appearance-change-functions #'(lambda (_) (kind-icon-reset-cache))))
        (add-hook 'ns-system-appearance-change-functions #'ngq/set-theme))
    (ngq/set-theme 'dark)))

(provide 'init-theme)

;;; init-theme.el ends here
