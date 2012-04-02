;; -------~-------~--~------------------~------
;;
;; EMACS configuration file
;;
;; Nicolas G. Querol <nicolas.gquerol@gmail.com>
;;
;; -------~-------~--~------------------~------

;; -------~-------~--~------------------~------
;; EL-GET
;; -------~-------~--~------------------~------
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require 'el-get)

;; local sources
(setq el-get-sources
      '(
	(:name "yasnippet"
	       :description "YASnippet is a template system for Emacs."
	       :type git
	       :url "https://github.com/capitaomorte/yasnippet")
	(:name popup
	       :website "https://github.com/m2ym/popup-el"
	       :description "Visual Popup Interface Library for Emacs"
	       :type git
	       :url "https://github.com/m2ym/popup-el.git"
	       :features popup)
	)
      )

(setq my-packages
      (append
       '(
     yasnippet
     popup
	 autopair
	 auto-complete
	 lua-mode
	 yaml-mode
	 linum-off
	 )
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-packages)

;; -------~-------~--~------------------~------
;; YASNIPPET
;; -------~-------~--~------------------~------
(setq yas/snippet-dirs '("~/.emacs.d/snippets" "~/.emacs.d/el-get/yasnippet/snippets"))
(yas/global-mode 1)

;; -------~-------~--~------------------~------
;; AUTO-COMPLETE
;; -------~-------~--~------------------~------
(add-to-list 'ac-dictionary-directories "~/.emacs.d/el-get/auto-complete/dict")
(require 'auto-complete-config)
(ac-config-default)

;; -------~-------~--~------------------~------
;; AUTOPAIR
;; -------~-------~--~------------------~------
(autopair-global-mode 1)
(setq autopair-autowrap t)

;; -------~-------~--~------------------~------
;; SEPARATE CONFIGS
;; -------~-------~--~------------------~------
(setq custom-file "~/.emacs-custom.el")
(load custom-file 'noerror)

(add-to-list 'load-path "~/.emacs.d/custom")
(load "mode-line")
(load "filetypes")
(load "line-numbers")

;; -------~-------~--~------------------~------
;; APPEARANCE
;; -------~-------~--~------------------~------
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'dotshare t)

;; -------~-------~--~------------------~------
;; SYNTAX
;; -------~-------~--~------------------~------
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; -------~-------~--~------------------~------
;; MISC
;; -------~-------~--~------------------~------
;; no...
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(blink-cursor-mode 0)
(setq make-backup-files nil
      auto-save-default nil)

;; show matching parens
(show-paren-mode t)

;; no splash
(setq inhibit-splash-screen t)

;; hilight current line
(global-hl-line-mode 1)

;; indent
(setq standard-indent 2
      indent-tabs-mode nil
      tab-width)

;; always end a file with a newline
(setq require-final-newline t)

;; line numbers
(defvar linum-is-relative 1)
(toggle-linum)

;; ask for "y or n" instead of "yes or no"
(defalias 'yes-or-no-p 'y-or-n-p)

;; delete selected text
(delete-selection-mode t)

;; "smooth" scrolling
(setq scroll-preserve-screen-position 1
      scroll-margin                   10
      scroll-conservatively           10000)

;; clean up redundant whitespace when saving
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Environment
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
