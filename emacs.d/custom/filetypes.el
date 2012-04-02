(require 'yaml-mode)
(setq auto-mode-alist
	     (nconc
	      '(("COMMIT_EDITMSG$"  . diff-mode))
	      '(("\.fsh$"          . c++-mode))
	      '(("\.vsh$"          . c++-mode))
	      '(("\.lua$"          . lua-mode))
	      '(("\.md$"           . markdown-mode))
	      '(("\.markdown$"     . markdown-mode))
	      '(("\.xml$"          . nxml-mode))
	      '(("\.html$"         . nxml-mode))
	      '(("\.m$"            . objc-mode))
	      '(("\.haml$"         . haml-mode))
	      '(("\.scss$"         . css-mode))
	      '(("\.yml$"          . yaml-mode))
	      '(("\.yaml$"         . yaml-mode))
	      '(("\.json$"         . yaml-mode))
	      '(("\.rb$"           . ruby-mode))
	      '(("\.gemspec$"      . ruby-mode))
	      '(("\.md$"           . markdown-mode))
	      '(("\.zsh$"          . sh-mode))
	      '(("\.rake$"         . ruby-mode))
	      '(("emfile$"         . ruby-mode))
	      '(("akefile$"        . ruby-mode))
	      '(("/PKGBUILD$" . pkgbuild-mode))
	      '(("\xresources*"    . conf-xdefaults-mode))
	      auto-mode-alist))

(setq magic-mode-alist ())

;; Lua
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;; PKGBUILD
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)

;; Markdown
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)

;; Mutt
(autoload 'post-mode "post" "mode for e-mail" t) (add-to-list 'auto-mode-alist '(".*mutt.*" . post-mode))
(add-hook 'post-mode-hook (lambda ()  (auto-fill-mode t) (setq fill-column 72)))

;; Perl
(add-hook 'perl-mode-hook '(lambda ()
			     (setq perl-indent-level 2)))

;; Ruby
(add-hook 'ruby-mode-hook '(lambda ()
			     (setq ruby-indent-level 2)))

(fset 'ruby-insert-end
      [?e ?n ?d ?  tab backspace return])
(fset 'ruby-method-definition
      [tab ?d ?e ?f ?  ?a return ?e ?n ?d ?  tab backspace ?\C-p ?\C-e backspace])
