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
	      '(("/PKGBUILD$"      . pkgbuild-mode))
	      '(("\xresources*"    . conf-xdefaults-mode))
	      auto-mode-alist))

(setq magic-mode-alist ())

;; Lua
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;; Markdown
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)

;; Perl
(add-hook 'perl-mode-hook '(lambda ()
			     (setq perl-indent-level 4)))

;; Ruby
(add-hook 'ruby-mode-hook '(lambda ()
			     (setq ruby-indent-level 4)))
