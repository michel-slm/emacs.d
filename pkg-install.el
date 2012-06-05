(when (version< emacs-version "24")
  (add-to-list 'load-path "~/.emacs.d/pkg-el23"))
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(package-refresh-contents)

(setq pkg-list
      '(
	auto-complete
	clojure-mode
	color-theme
	go-mode
	graphviz-dot-mode
	haskell-mode
	markdown-mode
	paredit
	scala-mode
	slime
	;slime-clj
	))

(dolist (pkg pkg-list)
  (when (not (package-installed-p pkg))
    (package-install pkg)))
