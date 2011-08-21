(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

(package-menu-refresh)
(package-install 'slime-repl)
