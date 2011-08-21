(when (version< emacs-version "24")
  (add-to-list 'load-path "~/.emacs.d/pkg-el23"))
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-refresh-contents)
(package-install 'clojure-mode)
(package-install 'slime)
;(package-install 'slime-clj)
