(require 'package)
(add-to-list 'package-archives
             '("stable" . "http://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(package-refresh-contents)

(setq pkg-list
      '(
        ansible
        ansible-doc
        auto-complete
        cider
        feature-mode
        flycheck-clojure
        flycheck-haskell
        flycheck-pyflakes
        flycheck-rust
        go-mode
        gradle-mode
        graphviz-dot-mode
        groovy-mode
        haskell-mode
        markdown-mode
        paredit
        rainbow-mode
        ;rspec-mode
        rust-mode
        ;scala-mode
        yaml-mode
        ))

(when (< emacs-major-version 24)
  (add-to-list 'pkg-list 'color-theme))

(dolist (pkg pkg-list)
  (when (not (package-installed-p pkg))
    (package-install pkg)))
