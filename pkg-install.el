(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(package-refresh-contents)

(setq pkg-list
      '(
        auto-complete
        cider
        feature-mode
        go-mode
        graphviz-dot-mode
        haskell-mode
        markdown-mode
        paredit
        rainbow-mode
        ;rspec-mode
        ;scala-mode
        ))

(when (< emacs-major-version 24)
  (add-to-list 'pkg-list 'color-theme))

(dolist (pkg pkg-list)
  (when (not (package-installed-p pkg))
    (package-install pkg)))
