;;; pkg-install.el --- installs add-on packages

;;; Commentary:
;; 

(require 'package)

;;; Code:

(add-to-list 'package-archives
             '("stable" . "http://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(package-refresh-contents)

(defvar pkg-list)
(setq pkg-list
      '(
        ansible
        ansible-doc
        auto-complete
        cider
        color-theme-solarized
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
        lorem-ipsum
        lua-mode
        markdown-mode
        nginx-mode
        paredit
        powerline
        rainbow-mode
        rust-mode
        sphinx-doc
        yaml-mode
        ))

(when (< emacs-major-version 24)
  (add-to-list 'pkg-list 'color-theme))

(dolist (pkg pkg-list)
  (when (not (package-installed-p pkg))
    (package-install pkg)))

(provide 'pkg-install)

;;; pkg-install.el ends here
