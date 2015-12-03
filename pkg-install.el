;;; pkg-install.el --- installs add-on packages

;;; Commentary:
;; 

(require 'package)

;;; Code:

(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/")
             '("melpa" . "http://melpa.org/packages/")
             )
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
        cargo
        cider
        color-theme-solarized
        elpy
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
        php-mode
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
