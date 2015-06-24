(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Local packages
(add-to-list 'load-path "~/.emacs.d/misc")

;; clipboard copy
(setq x-select-enable-clipboard t)

;; Color
(defun color-theme-dark-bliss ()
  ""
  (interactive)
  (color-theme-install
   '(color-theme-dark-bliss
     ((foreground-color . "#eeeeee")
      (background-color . "#001122")
      (background-mode . dark)
      (cursor-color . "#ccffcc"))
     (bold ((t (:bold t))))
     (bold-italic ((t (:italic t :bold t))))
     (default ((t (nil))))

     (font-lock-builtin-face ((t (:foreground "#f0f0aa"))))
     (font-lock-comment-face ((t (:italic t :foreground "#aaccaa"))))
     (font-lock-delimiter-face ((t (:foreground "#aaccaa"))))
     (font-lock-constant-face ((t (:bold t :foreground "#ffaa88"))))
     (font-lock-doc-string-face ((t (:foreground "#eeccaa"))))
     (font-lock-doc-face ((t (:foreground "#eeccaa"))))
     (font-lock-reference-face ((t (:foreground "#aa99cc"))))
     (font-lock-function-name-face ((t (:foreground "#ffbb66"))))
     (font-lock-keyword-face ((t (:foreground "#ccffaa"))))
     (font-lock-preprocessor-face ((t (:foreground "#aaffee"))))
     (font-lock-string-face ((t (:foreground "#bbbbff")))))))

(if (version< emacs-version "24")
    (begin
     (require 'color-theme)
     (color-theme-dark-bliss))
    (load-theme 'deeper-blue t))

;;;;  Clojure

;;; CoffeeScript
;(add-to-list 'load-path "~/.emacs.d/coffee-mode")
;(require 'coffee-mode)

;;; git-wip
;(load "~/checkouts/git-wip/emacs/git-wip.el")

;;; Markdown
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;; NXML
(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))


;;; Org-mode
;; now bundled with Emacs but we want the latest version
(add-to-list 'load-path "~/.emacs.d/org-mode/lisp")
(add-to-list 'load-path "~/.emacs.d/org-mode/contrib/lisp")
(require 'org-install)

;; The following lines are always needed. Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on

;;;; also integrate with bibtex
;;;; http://www.mfasold.net/blog/2009/02/using-emacs-org-mode-to-draft-papers/

(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name)
       (file-exists-p (buffer-file-name))
       (reftex-parse-all))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
  )
(add-hook 'org-mode-hook 'org-mode-reftex-setup)

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)


;; require or autoload paredit-mode
(defun turn-on-paredit () (paredit-mode 1))
(add-hook 'clojure-mode-hook 'turn-on-paredit)


;; rainbow-mode
;; from https://github.com/purcell/emacs.d/blob/master/init-css.el
(autoload 'rainbow-turn-on "rainbow-mode"
  "Enable rainbow mode color literal overlays")
(dolist (hook '(css-mode-hook
		emacs-lisp-mode-hook
		html-mode-hook
		sass-mode-hook))
  (add-hook hook 'rainbow-turn-on))


;; Scala
; (add-to-list 'load-path "~/opt/misc/scala-tool-support/emacs")
; (require 'scala-mode-auto)
(when (file-exists-p "~/apps/ensime/elisp")
  (add-to-list 'load-path "~/apps/ensime/elisp")
  (require 'ensime)
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))


(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(delete-selection-mode nil)
 '(inhibit-startup-screen t)
 '(org-mobile-directory "~/Desktop/mobile.org")
 '(rpm-spec-user-full-name "Michel Salim")
 '(rpm-spec-user-mail-address "salimma@fedoraproject.org")
 '(scroll-bar-mode (quote right)))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;; from http://technomancy.us/135
;; in which the maintainer's perspective is considered
(require 'whitespace)

(setq whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 80)

;; add hooks for every major mode you use
(add-hook 'clojure-mode-hook (lambda () (whitespace-mode 1)))

;; XQuery
(require 'xquery-mode)
(add-to-list 'auto-mode-alist '("\\.xq\\'" . xquery-mode))
