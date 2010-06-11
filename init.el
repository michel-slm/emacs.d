;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

;; for Emacs 22
(when (version< emacs-version "23")
  (transient-mark-mode 1)
  (add-to-list 'load-path "~/.emacs.d/emacs22"))

;; at work
(when (search "suse" (version))
  ;; color-theme is installed by hand, not via RPM
  ;; add its location to load path
  (add-to-list 'load-path "~/.emacs.d/color-theme"))


;; Color
(require 'color-theme)

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

(color-theme-dark-bliss)


(require 'whitespace)

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; Clojure
(add-to-list 'load-path "~/checkouts/clojure-mode")
(require 'clojure-mode)

;; http://www.mail-archive.com/clojure@googlegroups.com/msg28178.html
;; http://groups.google.com/group/swank-clojure/msg/2d77ad2909eef2e0
(require 'slime)
(setq slime-protocol-version 'ignore)

;; http://wiki.github.com/technomancy/leiningen/emacs-integration
(when (version<= "23" emacs-version)
  (defun lein-swank ()
    (interactive)
    (let ((root (locate-dominating-file default-directory "project.clj")))
      (when (not root)
	(error "Not in a Leiningen project."))
      ;; you can customize slime-port using .dir-locals.el
      (shell-command (format "cd %s && lein swank %s &" root slime-port)
		     "*lein-swank*")
      (set-process-filter (get-buffer-process "*lein-swank*")
			  (lambda (process output)
			    (when (string-match "Connection opened on" output)
			      (slime-connect "localhost" slime-port)
			      (set-process-filter process nil))))
      (message "Starting swank server..."))))

;; Org-mode
(add-to-list 'load-path "~/checkouts/org-mode/lisp")
(add-to-list 'load-path "~/checkouts/org-mode/contrib/lisp")
(require 'org-install)

;; The following lines are always needed. Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on

;; NXML
(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))

;; Scala
(add-to-list 'load-path "~/opt/misc/scala-tool-support/emacs")
(require 'scala-mode-auto)
(add-to-list 'load-path "~/apps/ensime/elisp")
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)


(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(delete-selection-mode nil)
 '(inhibit-startup-screen t)
 '(scroll-bar-mode (quote right)))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;; from http://technomancy.us/135
;; in which the maintainer's perspective is considered
(setq whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 80)

;; add hooks for every major mode you use
(add-hook 'clojure-mode-hook (lambda () (whitespace-mode 1)))
