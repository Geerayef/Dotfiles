;;;; lisp.el --- Lisp development configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;; For Scheme and Racket, configure geiser.
;;   Out of the box, geiser already supports some scheme
;;   implementations.  However, there are several modules which can be
;;   added to geiser for specific implementations:
;;   * geiser-chez
;;   * geiser-chibi
;;   * geiser-chicken
;;   * geiser-gambit
;;   * geiser-gauche
;;   * geiser-guile
;;   * geiser-kawa
;;   * geiser-mit
;;   * geiser-racket
;;   * geiser-stklos
;;; Code:

(require 'eldoc)

;; ~  Indentation

(use-package aggressive-indent
  :ensure t
  :hook
  ((lisp-mode
    emacs-lisp-mode
    clojure-mode
    scheme-mode) . aggressive-indent-mode))

;; ~  Emacs Lisp

(use-package package-lint :ensure t)

;; ~  Common Lisp

(use-package sly
  :ensure t
  :hook (lisp-mode . sly-editing-mode)
  ;; :config
  ;; Uncomment and update if you need to set the path to an
  ;; implementation of common lisp. This would be needed only if you
  ;; have multiple instances of common lisp installed, for example,
  ;; both CLISP and SBCL. In this case, we are assuming SBCL.
  ;; (setq inferior-lisp-program "/usr/bin/sbcl")
  )
(use-package sly-asdf
  :ensure t
  :after sly)
(use-package sly-quicklisp
  :ensure t
  :after sly)
(use-package sly-repl-ansi-color
  :ensure t
  :after sly)

;; ~  Scheme and Racket

(use-package geiser
  :ensure t
  :custom (scheme-program-name "guile"))
(use-package geiser-guile :ensure t :after geiser)
;; (use-package geiser-racket :ensure t :after geiser)

;; ~  Clojure

;; (with-eval-after-load "clojure-mode"
;;   (require 'cider "cider" :no-error)
;;   (require 'clj-refactor "clj-refactor" :no-error)
;;   (defun crafted-lisp-load-clojure-refactor ()
;;     "Load `clj-refactor' toooling and fix keybinding conflicts with cider."
;;     (when (locate-library "clj-refactor")
;;       (clj-refactor-mode 1)
;; keybindings mentioned on clj-refactor github page
;; conflict with cider, use this by default as it does
;; not conflict and is a better mnemonic
;;       (cljr-add-keybindings-with-prefix "C-c r")))
;;   (add-hook 'clojure-mode-hook #'crafted-lisp-load-clojure-refactor)
;;   (with-eval-after-load "flycheck"
;;     (flycheck-clojure-setup)))
;; (use-package cider)
;; (use-package clj-refactor)
;; (use-package clojure-mode)
;; (use-package flycheck-clojure)

(provide 'lisp)
;;; lisp.el ends here
