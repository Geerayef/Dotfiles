;;;; lisp.el --- Lisp development configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;; For Scheme and Racket, configure `geiser' and additional lips flavours.
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

;; ~ ELisp ----------------------------------------------------------------- ~ ;;

(use-package eldoc
  :hook
  (emacs-lisp-mode . eldoc-mode))

(use-package package-lint :ensure t)

;; ~ Common Lisp ----------------------------------------------------------- ~ ;;

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

;; ~ Geiser ---------------------------------------------------------------- ~ ;;

(use-package geiser
  :ensure t
  :custom (scheme-program-name "guile"))
(use-package geiser-guile :ensure t :after geiser)

(provide 'lisp)
;;; lisp.el ends here
