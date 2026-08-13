;;;; mod-format.el --- Formatting -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; ~ EL-autofmt ----------------------------------------------------------- ~ ;;

(use-package elisp-autofmt
  :ensure (:host github :repo "emacsmirror/elisp-autofmt")
  :commands (elisp-autofmt-mode elisp-autofmt-buffer elisp-autofmt-region)
  :hook (emacs-lisp-mode . elisp-autofmt-mode))

;; ~ Apheleia ------------------------------------------------------------- ~ ;;

(use-package apheleia
  :ensure t
  :bind (("C-c f" . apheleia-format-buffer))
  :config
  (push '(shftm . ("shfmt" "-i2" "-s" "-bn" "-ci" "--filename" filepath))
        apheleia-formatters)
  (push '(mdformat . ("mdformat" filepath)) apheleia-formatters)
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff))
  (setf (alist-get 'go-ts-mode apheleia-mode-alist) '(gofmt))
  (setf (alist-get 'ocaml-ts-mode apheleia-mode-alist) '(dune))
  (setf (alist-get 'markdown-mode apheleia-mode-alist) '(mdformat)))

;; ~ Aggressive indent ---------------------------------------------------- ~ ;;

(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode 1))

(provide 'mod-format)
;;; mod-format.el ends here
