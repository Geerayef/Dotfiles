;;;; orderless.el --- Orderless -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package orderless
  :ensure t
  :demand t
  :custom
  (orderless-define-completion-style +orderless-with-initialism
                                     (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))
  :config
  (setq completion-styles '(orderless partial-completion substring basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion)))
                                        (command (styles +orderless-with-initialism))
                                        (variable (styles +orderless-with-initialism))
                                        (symbol (styles +orderless-with-initialism))
                                        (eglot (styles orderless))
                                        (eglot-capf (styles orderless)))
        orderless-component-separator #'orderless-escapable-split-on-space
        ;; orderless-component-separator /
        ))

;; Use Orderless as pattern compiler for consult-grep/ripgrep/find
;; (defun consult--orderless-regexp-compiler (input type &rest _config)
;;   (setq input (orderless-pattern-compiler input))
;;   (cons
;;     (mapcar (lambda (r) (consult--convert-regexp r type)) input)
;;     (lambda (str) (orderless--highlight input str))))
;;
;; (setq consult--regexp-compiler #'consult--orderless-regexp-compiler)

(provide 'orderless)
;;; orderless.el ends here
