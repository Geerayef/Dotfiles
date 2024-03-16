;;;; orderless.el --- Orderless -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package orderless
  :ensure t
  :config
  (orderless-define-completion-style +orderless-with-initialism
                                     (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))
  (setq completion-styles '(partial-completion substring flex orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion)))
                                        (command (styles +orderless-with-initialism))
                                        (variable (styles +orderless-with-initialism))
                                        (symbol (styles +orderless-with-initialism)))
        ;; allow escaping space
        orderless-component-separator #'orderless-escapable-split-on-space)


(elpaca-wait)

(provide 'orderless)
;;; orderless.el ends here
