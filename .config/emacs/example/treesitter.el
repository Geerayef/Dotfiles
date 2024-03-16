;;;; treesitter.el --- Tree Sitter -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; (use-package tree-sitter
;;   :demand t
;;   :hook
;;   ((css-mode
;;     js-mode
;;     json-mode
;;     php-mode
;;     rust-mode
;;     tuareg-mode
;;     python-mode
;;     sh-mode
;;     typescript-mode
;;     yaml-mode) . enable)
;;   :preface
;;   (defun enable ()
;;     (tree-sitter-mode t))
;;   :defer t)

;; (use-package tree-sitter-langs
;;   :after tree-sitter
;;   :hook
;;   (tree-sitter-after-on . tree-sitter-hl-mode))

;; (use-package emacs :ensure nil
;;   :after treesit
;;   :custom-face
;;   (typescript-ts-jsx-tag-face
;;    ((t ( :inherit font-lock-type-face))))
;;   :mode
;;   ("\\.js$" . js-ts-mode)
;;   ("\\.ts$" . typescript-ts-mode)
;;   ("\\.tsx$" . tsx-ts-mode))

;;; treesitter.el ends here
