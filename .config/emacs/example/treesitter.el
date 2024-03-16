;;;; treesitter.el --- Tree Sitter -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun treesitter-setup (opt-in-only)
  "Configure tree-sitter for Emacs 29 or later.
`OPT-IN-ONLY' is a list of symbols of language grammars to
auto-install instead of all grammars."
  (when (member "TREE_SITTER" (split-string system-configuration-features))
    (when (require 'treesit-auto nil :noerror)
      (when opt-in-only
        ;; (mapc (lambda (e) (add-to-list 'treesit-auto-langs e)) opt-in-only)
        (if (listp opt-in-only)
            (customize-set-variable 'treesit-auto-langs opt-in-only)
          (customize-set-variable 'treesit-auto-langs (list opt-in-only))))
      (global-treesit-auto-mode)
      (treesit-auto-install-all)
      ;; configure `auto-mode-alist' for tree-sitter modes relying on `fundamental-mode'
      (treesit-auto-add-to-auto-mode-alist))
    (when (locate-library "combobulate")
      ;; *-ts-modes eventually derive from this mode.
      (add-hook 'prog-mode-hook #'combobulate-mode))))

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

;; (use-package treesit-auto
;;   :custom
;;   (treesit-auto-install 'prompt)
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))

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
