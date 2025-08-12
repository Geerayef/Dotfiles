;;; mod-lsp.el --- LSP -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; ~ Yasnippet ------------------------------------------------------------ ~ ;;

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
  :config (yas-reload-all))

(use-package yasnippet-snippets
  :ensure t)

;; ~ LSP Bridge ----------------------------------------------------------- ~ ;;

;; (use-package lsp-bridge
;;   :ensure (:host github :repo "manateelazycat/lsp-bridge"))

;; ~ Eglot ---------------------------------------------------------------- ~ ;;

(defun add-eglot-hooks (mode-list)
  "Add `eglot-ensure' to modes in MODE-LIST.
  The mode must be loaded, i.e. found with `fboundp'."
  (dolist (mode-def mode-list)
    (let ((mode (if (listp mode-def) (car mode-def) mode-def)))
      (cond
       ((listp mode) (add-eglot-hooks mode))
       (t (when (and (fboundp mode)
                     (not (eq 'clojure-mode mode))
                     (not (eq 'lisp-mode mode))
                     (not (eq 'scheme-mode mode)))
            (let ((hook-name (format "%s-hook" (symbol-name mode))))
              (message " >>> [INFO] Eglot -- Ensure %s" hook-name)
              (add-hook (intern hook-name) #'eglot-ensure))))))))

(defun lsp-exists-p (mode-def)
  "Return non-nil if LSP binary of MODE-DEF is found via `executable-find'."
  (let ((lsp-program (cdr mode-def)))
    (cond
     ((functionp lsp-program)
      (condition-case nil (executable-find (car (funcall lsp-program))) (error nil)))
     ((listp lsp-program) (executable-find (car lsp-program)))
     (t (executable-find lsp-program)))))

(defun eglot-auto-ensure ()
  "Add `eglot-ensure' to major modes that have LSP installed."
  (add-eglot-hooks (seq-filter #'lsp-exists-p eglot-server-programs)))

(use-package eglot
  :ensure nil
  :config
  (setq eglot-confirm-server-initiated-edits nil
        eglot-events-buffer-size 0
        eglot-sync-connect 0
        eglot-autoshutdown t)
  (fset #'jsonrpc--log-event #'ignore)
  (push '((c-ts-mode c++-ts-mode) . ("clangd"
                                     "-j=8"
                                     "--log=error"
                                     "--malloc-trim"
                                     "--background-index"
                                     "--clang-tidy"
                                     "--cross-file-rename"
                                     "--completion-style=detailed"
                                     "--pch-storage=memory"
                                     "--header-insertion=never"
                                     "--header-insertion-decorators=0")
                                  ) eglot-server-programs)
  (push '(python-ts-mode . ("basedpyright-langserver" "--stdio")) eglot-server-programs)
  (push '(ocaml-ts-mode . ("ocamllsp")) eglot-server-programs)
  (push '(js-ts-mode . ("biome" "lsp-proxy")) eglot-server-programs)
  (eglot-auto-ensure)
  :bind
  (:map eglot-mode-map
        ("C-c l c a" . eglot-code-actions)
        ("C-c l i o" . eglot-code-actions-organize-imports)
        ("C-c l r n" . eglot-rename)
        ("C-c l h" . eldoc)
        ("C-c l g d" . xref-find-definitions)))

(provide 'mod-lsp)
;;; mod-lsp.el ends here
