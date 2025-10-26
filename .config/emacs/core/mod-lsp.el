;;; mod-lsp.el --- LSP -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; ~ Yasnippet ------------------------------------------------------------ ~ ;;

(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
  :config (yas-reload-all))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; ~ Eglot ---------------------------------------------------------------- ~ ;;

(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :hook ((ocaml-ts-mode
          rust-ts-mode
          haskell-ts-mode
          c-ts-mode
          c++-ts-mode
          c-or-c++-ts-mode
          go-ts-mode
          python-ts-mode
          lua-ts-mode
          bash-ts-mode
          js-ts-mode
          typescript-ts-mode
          ;; Fallback: Non-TS modes
          tuareg-mode
          caml-mode
          haskell-mode
          rust-mode
          c-mode
          c++-mode
          go-mode
          lua-mode
          python-mode
          js-mode
          typescript-mode) . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-initiated-edits nil)
  (eglot-sync-connect 0)
  (eglot-report-progress t)
  :config
  (fset #'jsonrpc--log-event #'ignore)
  (setq-default eglot-events-buffer-size 0)
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
  (push '((haskell-ts-mode haskell-mode) . ("haskell-language-server-wrapper" "--lsp")) eglot-server-programs)
  (setq-default eglot-workspace-configuration
                '(:haskell (:plugin (:stan (:globalOn :json-false))
                                    :formattingProvider "")))
  :bind
  (:map eglot-mode-map
        ("C-c l c a" . eglot-code-actions)
        ("C-c l i o" . eglot-code-actions-organize-imports)
        ("C-c l r n" . eglot-rename)
        ("C-c l h" . eldoc)
        ("C-c l g d" . xref-find-definitions)))

(provide 'mod-lsp)
;;; mod-lsp.el ends here
