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

;; (defvar eglot-server-programs ())
;; 
;; (defun add-eglot-hooks (mode-list)
;;   "Add `eglot-ensure' to modes in MODE-LIST.
;;   The mode must be loaded, i.e. found with `fboundp'.  A mode which
;;   is not loaded will not have a hook added, in which case add it
;;   manually with something like this:
;;   `(add-hook \='some-mode-hook #\='eglot-ensure)'"
;;   (dolist (mode-def mode-list)
;;     (let ((mode (if (listp mode-def) (car mode-def) mode-def)))
;;       (cond
;;        ((listp mode) (add-eglot-hooks mode))
;;        (t (when (and (fboundp mode)
;;                      (not (eq 'clojure-mode mode))  ; prefer cider
;;                      (not (eq 'lisp-mode mode))     ; prefer sly/slime
;;                      (not (eq 'scheme-mode mode)))  ; prefer geiser
;;             (let ((hook-name (format "%s-hook" (symbol-name mode))))
;;               (add-hook (intern hook-name) #'eglot-ensure))))))))
;; 
;; (defun lsp-exists-p (mode-def)
;;   "Return non-nil if LSP binary of MODE-DEF is found via `executable-find'."
;;   (let ((lsp-program (cdr mode-def)))
;;     ;; `lsp-program' is either a list of strings or a function object
;;     ;; calling `eglot-alternatives'.
;;     (if (functionp lsp-program)
;;         (condition-case nil
;;             (car (funcall lsp-program))
;;           (error nil))
;;       (executable-find (car lsp-program)))))
;; 
;; (defun eglot-auto-ensure ()
;;   "Add `eglot-ensure' to major modes that offer LSP support.
;; Major modes are only selected if the major mode's associated LSP
;;   binary is detected on the system."
;;   (add-eglot-hooks (seq-filter #'lsp-exists-p eglot-server-programs)))
;; 
;; (use-package eglot
;;   :ensure nil
;;   :config
;;   (setq eglot-confirm-server-initiated-edits nil
;;         eglot-events-buffer-size 0
;;         eglot-sync-connect 0
;;         eglot-autoshutdown t)
;;   (fset #'jsonrpc--log-event #'ignore)
;;   (add-to-list 'eglot-server-programs
;;                '((c-ts-mode c++-ts-mode) . ("clangd"
;;                                             "-j=8"
;;                                             "--log=error"
;;                                             "--malloc-trim"
;;                                             "--background-index"
;;                                             "--clang-tidy"
;;                                             "--cross-file-rename"
;;                                             "--completion-style=detailed"
;;                                             "--pch-storage=memory"
;;                                             "--header-insertion=never"
;;                                             "--header-insertion-decorators=0"))
;;                '((python-mode python-ts-mode) . ("basedpyright-langserver" "--stdio"))
;;                '(ocaml-ts-mode . ("dune" "build" "--watch"))
;;                '(js-ts-mode . ("biome" "lsp-proxy")))
;;   :bind
;;   (:map eglot-mode-map
;;         ("C-c e c a" . eglot-code-actions)
;;         ("C-c e i o" . eglot-code-actions-organize-imports)
;;         ("C-c e r n" . eglot-rename)
;;         ("C-c e h" . eldoc)
;;         ("C-c e g d" . xref-find-definitions))
;;   :hook
;;   ((ocaml-ts-mode
;;     go-ts-mode
;;     js-ts-mode
;;     python-ts-mode
;;     rust-ts-mode
;;     c-ts-mode
;;     c++-ts-mode) . eglot-auto-ensure))

;; ~ LSP mode ------------------------------------------------------------- ~ ;;

(use-package lsp-mode
  :ensure t
  :defer t
  :hook ((ocaml-ts-mode . lsp)
         (c-ts-mode . lsp)
         (c++-ts-mode . lsp)
         (rust-ts-mode . lsp)
         (go-ts-mode . lsp)
         (bash-ts-mode . lsp)
         (typescript-ts-mode . lsp)
         (js-ts-mode . lsp))
  :commands lsp
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-inlay-hint-enable t)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil)
  (lsp-idle-delay 0)
  (lsp-keep-workspace-alive nil)
  ;; Core settings
  (lsp-auto-configure t)
  (lsp-eldoc-enable-hover t)
  (lsp-enable-xref t)
  (lsp-enable-snippet t)
  (lsp-enable-links nil)
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)
  (lsp-enable-imenu t)
  (lsp-enable-indentation nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-suggest-server-download t)
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-text-document-color nil)
  ;; Modeline settings
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-modeline-workspace-status-enable t)
  (lsp-signature-doc-lines 1)
  (lsp-eldoc-render-all nil)
  ;; Completion settings
  (lsp-completion-provider :capf)
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit t)
  (lsp-completion-show-kind t)
  ;; Lens settings
  (lsp-lens-enable t)
  ;; Headerline settings
  (lsp-headerline-breadcrumb-enable-symbol-numbers t)
  (lsp-headerline-arrow "▶")
  (lsp-headerline-breadcrumb-enable-diagnostics nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  ;; Semantic settings
  (lsp-semantic-tokens-enable nil))

(provide 'mod-lsp)
;;; mod-lsp.el ends here
