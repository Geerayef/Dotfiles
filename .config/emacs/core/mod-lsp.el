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

;; ~ Eglot ---------------------------------------------------------------- ~ ;;

;; LSP Configs

(setq-default eglot-workspace-configuration
              '(:gopls
                (
                 :usePlaceholders t
                 :analyses (
                            :unreachable t
                            :appendclipped t
                            :slicesdelete t
                            :QF1001 t
                            :QF1005 t
                            :QF1006 t
                            :QF1007 t
                            :QF1011 t
                            :S1002 t
                            :S1005 t
                            :S1006 t
                            :S1008 t
                            :S1016 t
                            :S1021 t
                            :S1025 t
                            :S1029 t
                            :SA1000 t
                            :SA1003 t
                            :SA1007 t
                            :SA1010 t
                            :SA1011 t
                            :SA1014 t
                            :SA1017 t
                            :SA1018 t
                            :SA1020 t
                            :SA1024 t
                            :SA1026 t
                            :SA1028 t
                            :SA1031 t
                            :SA1032 t
                            :SA4006 t
                            :SA4009 t
                            :SA4010 t
                            :SA4012 t
                            :SA4015 t
                            :SA4017 t
                            :SA4018 t
                            :SA4031 t
                            :SA5000 t
                            :SA5010 t
                            :SA5012 t
                            :SA6000 t
                            :SA6001 t
                            :SA6003 t
                            :SA9001 t
                            :SA9003 t
                            :SA9005 t
                            :SA9007 t
                            :SA9008 t
                            :ST1005 t
                            :ST1008 t
                            :ST1012 t
                            :ST1013 t
                            :ST1015 t
                            :ST1017 t
                            :ST1023 t))))

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
  "Add `eglot-ensure' to major modes that offer LSP support.
Major modes are only selected if the major mode's associated LSP
binary is detected on the system."
  (add-eglot-hooks (seq-filter #'lsp-exists-p eglot-server-programs)))

(use-package eglot
  :ensure nil
  :hook
  ((ocaml-ts-mode
    go-ts-mode
    js-ts-mode
    python-ts-mode
    rust-ts-mode
    c-ts-mode
    c++-ts-mode) . eglot-ensure)
  :custom
  (eglot-confirm-server-initiated-edits nil)
  (eglot-events-buffer-size 0)
  (eglot-sync-connect 0)
  (eglot-autoshutdown t)
  :config
  (add-to-list 'eglot-server-programs
               '((c-ts-mode c++-ts-mode) . ("clangd"
                                            "-j=8"
                                            "--log=error"
                                            "--malloc-trim"
                                            "--background-index"
                                            "--clang-tidy"
                                            "--cross-file-rename"
                                            "--completion-style=detailed"
                                            "--pch-storage=memory"
                                            "--header-insertion=never"
                                            "--header-insertion-decorators=0")))
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("basedpyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs '(go-ts-mode . ("gopls")))
  (add-to-list 'eglot-server-programs '(ocaml-ts-mode . ("ocamllsp")))
  (add-to-list 'eglot-server-programs '(js-ts-mode . ("biome" "lsp-proxy")))
  (eglot-auto-ensure)
  :bind
  (:map eglot-mode-map
        ("C-c l c a" . eglot-code-actions)
        ("C-c l r n" . eglot-rename)
        ("C-c l h" . eldoc)
        ("C-c l g d" . xref-find-definitions)))

(provide 'mod-lsp)
;;; mod-lsp.el ends here
