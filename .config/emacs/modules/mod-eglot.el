;;;; eglot.el --- Eglot -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun add-eglot-hooks (mode-list)
  "Add `eglot-ensure' to modes in MODE-LIST.
  The mode must be loaded, i.e. found with `fboundp'.  A mode which
  is not loaded will not have a hook added, in which case add it
  manually with something like this:
  `(add-hook 'some-mode-hook #'eglot-ensure)'"
  (dolist (mode-def mode-list)
    (let ((mode (if (listp mode-def) (car mode-def) mode-def)))
      (cond
        ((listp mode) (add-eglot-hooks mode))
        (t
          (when (and (fboundp mode)
                     (not (eq 'clojure-mode mode))  ; prefer cider
                     (not (eq 'lisp-mode mode))     ; prefer sly/slime
                     (not (eq 'scheme-mode mode)))  ; prefer geiser
            (let ((hook-name (format "%s-hook" (symbol-name mode))))
              (message "~~~~~ [INFO] Adding eglot to %s." hook-name)
              (add-hook (intern hook-name) #'eglot-ensure))))))))

(defun lsp-exists (mode-def)
  "Return non-nil if LSP binary of MODE-DEF is found via `executable-find'."
  (let ((lsp-program (cdr mode-def)))
    ;; `lsp-program' is either a list of strings or a function object
    ;; calling `eglot-alternatives'.
    (if (functionp lsp-program)
      (condition-case nil
                      (car (funcall lsp-program))
                      (error nil))
      (executable-find (car lsp-program)))))

(defun eglot-auto-ensure ()
  "Add `eglot-ensure' to major modes that offer LSP support.
  Major modes are only selected if the major mode's associated LSP
  binary is detected on the system."
  (when (require 'eglot nil :noerror)
    (add-eglot-hooks (seq-filter #'lsp-exists eglot-server-programs))))

(use-package eglot
  :ensure nil
  :hook
  ((prog-mode . eglot-ensure)
   (go-ts-mode . eglot-ensure)
   (java-ts-mode . eglot-ensure)
   (python-ts-mode . eglot-ensure)
   (rust-ts-mode . eglot-ensure))
  :bind (:map eglot-mode-map
              ("C-c e a" . eglot-code-actions)
              ("C-c e o" . eglot-code-actions-organize-imports)
              ("C-c e r" . eglot-rename)
              ("C-c e h" . eldoc)
              ("C-c e g d" . xref-find-definitions))
  :config
  (setq eglot-confirm-server-initiated-edits nil
        eglot-events-buffer-size 0
        eglot-sync-connect 0
        eglot-autoshutdown t))

;; »»»»» Help prevent lag with the typescript language server.
(with-eval-after-load 'eglot (fset #'jsonrpc--log-event #'ignore))
;; ««««« TODO: Matbe mutually exclusive
(with-eval-after-load 'eglot
                      (add-to-list 'eglot-server-programs
                                   '((c-ts-mode c++-ts-mode)
                                     . ("clangd"
                                        "-j=8"
                                        "--log=error"
                                        "--malloc-trim"
                                        "--background-index"
                                        "--clang-tidy"
                                        "--cross-file-rename"
                                        "--completion-style=detailed"
                                        "--pch-storage=memory"
                                        "--header-insertion=never"
                                        "--header-insertion-decorators=0"))))
;; (define-key eglot-mode-map 
;; (define-key eglot-mode-map 

(provide 'mod-eglot)
;;; eglot.el ends here
