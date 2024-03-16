;;;; eglot.el --- Eglot -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun eglot-hooks (mode-list)
  "Add `eglot-ensure' to modes in MODE-LIST.
The mode must be loaded, i.e. found with `fboundp'.  A mode which
is not loaded will not have a hook added, in which case add it
manually with something like this:
`(add-hook 'some-mode-hook #'eglot-ensure)'"
  (dolist (mode-def mode-list)
    (let ((mode (if (listp mode-def) (car mode-def) mode-def)))
      (cond
       ((listp mode) (eglot-hooks mode))
       (t
        (when (and (fboundp mode)
                   (not (eq 'clojure-mode mode))  ; prefer cider
                   (not (eq 'lisp-mode mode))     ; prefer sly/slime
                   (not (eq 'scheme-mode mode))   ; prefer geiser
                   )
          (let ((hook-name (format "%s-hook" (symbol-name mode))))
            (message "adding eglot to %s" hook-name)
            (add-hook (intern hook-name) #'eglot-ensure))))))))

(defun lsp-exists (mode-def)
  "Return non-nil if LSP binary of MODE-DEF is found via `executable-find'."
  (let ((lsp-program (cdr mode-def)))
    ;; `lsp-program' is either a list of strings or a function object
    ;; calling `eglot-alternatives'.
    (if (functionp lsp-program)
        (condition-case nil
            (car (funcall lsp-program))
          ;; When an error occurs it's because Eglot checked for a
          ;; binary and didn't find one among alternatives.
          (error nil))
      (executable-find (car lsp-program)))))

(defun eglot-auto-ensure ()
  "Add `eglot-ensure' to major modes that offer LSP support.
Major modes are only selected if the major mode's associated LSP
binary is detected on the system."
  (when (require 'eglot nil :noerror)
    (eglot-hooks (seq-filter #'lsp-exists eglot-server-programs))))

;; Shutdown server when last managed buffer is killed
(customize-set-variable 'eglot-autoshutdown t)

;; (use-package eglot :ensure nil :defer t
;;   :custom-face
;;   ;; personal preference here; I hate it when packages
;;   ;; use the `shadow' face for stuff, it's impossible to read
;;   (eglot-inlay-hint-face
;;    ((t ( :foreground unspecified
;;          :inherit font-lock-comment-face))))
;;   :config
;;   ;; these two lines help prevent lag with the typescript language server. they
;;   ;; might actually be mutually exclusive but I haven't investigated further
;;   (with-eval-after-load 'eglot (fset #'jsonrpc--log-event #'ignore))
;;   (setq eglot-events-buffer-size 0)
;;   ;; just do it, don't prompt me
;;   (setq eglot-confirm-server-initiated-edits nil)
;;   (setq eglot-sync-connect 0)
;;   (setq eglot-autoshutdown t)
;;   (setq rex/language-servers
;;         (list '(tsx-ts-mode "typescript-language-server" "--stdio")
;;               '(php-mode "phpactor" "language-server")))
;;   (dolist (server rex/language-servers)
;;     (add-to-list 'eglot-server-programs server))
;;   :hook
;;   (php-mode . eglot-ensure)
;;   (typescript-ts-mode . eglot-ensure)
;;   (tsx-ts-mode . eglot-ensure)
;;   (eglot-managed-mode
;;    . (lambda () (setq eldoc-documentation-function
;;                       'eldoc-documentation-compose-eagerly))))

;;; eglot.el ends here
