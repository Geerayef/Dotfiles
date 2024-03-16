;;;; consult.el --- Consult -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package consult
  :ensure t
  :bind
  (("C-c M-x" . consult-mode-command)
  ("C-c h" . consult-history)
  ("C-c k" . consult-kmacro)
  ("C-c m" . consult-man)
  ("C-c i" . consult-info)
  ([remap Info-search] . consult-info)
  ;; ~  --------------------------------------------------------------------------------  ~ ;;
  ;; C-x bindings in `ctl-x-map'
  ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
  ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
  ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
  ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
  ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
  ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
  ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
  ;; ~  --------------------------------------------------------------------------------  ~ ;;
  ;; M-g bindings in `goto-map'
  ("M-g e" . consult-compile-error)
  ("M-g d" . consult-flycheck)
  ("M-g g" . consult-goto-line)             ;; orig. goto-line
  ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
  ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
  ("M-g m" . consult-mark)
  ("M-g k" . consult-global-mark)
  ("M-g i" . consult-imenu)
  ("M-g I" . consult-imenu-multi)
  ;; ~  --------------------------------------------------------------------------------  ~ ;;
  ;; M-s bindings in `search-map'
  ("M-s f" . consult-fd)
  ("M-s g" . consult-ripgrep)
  ("M-s G" . consult-git-grep)
  ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
  ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
  ("M-s k" . consult-keep-lines)
  ("M-s u" . consult-focus-lines)
  ("M-s e" . consult-isearch-history)
  ;; ~  --------------------------------------------------------------------------------  ~ ;;
  ("C-s" . consult-isearch)
  ("M-y" . consult-yank-pop)                ;; orig. yank-pop
  ;; M-# bindings for fast register access
  ("M-#" . consult-register-load)
  ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
  ("C-M-#" . consult-register))
  ;; ~  --------------------------------------------------------------------------------  ~ ;;
  ;; Isearch integration
  :map isearch-mode-map
  ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
  ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
  ;; ~  --------------------------------------------------------------------------------  ~ ;;
  ;; Minibuffer history
  :map minibuffer-local-map
  ("M-s" . consult-history)                 ;; orig. next-matching-history-element
  ("M-r" . consult-history)                 ;; orig. previous-matching-history-element
  ;; ~  --------------------------------------------------------------------------------  ~ ;;
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  ;; ~  --------------------------------------------------------------------------------  ~ ;;
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5
     register-preview-function #'consult-register-format)
  (setq xref-show-xrefs-function #'consult-xref
     xref-show-definitions-function #'consult-xref)
  (setq completion-in-region-function #'consult-completion-in-region)
  ;; ~  --------------------------------------------------------------------------------  ~ ;;
  :config
  (setq consult-preview-key 'any)
  (consult-customize
  consult-theme :preview-key '(:debounce 0.2 any)
  consult-ripgrep consult-git-grep consult-grep
  consult-bookmark consult-recent-file consult-xref
  consult--source-bookmark consult--source-file-register
  consult--source-recent-file consult--source-project-recent-file
  :preview-key '(:debounce 0.4 "M-."))
  (setq consult-fd-args "fd --hidden --color=never --type f --type d"
     consult-narrow-key "C-+"))

;; Optionally make narrowing help available in the minibuffer.
;; You may want to use `embark-prefix-help-command' or which-key instead.
;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
;; By default `consult-project-function' uses `project-root' from project.el.

;; Project root finding function config:
;;;; 1. project.el (the default)
;; (setq consult-project-function #'consult--default-project--function)
;;;; 2. vc.el (vc-root-dir)
;; (setq consult-project-function (lambda (_) (vc-root-dir)))
;;;; 3. locate-dominating-file
;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
;;;; 4. projectile.el (projectile-project-root)
;; (autoload 'projectile-project-root "projectile")
;; (setq consult-project-function (lambda (_) (projectile-project-root)))
;;;; 5. No project support
;; (setq consult-project-function nil)

(elpaca-wait)

;; ~  --------------------------------------------------------------------------------  ~ ;;

(use-package affe
             :config
             (defun affe-orderless-regexp-compiler (input _type _ignorecase)
               (setq input (cdr (orderless-compile input)))
               (cons input (apply-partially #'orderless--highlight input t)))
             (setq affe-regexp-compiler #'affe-orderless-regexp-compiler)
             (consult-customize
               affe-grep :preview-key "M-."))

(elpaca-wait)

(use-package consult-eglot :ensure t)

(provide 'consult)
;;; consult.el ends here
