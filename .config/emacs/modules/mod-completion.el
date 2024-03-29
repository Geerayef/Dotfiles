;;;; mod-completion.el --- Completion configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq tab-always-indent 'complete)

;; ~  --------------------------------------------------------------------------------  ~ ;;

;; ~  Orderless

(use-package orderless
  :ensure t
  :demand t
  :config
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))
  (setq completion-styles '(orderless partial-completion substring basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (flex)))
                                        (command (styles +orderless-with-initialism))
                                        (variable (styles +orderless-with-initialism))
                                        (symbol (styles +orderless-with-initialism))
                                        (eglot (styles orderless))
                                        (eglot-capf (styles orderless)))
        orderless-component-separator #'orderless-escapable-split-on-space))

;; ~  --------------------------------------------------------------------------------  ~ ;;

;; ~  Consult

(use-package consult
  :ensure t
  :bind
  (("C-c M-x" . consult-mode-command)
   ("C-c h" . consult-history)
   ("C-c k" . consult-kmacro)
   ("C-c m" . consult-man)
   ("C-c i" . consult-info)
   ;; ([remap Info-search] . consult-info)
   ;; C-x bindings in `ctl-x-map'
   ;; orig. repeat-complex-command
   ("C-x M-:" . consult-complex-command)
   ;; orig. switch-to-buffer
   ("C-x b" . consult-buffer)
   ;; orig. switch-to-buffer-other-window
   ("C-x 4 b" . consult-buffer-other-window)
   ;; orig. switch-to-buffer-other-frame
   ("C-x 5 b" . consult-buffer-other-frame)
   ;; orig. switch-to-buffer-other-tab
   ("C-x t b" . consult-buffer-other-tab)
   ;; orig. bookmark-jump
   ("C-x r b" . consult-bookmark)
   ;; orig. project-switch-to-buffer
   ("C-x p b" . consult-project-buffer)
   ;; M-g bindings in `goto-map'
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flycheck)
   ;; orig. goto-line
   ("M-g g" . consult-goto-line)
   ;; orig. goto-line
   ("M-g M-g" . consult-goto-line)
   ;; Alternative: consult-org-heading
   ("M-g o" . consult-outline)
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ;; M-s bindings in `search-map'
   ("M-s f" . consult-fd)
   ("M-s g" . consult-ripgrep)
   ("M-s G" . consult-git-grep)
   ;; needed by consult-line to detect isearch
   ("M-s l" . consult-line)
   ;; needed by consult-line to detect isearch
   ("M-s L" . consult-line-multi)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
   ("M-s e" . consult-isearch-history)
   ;; orig. yank-pop
   ("M-y" . consult-yank-pop)
   ;; M-# bindings for fast register access
   ("M-#" . consult-register-load)
   ;; orig. abbrev-prefix-mark (unrelated)
   ("M-'" . consult-register-store)
   ("C-M-#" . consult-register)
   ;; Isearch integration
   :map isearch-mode-map
   ;; orig. isearch-edit-string
   ("M-e" . consult-isearch-history)
   ;; orig. isearch-edit-string
   ("M-s e" . consult-isearch-history)
   ;; Minibuffer history
   :map minibuffer-local-map
   ;; orig. next-matching-history-element
   ("M-s" . consult-history)
   ;; orig. previous-matching-history-element
   ("M-r" . consult-history))
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (setq completion-in-region-function #'consult-completion-in-region)
  :custom
  (setq consult-preview-key 'any)
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 "M-."))
  (setq consult-fd-args '("fd" "-H" "-i" "-E .git node_modules" "--prune" "--color=never" "-t f")
        consult-ripgrep-args (concat consult-ripgrep-args " --hidden")
        consult-narrow-key "<"))

(use-package consult-flycheck :ensure t :after consult)

(use-package consult-eglot :ensure t :after consult)

;; ~  --------------------------------------------------------------------------------  ~ ;;

;; ~  Affe

(use-package affe
  :ensure t
  :after (orderless consult)
  :config
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (cdr (orderless-compile input)))
    (cons input (apply-partially #'orderless--highlight input t)))
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler
        affe-regexp-function #'orderless-pattern-compiler
        affe-highlight-function #'orderless-highlight-matches)
  (consult-customize affe-grep :preview-key "M-."))

;; ~  --------------------------------------------------------------------------------  ~ ;;

;; ~  Marginalia

(use-package marginalia
  :ensure t
  :after vertico
  :config
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

;; ~  --------------------------------------------------------------------------------  ~ ;;

;; ~  Cape

(use-package cape
  :ensure t
  :config
  (defun my/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point
                       #'tempel-expand
                       #'cape-file))))
  :hook (eglot-managed-mode . my/eglot-capf)
  :bind (("C-c p p" . completion-at-point)
         ("C-c p t" . complete-tag)
         ("C-c p d" . cape-dabbrev)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
		 ("C-c p l" . cape-line)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
         ("C-c p w" . cape-dict)
         ("C-c p :" . cape-emoji)
         ("C-c p _" . cape-tex))
  ;; ("C-c p \\" . cape-tex)
  ;; ("C-c p ^" . cape-tex)
  ;; ("C-c p h" . cape-history)
  ;; ("C-c p a" . cape-abbrev)
  ;; ("C-c p &" . cape-sgml)
  ;; ("C-c p r" . cape-rfc1345)
  :init
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol))

;; ~  --------------------------------------------------------------------------------  ~ ;;

;; ~  Vertico

(use-package vertico
  :ensure t
  :demand t
  :custom
  (completion-cycle-threshold 1)
  (completions-detailed t)
  (vertico-cycle t)
  (vertico-count 10)
  (vertico-scroll-margin 0)
  (vertico-resize t)
  (enable-recursive-minibuffers t)
  :init
  (fido-mode -1)
  (fido-vertical-mode -1)
  (icomplete-mode -1)
  (icomplete-vertical-mode -1)
  (vertico-mode 1))

;; ~  --------------------------------------------------------------------------------  ~ ;;

;; ~  Corfu

(use-package corfu
  :ensure (corfu :files (:defaults "extensions/*")
                 :includes (corfu-popupinfo))
  :custom
  (corfu-cycle t)
  (corfu-auto nil)
  (corfu-auto-prefix 2)
  (corfu-quit-no-match t)
  :config
  (eldoc-add-command #'corfu-insert)
  ;; (corfu-separator ?\s)
  ;; (corfu-quit-at-boundary nil)
  ;; (corfu-preview-current nil)
  ;; (corfu-preselect 'prompt)
  ;; (corfu-on-exact-match nil)
  ;; (corfu-scroll-margin 5)
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("M-d" . corfu-popupinfo-toggle)
        ("M-p" . corfu-popupinfo-scroll-down)
        ("M-n" . corfu-popupinfo-scroll-up))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

;; ~  --------------------------------------------------------------------------------  ~ ;;

;; ~  Embark

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-h B" . embark-bindings))
  :config
  (setq prefix-help-command #'embark-prefix-help-command)
  (add-to-list 'display-buffer-alist
			   '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; ~  --------------------------------------------------------------------------------  ~ ;;

(provide 'mod-completion)
;;; mod-completion.el ends here
