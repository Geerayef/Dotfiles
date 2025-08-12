;;; mod-completion.el --- Completion -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; ~ Orderless ------------------------------------------------------------ ~ ;;

(use-package orderless
  :ensure t
  :demand t
  :config
  (orderless-define-completion-style orderless+initialism
    (orderless-matching-styles
     '(orderless-initialism orderless-literal orderless-regexp)))
  :custom
  (completion-styles '(orderless partial-completion substring basic))
  (completion-category-defaults '(()))
  (completion-category-overrides '((file (styles flex))
                                   (command (styles orderless+initialism))
                                   (variable (styles orderless+initialism))
                                   (symbol (styles orderless+initialism))
                                   (eglot (styles orderless))
                                   (eglot-capf (styles orderless))))
  (orderless-component-separator #'orderless-escapable-split-on-space))

;; ~ Consult -------------------------------------------------------------- ~ ;;

(use-package consult
  :ensure t
  :after orderless
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref
        completion-in-region-function #'consult-completion-in-region)
  :config
  (setq consult-fd-args '("fd" "-H" "-i" "-E .git node_modules" "--prune" "--color=never" "-t f")
        consult-ripgrep-args (concat consult-ripgrep-args " --hidden")
        consult-narrow-key "<")
  (setq consult-preview-key 'any)
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 "M-."))
  :bind
  (("C-c M-x" . consult-mode-command)
   ;; ("C-c h" . consult-history)
   ;; ("C-c k" . consult-kmacro)
   ("C-c m" . consult-man)
   ("C-c i" . consult-info)
   ("C-x M-:" . consult-complex-command)
   ("C-x b" . consult-buffer)
   ;; ("C-x 4 b" . consult-buffer-other-window)
   ;; ("C-x 5 b" . consult-buffer-other-frame)
   ("C-x t b" . consult-buffer-other-tab)
   ("C-x r b" . consult-bookmark)
   ("C-x p b" . consult-project-buffer)
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flycheck)
   ("M-g g" . consult-goto-line)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ("M-s f" . consult-fd)
   ("M-s g" . consult-ripgrep)
   ("M-s G" . consult-git-grep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
   ("M-s e" . consult-isearch-history)
   ("M-y" . consult-yank-pop)
   ("M-#" . consult-register-load)
   ("M-'" . consult-register-store)
   ("C-M-#" . consult-register)
   :map isearch-mode-map
   ("M-s e" . consult-isearch-history)
   :map minibuffer-local-map
   ("M-r" . consult-history))
  :hook (completion-list-mode . consult-preview-at-point-mode))

(use-package consult-flycheck :ensure t :after consult)

(use-package consult-eglot :ensure t :after consult)

;; ~ Marginalia ----------------------------------------------------------- ~ ;;

(use-package marginalia
  :ensure t
  :after vertico
  :init (marginalia-mode)
  ;; :custom
  ;; (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  )

;; ~ Vertico -------------------------------------------------------------- ~ ;;

(use-package vertico
  :ensure t
  :demand t
  :hook (minibuffer-setup . cursor-intangible-mode)
  :init
  (fido-mode -1)
  (fido-vertical-mode -1)
  (icomplete-mode -1)
  (icomplete-vertical-mode -1)
  (vertico-mode 1)
  (vertico-multiform-mode 1)
  :custom
  (completion-cycle-threshold 1)
  (completions-detailed t)
  (vertico-cycle t)
  (vertico-count 10)
  (vertico-scroll-margin 0)
  (vertico-resize t)
  (enable-recursive-minibuffers t)
  (minibuffer-prompt-properties
   '(read-only t intangible t cursor-intangible t face
               minibuffer-prompt))
  (add-to-list 'vertico-multiform-categories '(jinx grid (vertico-grid-annotate . 20))))

;; ~ Cape ----------------------------------------------------------------- ~ ;;

(use-package cape
  :ensure t
  :after orderless
  :init
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  :config
  (setq corfu-separator 32)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (defun gracs/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super #'eglot-completion-at-point #'cape-file))))
  :hook (eglot-managed-mode . gracs/eglot-capf)
  :bind (("C-c c" . cape-prefix-map)))
;; ("C-c c p" . completion-at-point)
;; ("C-c c t" . complete-tag)
;; ("C-c c d" . cape-dabbrev)
;; ("C-c c f" . cape-file)
;; ("C-c c k" . cape-keyword)
;; ("C-c c l" . cape-line)
;; ("C-c c s" . cape-elisp-symbol)
;; ("C-c c e" . cape-elisp-block)
;; ("C-c c w" . cape-dict)
;; ("C-c c :" . cape-emoji)
;; ("C-c c _" . cape-tex)))
;; ("C-c c h" . cape-history)
;; ("C-c c a" . cape-abbrev)
;; ("C-c c &" . cape-sgml)
;; ("C-c c r" . cape-rfc1345)

;; ~ Corfu ---------------------------------------------------------------- ~ ;;

(use-package corfu
  :ensure t
  :after orderless
  :init
  (corfu-popupinfo-mode)
  (global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto nil)
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
        ("M-C-p" . corfu-popupinfo-toggle)
        ("M-C-f" . corfu-popupinfo-scroll-down)
        ("M-C-b" . corfu-popupinfo-scroll-up)))

;; ~ Embark --------------------------------------------------------------- ~ ;;

;; (use-package embark
;;              :ensure t
;;              :config
;;              (setq prefix-help-command #'embark-prefix-help-command)
;;              (add-to-list 'display-buffer-alist '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
;;                                                   nil
;;                                                   (window-parameters (mode-line-format . none))))
;;              :bind
;;              (("C-." . embark-act)
;;               ("C-h B" . embark-bindings)))

;; (use-package embark-consult
;;              :ensure t
;;              :after (embark consult)
;;              :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide 'mod-completion)
;;; mod-completion.el ends here
