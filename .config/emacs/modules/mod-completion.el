;;;; mod-completion.el --- Completion -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq tab-always-indent 'complete)

;; ~ Orderless ------------------------------------------------------------- ~ ;;

(use-package orderless
  :ensure t
  :demand t
  :config
  (orderless-define-completion-style orderless+initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))
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

;; ~ Consult --------------------------------------------------------------- ~ ;;

(use-package consult
  :ensure t
  :bind
  (("C-c M-x" . consult-mode-command)
   ("C-c h" . consult-history)
   ("C-c k" . consult-kmacro)
   ("C-c m" . consult-man)
   ("C-c i" . consult-info)
   ;;;; ~ C-x bindings in `ctl-x-map'
   ("C-x M-:" . consult-complex-command)
   ("C-x b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("C-x t b" . consult-buffer-other-tab)
   ("C-x r b" . consult-bookmark)
   ("C-x p b" . consult-project-buffer)
   ;;;; ~ M-g bindings in `goto-map'
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flycheck)
   ("M-g g" . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ;; Alternative: consult-org-heading
   ("M-g o" . consult-outline)
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ;;;; ~ M-s bindings in `search-map'
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
   ("M-y" . consult-yank-pop)
   ("M-#" . consult-register-load)
   ("M-'" . consult-register-store)
   ("C-M-#" . consult-register)
   ;;;; ~ Isearch integration
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)
   ("M-s e" . consult-isearch-history)
   ;;;; ~ Minibuffer history
   :map minibuffer-local-map
   ("M-s" . consult-history)
   ("M-r" . consult-history))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref
        completion-in-region-function #'consult-completion-in-region)
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

;; ~ Marginalia ------------------------------------------------------------ ~ ;;

(use-package marginalia
  :ensure t
  :after vertico
  :init
  (marginalia-mode)
  :config
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

;; ~ Vertico --------------------------------------------------------------- ~ ;;

(use-package vertico
  :ensure t
  :demand t
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
  (add-to-list 'vertico-multiform-categories '(jinx grid (vertico-grid-annotate . 20))))

;; ~ Cape ------------------------------------------------------------------ ~ ;;

;; (use-package cape
;;              :ensure t
;;              :init
;;              (add-to-list 'completion-at-point-functions #'cape-keyword)
;;              (add-to-list 'completion-at-point-functions #'cape-dabbrev)
;;              (add-to-list 'completion-at-point-functions #'cape-file)
;;              (add-to-list 'completion-at-point-functions #'cape-elisp-block)
;;              (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
;;              :config
;;              (defun my/eglot-capf ()
;;                (setq-local completion-at-point-functions
;;                            (list (cape-capf-super #'eglot-completion-at-point #'cape-file))))
;;              :hook (eglot-managed-mode . my/eglot-capf)
;;              :bind (("C-c p p" . completion-at-point)
;;                     ("C-c p t" . complete-tag)
;;                     ("C-c p d" . cape-dabbrev)
;;                     ("C-c p f" . cape-file)
;;                     ("C-c p k" . cape-keyword)
;;                     ("C-c p l" . cape-line)
;;                     ("C-c p s" . cape-elisp-symbol)
;;                     ("C-c p e" . cape-elisp-block)
;;                     ("C-c p w" . cape-dict)
;;                     ("C-c p :" . cape-emoji)
;;                     ("C-c p _" . cape-tex)))
;; ("C-c p \\" . cape-tex)
;; ("C-c p ^" . cape-tex)
;; ("C-c p h" . cape-history)
;; ("C-c p a" . cape-abbrev)
;; ("C-c p &" . cape-sgml)
;; ("C-c p r" . cape-rfc1345)

;; ~ Corfu ----------------------------------------------------------------- ~ ;;

;; (use-package corfu
;;              :ensure (corfu :files (:defaults "extensions/*") :includes (corfu-popupinfo))
;;              :init
;;              (global-corfu-mode)
;;              (corfu-popupinfo-mode)
;;              :custom
;;              (corfu-cycle t)
;;              (corfu-auto nil)
;;              (corfu-auto-prefix 2)
;;              (corfu-quit-no-match t)
;;              :config
;;              (eldoc-add-command #'corfu-insert)
;;              ;; (corfu-separator ?\s)
;;              ;; (corfu-quit-at-boundary nil)
;;              ;; (corfu-preview-current nil)
;;              ;; (corfu-preselect 'prompt)
;;              ;; (corfu-on-exact-match nil)
;;              ;; (corfu-scroll-margin 5)
;;              :bind
;;              (:map corfu-map
;;                    ("SPC" . corfu-insert-separator)
;;                    ("M-d" . corfu-popupinfo-toggle)
;;                    ("M-p" . corfu-popupinfo-scroll-down)
;;                    ("M-n" . corfu-popupinfo-scroll-up)))

;; ~ Embark ---------------------------------------------------------------- ~ ;;

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
