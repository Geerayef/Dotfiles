;;;; vertico.el --- Vertico -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package vertico
  :ensure t
  :config
  (fido-mode -1)
  (fido-vertical-mode -1)
  (icomplete-mode -1)
  (icomplete-vertical-mode -1)
  (vertico-mode 1)
  (setq vertico-cycle t
        vertico-count 10
        vertico-scroll-margin 0
        vertico-resize t))

(elpaca-wait)

;; (use-package emacs
;;   :init
;;   ;; Add prompt indicator to `completing-read-multiple'.
;;   ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
;;   (defun crm-indicator (args)
;;     (cons (format "[CRM%s] %s"
;;                   (replace-regexp-in-string
;;                    "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
;;                    crm-separator)
;;                   (car args))
;;           (cdr args)))
;;   (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
;;   ;; Do not allow the cursor in the minibuffer prompt
;;   (setq minibuffer-prompt-properties
;;         '(read-only t cursor-intangible t face minibuffer-prompt))
;;   (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
;;   ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
;;   ;; Vertico commands are hidden in normal buffers.
;;   ;; (setq read-extended-command-predicate
;;   ;;       #'command-completion-default-include-p)
;;   (setq enable-recursive-minibuffers t))

(provide 'vertico)
;;; vertico.el ends here
