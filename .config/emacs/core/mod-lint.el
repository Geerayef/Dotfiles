;;; mod-lint.el --- Flycheck -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; ~ Flycheck -------------------------------------------------------------- ~ ;;

;; (use-package flycheck
;;   :ensure t
;;   :custom
;;   (flycheck-check-syntax-automatically '(mode-enabled save idle-buffer-switch))
;;   (flycheck-emacs-lisp-load-path 'inherit)
;;   (flycheck-buffer-switch-check-intermediate-buffers t)
;;   (flycheck-idle-change-delay 1.0)
;;   (flycheck-display-errors-delay 0.25)
;;   (flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
;;   :hook ((prog-mode-hook text-mode-hook) . flycheck-mode))

;; ~ Flymake -------------------------------------------------------------- ~ ;;



(provide 'mod-lint)
;;; mod-lint.el ends here
