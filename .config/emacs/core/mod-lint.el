;;; mod-lint.el --- Flycheck -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; ~ Flycheck -------------------------------------------------------------- ~ ;;
(use-package flycheck-posframe
  :ensure t
  :hook ((flycheck-mode . flycheck-posframe-mode))
  :custom
  (flycheck-posframe-warning-prefix " ")
  (flycheck-posframe-error-prefix "× ")
  (flycheck-posframe-info-prefix " ")
  :config
  (defun flycheck-posframe-monitor-post-command ()
    (when (not (flycheck-posframe-check-position))
      (posframe-hide flycheck-posframe-buffer)))
  (set-face-attribute 'flycheck-posframe-info-face nil :inherit 'font-lock-variable-name-face)
  (set-face-attribute 'flycheck-posframe-warning-face nil :inherit 'warning)
  (set-face-attribute 'flycheck-posframe-error-face nil :inherit 'error))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :custom
  (flycheck-standard-error-navigation nil)
  (flycheck-check-syntax-automatically '(mode-enabled save idle-buffer-switch idle-change))
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-buffer-switch-check-intermediate-buffers t)
  (flycheck-idle-change-delay 1.0)
  (flycheck-display-errors-delay 0.25)
  (flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

(provide 'mod-lint)
;;; mod-lint.el ends here
