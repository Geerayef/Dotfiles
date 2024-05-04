;;;; mod-flycheck.el --- Flycheck -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :custom
  (flycheck-check-syntax-automatically '(mode-enabled save idle-buffer-switch))
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-buffer-switch-check-intermediate-buffers t)
  (flycheck-idle-change-delay 1.0)
  (flycheck-display-errors-delay 0.25)
  (flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

(use-package flycheck-package
  :ensure t
  :after (flycheck package-lint)
  :init (flycheck-package-setup))

(provide 'mod-flycheck)
;;; mod-flycheck.el ends here
