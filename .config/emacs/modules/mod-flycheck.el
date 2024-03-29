;;;; mod-flycheck.el --- Lisp development configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package flycheck
  :ensure t
  :custom
  (flycheck-check-syntax-automatically '(mode-enabled save idle-buffer-switch))
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-buffer-switch-check-intermediate-buffers t)
  (flycheck-idle-change-delay 1.0)
  (flycheck-display-errors-delay 0.25)
  (flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  :config
  ;; Change double arrow to triangle.
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))
  :init
  (global-flycheck-mode))

(use-package flycheck-package
  :ensure t
  :after (flycheck package-lint)
  :init
  (flycheck-package-setup))

(provide 'mod-flycheck)
;;; mod-flycheck.el ends here
