;;;; mod-flycheck.el --- Lisp development configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save idle-buffer-switch))
  (setq flycheck-buffer-switch-check-intermediate-buffers t)
  (setq flycheck-display-errors-delay 0.25)
  ;; We change the double arrow to be a triangle because it looks cleaner.
  ;; (when (fboundp 'define-fringe-bitmap)
  ;;   (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
  ;;     [16 48 112 240 112 48 16] nil nil 'center))
  ;; Don't display flycheck errors in the minibuffer
  ;; (setq flycheck-display-errors-function 'ignore)
  ;; When we use the error list, we want to make sure shackle puts it somewhere
  ;; better.
  ;; (add-shackle-rule! '(flycheck-error-list-mode :noselect t :align 'below :size 7))
  ;; (add-winner-boring-buffer! "*Flycheck errors*"))
  )

(provide 'mod-flycheck)
;;; mod-flycheck.el ends here
