;;; mod-lint.el --- Flycheck -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; ~ Flymake -------------------------------------------------------------- ~ ;;

(use-package flymake
  :ensure t
  :hook (eglot-managed-mode . flymake-mode)
  :custom
  (flymake-no-changes-timeout 0.5)
  (flymake-start-on-save-buffer t)
  (flymake-indicator-type 'margins)
  (flymake-margin-indicators-string
   `((error "»" compilation-error)
     (warning "»" compilation-warning)
     (note "»" compilation-info)))
  :bind
  (("C-c d s" . flymake-show-buffer-diagnostics)
   ("C-c [ d" . flymake-goto-prev-error)
   ("C-c ] d" . flymake-goto-next-error)))

;; Flyover ---------------------------------------------------------------- ~ ;;

(use-package flyover
  :ensure (:host github :repo "konrad1977/flyover")
  :hook (flymake-mode . flyover-mode)
  :custom
  (flyover-checkers '(flymake))
  (flyover-debug nil)
  (flyover-debounce-interval 0.2)
  (flyover-levels '(error warning info))
  (flyover-line-position-offset 0)
  (flyover-wrap-messages nil)
  (flyover-max-line-length 80)
  (flyover-hide-checker-name nil)
  (flyover-show-at-eol t)
  (flyover-hide-when-cursor-is-on-same-line t)
  (flyover-show-virtual-line t)
  (flyover-use-theme-colors t)
  (flyover-background-lightness 45)
  (flyover-percent-darker 40)
  (flyover-icon-enable t)
  (flyover-info-icon " ")
  (flyover-warning-icon " ")
  (flyover-error-icon "× ")
  (flyover-icon-left-padding 0.9)
  (flyover-icon-right-padding 0.9)
  (flyover-virtual-line-type 'line-no-arrow)
  (flyover-virtual-line-icon nil)
  (flyover-text-tint 'darker)
  (flyover-text-tint-percent 50))

;; ~ Flycheck ------------------------------------------------------------- ~ ;;

;; (use-package flycheck-posframe
;;   :ensure t
;;   :hook ((flycheck-mode . flycheck-posframe-mode))
;;   :custom
;;   (flycheck-posframe-warning-prefix " ")
;;   (flycheck-posframe-error-prefix "× ")
;;   (flycheck-posframe-info-prefix " ")
;;   :config
;;   (defun flycheck-posframe-monitor-post-command ()
;;     (when (not (flycheck-posframe-check-position))
;;       (posframe-hide flycheck-posframe-buffer)))
;;   (set-face-attribute 'flycheck-posframe-info-face nil :inherit 'font-lock-variable-name-face)
;;   (set-face-attribute 'flycheck-posframe-warning-face nil :inherit 'warning)
;;   (set-face-attribute 'flycheck-posframe-error-face nil :inherit 'error))

;; (use-package flycheck
;;   :ensure t
;;   :init (global-flycheck-mode)
;;   :custom
;;   (flycheck-standard-error-navigation nil)
;;   (flycheck-check-syntax-automatically '(mode-enabled save idle-buffer-switch idle-change))
;;   (flycheck-emacs-lisp-load-path 'inherit)
;;   (flycheck-buffer-switch-check-intermediate-buffers t)
;;   (flycheck-idle-change-delay 1.0)
;;   (flycheck-display-errors-delay 0.25)
;;   (flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

(provide 'mod-lint)
;;; mod-lint.el ends here
