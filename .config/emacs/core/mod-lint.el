;;; mod-lint.el --- Linting -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; ~ Flymake -------------------------------------------------------------- ~ ;;

(use-package flymake
  :ensure t
  :hook ((prog-mode eglot-managed-mode) . flymake-mode)
  :custom
  (flymake-no-changes-timeout 0.5)
  (flymake-start-on-save-buffer t)
  (flymake-indicator-type 'margins))

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

(provide 'mod-lint)
;;; mod-lint.el ends here
