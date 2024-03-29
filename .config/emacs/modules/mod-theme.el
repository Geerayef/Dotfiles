;;;; mod-theme.el --- Themes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq custom-safe-themes t)

;; (use-package doom-themes
;;   :config
;;   (setq doom-themes-enable-bold t
;;         doom-themes-enable-italic t
;;         doom-ayu-dark-brighter-comments t
;;         doom-ayu-dark-comment-bg nil)
;;   (doom-themes-visual-bell-config)
;;   (doom-themes-org-config)
;;   :init
;;   (load-theme 'doom-ayu-dark t))

(use-package ef-themes
  :ensure t
  :demand t
  :config
  ;; (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'ef-dark t))

(provide 'mod-theme)
;;; mod-theme.el ends here
