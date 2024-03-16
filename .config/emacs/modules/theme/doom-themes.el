;;;; doom-themes.el --- DoomEmacs themes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-ayu-dark-brighter-comments t
        doom-ayu-dark-comment-bg nil)
  (doom-themes-visual-bell-config)
  (load-theme 'doom-ayu-dark t)
  ;; (doom-themes-neotree-config)
  ;; (setq doom-themes-treemacs-theme "doom-atom")
  ;; (doom-themes-treemacs-config)
  (doom-themes-org-config))

(elpaca-wait)

(provide 'doom-themes)
;;; doom-themes.el ends here
