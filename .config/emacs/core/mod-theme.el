;;; mod-theme.el --- UI -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq custom-safe-themes t)

(use-package ef-themes
  :ensure t
  :demand t
  :config
  (setq ef-elea-dark-palette-overrides '((cursor "#FFF779") (bg-main "#010202")))
  (mapc #'disable-theme custom-enabled-themes))

(use-package doom-themes
  :ensure t
  :demand t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  (doom-themes-padded-modeline 8)
  (doom-themes-org-config)
  :config
  (custom-set-faces 
   '(default ((t (:background "#010202"))))
   '(mode-line ((t (:background "#010202"))))
   '(mode-line-active ((t (:background "#010202"))))
   '(cursor ((t (:background "yellow"))))))

(provide 'mod-theme)
;;; mod-theme.el ends here
