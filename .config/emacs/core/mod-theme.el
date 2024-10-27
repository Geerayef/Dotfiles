;;; mod-theme.el --- UI -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq custom-safe-themes t)

(pcase gracs/theme
  ('doom (use-package doom-themes
           :ensure t
           :demand t
           :custom
           (doom-themes-enable-bold t)
           (doom-themes-enable-italic t)
           (doom-themes-org-config)
           :config
           (custom-set-faces 
            '(default ((t (:background "#010202"))))
            '(cursor ((t (:background "#FFF779")))))))

  ('ef (use-package ef-themes
         :ensure t
         :demand t
         :custom
         (ef-elea-dark-palette-overrides '((cursor "#FFF779") (bg-main "#010203")))
         :config
         (mapc #'disable-theme custom-enabled-themes))))

(provide 'mod-theme)
;;; mod-theme.el ends here
