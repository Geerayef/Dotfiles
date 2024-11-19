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
           (doom-themes-org-config)))

  ('ef (use-package ef-themes
         :ensure t
         :demand t
         :custom
         (ef-elea-dark-palette-overrides '((bg-main "#010204")))
         :config
         (mapc #'disable-theme custom-enabled-themes))))

(custom-set-faces
 '(default ((t (:background "#010204"))))
 '(cursor ((t (:background "#FFF779"))))
 '(line-number ((t (:slant normal))))
 '(line-number-current-line ((t (:foreground "#FFF779" :slant normal :weight bold)))))

(provide 'mod-theme)
;;; mod-theme.el ends here
