;;; mod-theme.el --- UI -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq custom-safe-themes t)

(defvar gracs/default-bg (face-attribute 'default :background))
(defvar gracs/color-yellow "#FFF779")
(defvar gracs/color-gray "#505050")
(defvar gracs/theme-fg "#EDE7DD")
(defvar gracs/theme-bg "#010204")
(defvar gracs/theme-sel-fg "#1B2229")
(defvar gracs/theme-sel-bg "#221010")

(cond ((string-search "doom" gracs/theme)
       (use-package doom-themes
         :ensure t
         :demand t
         :custom
         (doom-themes-enable-bold t)
         (doom-themes-enable-italic t)
         (doom-themes-org-config)))
      
      ((string-search "ef" gracs/theme)
       (use-package ef-themes
         :ensure t
         :demand t
         :custom
         (ef-elea-dark-palette-overrides `((bg-main ,gracs/theme-bg)))
         :config
         (mapc #'disable-theme custom-enabled-themes)))
      
      ((string-search "poet" gracs/theme)
       (use-package poet-theme
         :ensure t
         :demand t
         :config
         (custom-set-faces
          `(region ((t (:background ,gracs/theme-sel-bg))))
          `(highlight ((t (:background ,gracs/theme-sel-bg))))))))

(custom-set-faces
 ;; `(default ((t (:background ,gracs/theme-bg))))
 ;; `(fringe ((t (:background ,gracs/theme-bg))))
 `(cursor ((t (:background ,gracs/color-yellow))))
 `(show-paren-match ((t (:inherit 'default :background ,gracs/default-bg :foreground ,gracs/color-gray :weight bold))))
 `(line-number ((t (:slant normal))))
 `(line-number-current-line ((t (:foreground ,gracs/color-yellow :slant normal :weight bold)))))

(provide 'mod-theme)
;;; mod-theme.el ends here
