;;; mod-theme.el --- Theme -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq custom-safe-themes t)

(defvar gracs/road-palette-mint-cream "#D9E4DB")
(defvar gracs/road-palette-jet "#373643")
(defvar gracs/road-palette-ebony "#565848")
(defvar gracs/road-palette-cadet-gray "#86909A")
(defvar gracs/road-palette-raisin-black "#2B2A30")
(defvar gracs/road-palette-citron "#D7CC75")
(defvar gracs/road-palette-paynes-gray "#5E686E")
(defvar gracs/road-palette-charcoal "#4A515C")
(defvar gracs/road-palette-dragon-ink "#010204")
(defvar gracs/road-palette-emerald "#69DC9E")
(defvar gracs/road-palette-lotus-yellow "#FFF779")
(defvar gracs/road-palette-rusty-red "#DA2F43")
(defvar gracs/road-palette-gun-metal "#176087")
(defvar gracs/theme-fg "#EDE7DD")
(defvar gracs/theme-bg gracs/road-palette-dragon-ink)

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
         :demand t)))

(custom-set-faces
 `(default ((t (:background ,gracs/theme-bg))))
 `(fringe ((t (:background ,gracs/theme-bg))))
 `(region ((t (:background ,gracs/road-palette-jet))))
 `(highlight ((t (:background ,gracs/road-palette-raisin-black))))
 `(cursor ((t (:foreground ,gracs/road-palette-dragon-ink :background ,gracs/road-palette-lotus-yellow))))
 `(show-paren-match ((t (:inherit 'default :foreground ,gracs/road-palette-citron :background ,gracs/road-palette-paynes-gray))))
 `(line-number ((t (:slant normal))))
 `(line-number-current-line ((t (:foreground ,gracs/road-palette-lotus-yellow :slant normal :weight bold))))
 `(mode-line ((t (:background ,gracs/theme-bg)))))

(provide 'mod-theme)
;;; mod-theme.el ends here
