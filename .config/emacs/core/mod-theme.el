;;; mod-theme.el --- UI -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq custom-safe-themes t)

(use-package ef-themes
  :ensure t
  :demand t
  :config
  (setq ef-elea-dark-palette-overrides '((cursor "#FFF779") (bg-main "#010202")))
  (mapc #'disable-theme custom-enabled-themes)
  ;; (load-theme 'ef-elea-dark)
  )

; 'doom-plain-dark
; 'doom-tomorrow-night
; 'doom-sourcerer
; 'doom-pine
; 'doom-spacegrey
(use-package doom-themes
  :ensure t
  :demand t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-themes-padded-modeline 8)
  (load-theme 'doom-spacegrey t)
  (doom-themes-org-config)
  (custom-set-faces
   '(default ((t (:background "#010202"))))
   '(mode-line ((t (:background "#010202"))))
   '(cursor ((t (:background "yellow"))))))

(provide 'mod-theme)
;;; mod-theme.el ends here
