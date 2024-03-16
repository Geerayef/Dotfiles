;;;; mod-theme.el --- Themes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq custom-safe-themes t)

(require 'doom-themes)
(require 'ef-themes)

(defcustom default/theme 'ef-deuteranopia-dark
  "My prefered default theme.")
(add-hook 'elpaca-after-init-hook (lambda () (load-theme default/theme)))

(provide 'mod-theme)
;;; mod-theme.el ends here
