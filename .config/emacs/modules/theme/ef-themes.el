;;;; ef-themes.el --- EF Themes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package ef-themes
  :ensure t
  :config
  (mapc #'disable-theme custom-enabled-themes))

(provide 'ef-themes)
;;; ef-themes.el ends here
