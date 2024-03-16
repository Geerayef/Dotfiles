;;;; marginalia.el --- Marginalia -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package marginalia
  :ensure t
  :config
  (setq marginalia-annotators
        '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(provide 'marginalia)
;;; marginalia.el ends here
