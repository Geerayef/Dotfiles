;;;; mod-all-the-icons.el --- All the icons -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Dependency
(use-package memoize)

(use-package all-the-icons
  :after memoize
  :if window-system
  :custom
  (all-the-icons-scale-factor 1.0)
  (all-the-icons-default-adjust -0.2))

(elpaca-wait)

(provide 'mod-all-the-icons)
;;; mod-all-the-icons.el ends here
