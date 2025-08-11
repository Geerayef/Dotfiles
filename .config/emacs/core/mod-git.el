;;;; mod-git.el --- Version control systems -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package vc
  :ensure nil
  :custom
  (vc-follow-symlinks t)
  (vc-handled-backends '(Git)))

(use-package transient
  :ensure t)

(use-package magit
  :ensure t
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  (diff-hl-dired-mode))

(provide 'mod-git)
;;; mod-git.el ends here
