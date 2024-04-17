;;;; mod-vcs.el --- Version control systems -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package vc
  :ensure nil
  :custom
  (vc-follow-symlinks t)
  (vc-handled-backends '(Git)))

(use-package magit
  :ensure t)

(use-package git-gutter
  :ensure t
  :config (global-git-gutter-mode t))

(provide 'mod-vcs)
;;; mod-vcs.el ends here
