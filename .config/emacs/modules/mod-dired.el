;;;; mod-dired.el --- File system -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package dirvish
  :ensure t
  :init (dirvish-override-dired-mode)
  :custom (dirvish-quick-access-entries '(("h" "~/" "Home")))
  :config
  (setq dirvish-use-mode-line 'global
		dirvish-mode-line-format '(:left (sort symlink) :right (omit yank index))
		dirvish-attributes '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg)
		delete-by-moving-to-trash t
		;; dired-listing-switches "--long --almost-all --human-readable --group-directories-first"
		))

(provide 'mod-dired)
;;; mod-dired.el ends here
