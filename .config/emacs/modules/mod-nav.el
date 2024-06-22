;;;; mod-nav.el --- File system -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; (use-package zzz-to-char
;;   :ensure t
;;   :bind (("M-z" . zzz-to-char-up-to-char)))

;; (use-package dirvish
;;   :ensure t
;;   :init (dirvish-override-dired-mode)
;;   :custom
;;   (dirvish-default-layout '(0 0.4 0.6))
;;   (dirvish-quick-access-entries '(("h" "~/" "Home")))
;;   :config
;;   (setq dirvish-use-mode-line 'global
;;         dirvish-mode-line-format '(:left (sort symlink) :right (omit yank index))
;;         dirvish-attributes '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg)
;;         delete-by-moving-to-trash t
;;         dirvish-fd-switches "--hidden"
;;         dired-listing-switches "-l --all --group-directories-first")
;;   :bind (("C-x d" . dirvish)))

(provide 'mod-nav)
;;; mod-nav.el ends here
