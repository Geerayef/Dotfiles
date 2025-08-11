;;; mod-dirvish.el --- File system -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package dirvish
  :ensure t
  :init   (dirvish-override-dired-mode)
  :custom
  (dirvish-default-layout '(0 0.4))
  (dirvish-quick-access-entries '(("~" "~/" "Home")))
  (dirvish-nerd-icons-height 0.8)
  (dirvish-fd-switches "--hidden")
  (dirvish-peek-mode nil)
  :config
  (setq dirvish-use-mode-line 'global
        dirvish-mode-line-format '(:left (sort file-time " " file-size symlink) :right (omit yank index))
        dirvish-attributes '(vc-state subtree-state nerd-icons collapse git-msg file-time file-size)
        delete-by-moving-to-trash t
        dired-listing-switches "-l --all --group-directories-first")
  :bind (("C-x d" . dirvish)))

(provide 'mod-dirvish)
;;; mod-dirvish.el ends here
