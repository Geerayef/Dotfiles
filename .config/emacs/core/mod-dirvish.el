;;; mod-dirvish.el --- File system -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package dirvish
  :ensure t
  :init (dirvish-override-dired-mode)
  :config
  (setq dirvish-use-mode-line 'global
        dirvish-mode-line-format '(:left (sort file-time " " file-size symlink) :right (omit yank index))
        dirvish-attributes '(vc-state subtree-state nerd-icons collapse git-msg file-time file-size)
        delete-by-moving-to-trash t
        dired-listing-switches "-l --all --dired --group-directories-first --color=never --human-readable")
  :custom
  (dirvish-default-layout '(0 0.4))
  (dirvish-quick-access-entries '(("~" "~/" "Home")))
  (dirvish-nerd-icons-height 0.8)
  (dirvish-fd-switches "--hidden")
  (dirvish-peek-mode nil)
  :bind
  (("C-x d" . dirvish-dwim)
   :map dirvish-mode-map
   ("?" . dirvish-dispatch)
   ("v" . dirvish-vc-menu)
   ("*" . dirvish-mark-menu)
   ("y" . dirvish-yank-menu)
   ("TAB" . dirvish-subtree-toggle)))

(provide 'mod-dirvish)
;;; mod-dirvish.el ends here
