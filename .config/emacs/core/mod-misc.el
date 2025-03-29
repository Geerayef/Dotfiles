;;; mod-misc.el --- Miscellaneous -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package zzz-to-char
  :ensure t
  :bind (("M-z" . zzz-to-char-up-to-char)))

(use-package avy
  :ensure t
  :bind (("C-c j" . avy-goto-char-timer)
         ("C-c s" . avy-isearch)))

(provide 'mod-misc)
;;; mod-misc.el ends here
