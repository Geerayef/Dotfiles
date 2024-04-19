;;;; mod-write.el --- General editing enhancements -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:



(use-package jinx
  :ensure t
  :hook ((text-mode prog-mode conf-mode) . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

(use-package zzz-to-char
  :ensure t
  :bind (("M-z" . zzz-to-char-up-to-char)))

(provide 'mod-write)
;;; mod-write.el ends here
